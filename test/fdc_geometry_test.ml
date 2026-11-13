(* Where a track, side and sector land in a .dsk image. The image is plain
   sector data in logical order; the WD2793 model used to count every image
   as two sides of 9 sectors, so a 360KB one-sided disk or an 8-sector disk
   read the wrong sector from track 1 on. The rules follow openMSX
   SectorBasedDisk::detectGeometry, Disk::detectGeometry and
   Disk::physToLog. Every sector carries its own logical number at
   [mark], so a read tells which sector it got. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let sector_bytes = 512
let mark = 0x100

let window = 0x7ff8
let rd t k = Msx.mem_read t (window + k)
let wr t k v = Msx.mem_write t (window + k) v
let drq t = rd t 7 land 0x80 = 0

let read_sector = 0x80
let write_sector = 0xa0

(* [sectors] sectors, each with its number at [mark]. [jmp], [bpb]
   (sectors per track, sides) and [media] set the bytes geometry detection
   reads. *)
let image ~sectors ?(jmp = false) ?(jmp_byte = 0xeb) ?bpb ?media () =
  let d = Bytes.make (sectors * sector_bytes) '\000' in
  for n = 0 to sectors - 1 do
    Bytes.set d ((n * sector_bytes) + mark) (Char.chr (n land 255));
    Bytes.set d ((n * sector_bytes) + mark + 1) (Char.chr (n lsr 8))
  done;
  if jmp then Bytes.set d 0 (Char.chr jmp_byte);
  (match bpb with
   | Some (spt, sides) ->
     Bytes.set d 0x18 (Char.chr (spt land 255));
     Bytes.set d 0x19 (Char.chr (spt lsr 8));
     Bytes.set d 0x1a (Char.chr (sides land 255));
     Bytes.set d 0x1b (Char.chr (sides lsr 8))
   | None -> ());
  (match media with
   | Some m -> Bytes.set d sector_bytes (Char.chr m)
   | None -> ());
  Bytes.to_string d

let machine disk =
  let rom = Bytes.make 32768 '\000' in
  Bytes.set rom 0 (Char.chr 0x18);
  let t =
    Msx.create
      ~machine:{ ram_kb = 512; vram_kb = 128; roms = [ Bytes.to_string rom; ""; "" ] }
  in
  let disk_rom =
    match Msx.disk_rom_of_string (String.make 0x4000 '\000') with
    | Ok r -> r
    | Error e -> failwith e
  in
  Msx.load_disk ~disk_rom t disk;
  Msx.port_out t 0xA8 0xCC;
  Msx.mem_write t 0xffff 0x8C;
  wr t 5 0x80;
  t

(* The logical number READ SECTOR finds at (track, side, sector), or None
   when the command raises no DRQ. *)
let found t ~track ~side ~sector =
  wr t 1 track;
  wr t 4 side;
  wr t 2 sector;
  wr t 0 read_sector;
  if not (drq t) then None
  else begin
    let b = Array.init sector_bytes (fun _ -> rd t 3) in
    Some (b.(mark) lor (b.(mark + 1) lsl 8))
  end

let at t ~track ~side ~sector n =
  let got = found t ~track ~side ~sector in
  if got <> n then
    Printf.eprintf "  t%d s%d #%d: got %s\n%!" track side sector
      (match got with Some g -> string_of_int g | None -> "none");
  got = n

let () =
  (* 720KB: two sides of 9 whatever the boot sector says (openMSX keeps
     this for images whose BPB or media byte claims one side). *)
  let t = machine (image ~sectors:1440 ~jmp:true ~bpb:(9, 1) ~media:0xf8 ()) in
  check "720KB, BPB says one side: still two sides"
    (at t ~track:1 ~side:0 ~sector:1 (Some 18) && at t ~track:1 ~side:1 ~sector:1 (Some 27));

  (* 360KB with a BPB: one side of 9. *)
  let t = machine (image ~sectors:720 ~jmp:true ~bpb:(9, 1) ()) in
  check "360KB BPB one side: track 1 sector 1 is logical 9"
    (at t ~track:1 ~side:0 ~sector:1 (Some 9));
  check "360KB BPB one side: track 79 sector 9 is the last"
    (at t ~track:79 ~side:0 ~sector:9 (Some 719));
  check "360KB: track 80 is past the end" (at t ~track:80 ~side:0 ~sector:1 None);
  check "one side: side 1 counts on into the next track (openMSX physToLog)"
    (at t ~track:0 ~side:1 ~sector:1 (Some 9));

  (* No JMP: the FAT media byte decides. *)
  let t = machine (image ~sectors:720 ~media:0xf8 ()) in
  check "media 0xF8: one side of 9" (at t ~track:2 ~side:0 ~sector:3 (Some 20));
  let t = machine (image ~sectors:640 ~media:0xfa ()) in
  check "media 0xFA: one side of 8" (at t ~track:1 ~side:0 ~sector:1 (Some 8));
  check "8 sectors per track: no sector 9" (at t ~track:1 ~side:0 ~sector:9 None);
  let t = machine (image ~sectors:1280 ~media:0xfb ()) in
  check "media 0xFB: two sides of 8" (at t ~track:1 ~side:1 ~sector:1 (Some 24));
  let t = machine (image ~sectors:1280 ~jmp:true ~bpb:(8, 2) ~media:0xf8 ()) in
  check "a JMP boot sector wins over the media byte"
    (at t ~track:1 ~side:1 ~sector:1 (Some 24));

  (* Neither: 9 sectors, one side for 360KB and two for other sizes. *)
  let t = machine (image ~sectors:720 ~media:0x00 ()) in
  check "360KB, no JMP, no media byte: one side" (at t ~track:1 ~side:0 ~sector:1 (Some 9));
  let t = machine (image ~sectors:720 ~jmp:true ~bpb:(0, 1) ()) in
  check "BPB with 0 sectors per track: size decides" (at t ~track:1 ~side:0 ~sector:1 (Some 9));
  let t = machine (image ~sectors:720 ~jmp:true ~bpb:(9, 256) ()) in
  check "BPB with 256 sides: size decides" (at t ~track:1 ~side:0 ~sector:1 (Some 9));
  let t = machine (image ~sectors:720 ~jmp:true ~jmp_byte:0xe9 ~bpb:(8, 2) ()) in
  check "a 0xE9 JMP also marks a BPB" (at t ~track:1 ~side:0 ~sector:1 (Some 16));
  (* The BPB fields are 16-bit: 265 sectors per track is out of range even
     though its low byte is 9. 1000 sectors falls back to two sides. *)
  let t = machine (image ~sectors:1000 ~jmp:true ~bpb:(265, 1) ()) in
  check "BPB fields are 16-bit" (at t ~track:1 ~side:0 ~sector:1 (Some 18));

  (* A track cut short by the end of the image has no sectors (openMSX
     empties the whole track when one sector is missing). 1000 sectors,
     two sides of 9: track 55 side 1 is logical 999-1007. *)
  let t = machine (image ~sectors:1000 ()) in
  check "last complete track reads" (at t ~track:55 ~side:0 ~sector:9 (Some 998));
  check "track cut short: its first sector is gone too"
    (at t ~track:55 ~side:1 ~sector:1 None);

  (* WRITE SECTOR goes through the same lookup. *)
  let t = machine (image ~sectors:720 ~jmp:true ~bpb:(9, 1) ()) in
  wr t 1 3;
  wr t 4 0;
  wr t 2 4;
  wr t 0 write_sector;
  for i = 0 to sector_bytes - 1 do wr t 3 (if i = mark then 0x77 else 0) done;
  let img = match Msx.disk_image t with Some s -> s | None -> "" in
  check "360KB write lands on logical 30"
    (Char.code img.[(30 * sector_bytes) + mark] = 0x77
     && Char.code img.[(57 * sector_bytes) + mark] = 57);

  if !failures > 0 then begin
    Printf.eprintf "fdc_geometry_test: %d failure(s)\n" !failures;
    exit 1
  end;
  print_endline "fdc_geometry_test: all passed"
