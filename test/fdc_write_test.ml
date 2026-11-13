(* WD2793 WRITE SECTOR through the NMS8250 register window (0x7FF8-0x7FFF in
   slot 3-3), the path the real disk ROM takes. Before this the model routed
   0xA0/0xB0 to READ SECTOR and dropped every byte written to the data
   register, so a game save on a real-ROM boot left the image untouched with
   no error. Expected behaviour follows openMSX WD2793.cc: one byte per DRQ,
   the sector reaches the disk after its last byte, then BUSY and DRQ drop
   and INTRQ rises (endCmd); with the multi-sector flag the sector register
   steps on until the track has no such sector. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let sector_bytes = 512
let sectors_per_track = 9
let disk_sectors = 1440

(* Register window: 0 status/command, 1 track, 2 sector, 3 data, 4 side,
   5 drive/motor, 7 DRQ (bit 7 low) and INTRQ (bit 6 low). *)
let window = 0x7ff8
let rd t k = Msx.mem_read t (window + k)
let wr t k v = Msx.mem_write t (window + k) v
let drq t = rd t 7 land 0x80 = 0
let intrq t = rd t 7 land 0x40 = 0

let write_sector = 0xa0
let write_multi = 0xb0
let read_sector = 0x80

(* Every byte of the image is [i land 255], so a write shows against it. *)
let blank () =
  Bytes.init (disk_sectors * sector_bytes) (fun i -> Char.chr (i land 255))

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
  Msx.load_disk ~disk_rom t (Bytes.to_string disk);
  (* Page 1 and page 3 on slot 3; slot 3 page 1 on sub-slot 3 (the disk
     ROM), page 3 on sub-slot 2 (RAM), as the NMS8250 is wired. *)
  Msx.port_out t 0xA8 0xCC;
  Msx.mem_write t 0xffff 0x8C;
  wr t 5 0x80;
  t

let logical ~track ~side ~sector =
  (((track * 2) + side) * sectors_per_track) + (sector - 1)

let image t =
  match Msx.disk_image t with Some s -> s | None -> failwith "no disk"

let sector_of t n = String.sub (image t) (n * sector_bytes) sector_bytes

let original n =
  String.init sector_bytes (fun i -> Char.chr (((n * sector_bytes) + i) land 255))

let pattern seed = String.init sector_bytes (fun i -> Char.chr (((i * 7) + seed) land 255))

let seek t ~track ~side ~sector =
  wr t 1 track;
  wr t 4 side;
  wr t 2 sector

let send t s ~from ~upto =
  for i = from to upto - 1 do
    wr t 3 (Char.code s.[i])
  done

let () =
  (* 1. One sector on track 2, side 1, sector 5. *)
  let t = machine (blank ()) in
  let n = logical ~track:2 ~side:1 ~sector:5 in
  let data = pattern 3 in
  seek t ~track:2 ~side:1 ~sector:5;
  wr t 0 write_sector;
  check "write: DRQ up once the command is taken" (drq t);
  check "write: status shows BUSY and DRQ" (rd t 0 land 0x03 = 0x03);
  send t data ~from:0 ~upto:(sector_bytes - 1);
  check "write: a partial sector is not on the disk yet" (sector_of t n = original n);
  check "write: DRQ still up before the last byte" (drq t && not (intrq t));
  send t data ~from:(sector_bytes - 1) ~upto:sector_bytes;
  check "write: the full sector is on the disk" (sector_of t n = data);
  check "write: neighbours untouched"
    (sector_of t (n - 1) = original (n - 1) && sector_of t (n + 1) = original (n + 1));
  check "write: end of command drops DRQ and raises INTRQ" ((not (drq t)) && intrq t);
  check "write: status idle, not write-protected" (rd t 0 land 0x43 = 0);

  (* 2. READ SECTOR gives the written bytes back. *)
  wr t 0 read_sector;
  let back = String.init sector_bytes (fun _ -> Char.chr (rd t 3)) in
  check "read back the written sector" (back = data);

  (* 3. Reading the data register during a write returns the last byte
     sent and does not move the write position. *)
  let t = machine (blank ()) in
  let n = logical ~track:0 ~side:0 ~sector:1 in
  seek t ~track:0 ~side:0 ~sector:1;
  wr t 0 write_sector;
  send t data ~from:0 ~upto:10;
  check "data register read mid-write gives the last byte" (rd t 3 = Char.code data.[9]);
  send t data ~from:10 ~upto:sector_bytes;
  check "a data register read does not shift the sector" (sector_of t n = data);

  (* 4. Multi-sector: sectors 8 and 9 of track 0 side 0, then sector 10
     does not exist on a 9-sector track and the command ends. Track 0
     side 1 sector 1 must not take the third sector's bytes. *)
  let t = machine (blank ()) in
  let s8 = logical ~track:0 ~side:0 ~sector:8 in
  let next_side = logical ~track:0 ~side:1 ~sector:1 in
  seek t ~track:0 ~side:0 ~sector:8;
  wr t 0 write_multi;
  let d8 = pattern 11 and d9 = pattern 29 in
  send t d8 ~from:0 ~upto:sector_bytes;
  check "multi: first sector written, DRQ up for the next" (sector_of t s8 = d8 && drq t);
  check "multi: sector register stepped" (rd t 2 = 9);
  send t d9 ~from:0 ~upto:sector_bytes;
  check "multi: second sector written" (sector_of t (s8 + 1) = d9);
  check "multi: no sector 10, command ends" ((not (drq t)) && intrq t && rd t 2 = 10);
  wr t 3 0x55;
  check "multi: the next side is untouched" (sector_of t next_side = original next_side);

  (* 5. Sector 0 does not exist: WRITE ends at once without writing, and
     READ no longer raises (offset -512 used to reach Bytes.blit). *)
  let t = machine (blank ()) in
  let before = image t in
  seek t ~track:0 ~side:0 ~sector:0;
  wr t 0 write_sector;
  check "sector 0 write: command ends" ((not (drq t)) && intrq t);
  wr t 3 0xAA;
  check "sector 0 write: image unchanged" (image t = before);
  (match wr t 0 read_sector with
   | () -> check "sector 0 read: no DRQ" (not (drq t))
   | exception Invalid_argument _ -> check "sector 0 read raised" false);
  seek t ~track:0 ~side:0 ~sector:10;
  wr t 0 read_sector;
  check "sector 10 read: no DRQ, no wrap onto the other side" (not (drq t));

  (* 6. A save taken mid-write resumes: the command, the bytes so far and
     the position travel in the existing FDC state. *)
  let t = machine (blank ()) in
  let n = logical ~track:4 ~side:0 ~sector:2 in
  seek t ~track:4 ~side:0 ~sector:2;
  wr t 0 write_sector;
  send t data ~from:0 ~upto:100;
  let r =
    match Msx.restore ~state:(Msx.serialize t) with
    | Ok r -> r
    | Error e -> failwith e
  in
  send r data ~from:100 ~upto:sector_bytes;
  check "restored mid-write finishes the sector" (sector_of r n = data);
  check "the saved machine is not affected" (sector_of t n = original n);

  if !failures > 0 then begin
    Printf.eprintf "fdc_write_test: %d failure(s)\n" !failures;
    exit 1
  end;
  print_endline "fdc_write_test: all passed"
