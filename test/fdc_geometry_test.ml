(* Where a track, side and sector land in a .dsk image. The image is plain
   sector data in logical order; the drive must lay it out the way the
   disk's own boot sector and FAT describe it, or a 360KB one-sided disk
   or an 8-sector disk reads the wrong sector from track 1 on. The rules
   follow openMSX SectorBasedDisk::detectGeometry, Disk::detectGeometry
   and Disk::physToLog. Every sector carries its own logical number at
   [mark], so a read tells which sector it got. The controller runs on its
   own clock: a SEEK steps 6 ms a track, the sector comes round under the
   head and each byte is read at its own DRQ. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let sector_bytes = 512
let mark = 0x100

(* Window registers (0x7FF8 + r) and their bits. *)
let status = 0 and track = 1 and sector = 2 and data = 3 and side = 4 and drive = 5
and lines = 7
let busy = 0x01 and record_not_found = 0x10
let motor_on_a = 0x80
let force_interrupt = 0xd0 and seek = 0x10 and read_sector = 0x80 and write_sector = 0xa0

(* openMSX clocks in master ticks: a data byte, one turn of the disk. *)
let byte_ticks = 109964
let turn = 200_000 * 3436

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
  d

(* First time from [from] in [step] increments, at most [limit] polls, at
   which [cond] holds. *)
let poll ~from ~step ~limit cond =
  let rec go t n = if n >= limit then None else if cond t then Some t else go (t + step) (n + 1) in
  go from 0

let drq fdc ~disk ~time = Wd2793.read fdc ~disk ~time lines land 0x80 = 0
let idle fdc ~disk ~time = Wd2793.read fdc ~disk ~time status land busy = 0

(* A controller with the power-on RESTORE stopped, the motor on and the
   head moved to [track]. Returns the controller and the time the seek
   ends. *)
let at_track disk ~trk =
  let fdc = Wd2793.create () in
  Wd2793.write fdc ~disk ~time:0 status force_interrupt;
  Wd2793.write fdc ~disk ~time:0 track 0;
  Wd2793.write fdc ~disk ~time:0 drive motor_on_a;
  Wd2793.write fdc ~disk ~time:0 data trk;
  Wd2793.write fdc ~disk ~time:0 status seek;
  match poll ~from:0 ~step:byte_ticks ~limit:1_000_000 (fun time -> idle fdc ~disk ~time) with
  | Some t -> (fdc, t)
  | None -> failwith "seek never ended"

(* The logical number READ SECTOR finds at (track, side, sector), or None
   when the command ends without data (record not found). *)
let found disk ~trk ~sd ~sec =
  let fdc, t0 = at_track disk ~trk in
  Wd2793.write fdc ~disk ~time:t0 side sd;
  Wd2793.write fdc ~disk ~time:t0 sector sec;
  Wd2793.write fdc ~disk ~time:t0 status read_sector;
  let step = byte_ticks / 2 in
  let limit = ((6 * turn) / step) + 1 in
  match
    poll ~from:t0 ~step ~limit (fun time -> drq fdc ~disk ~time || idle fdc ~disk ~time)
  with
  | Some t when drq fdc ~disk ~time:t ->
    let b = Array.init sector_bytes (fun k -> Wd2793.read fdc ~disk ~time:(t + (k * byte_ticks)) data) in
    Some (b.(mark) lor (b.(mark + 1) lsl 8))
  | Some t ->
    if Wd2793.read fdc ~disk ~time:t status land record_not_found = 0 then
      failwith "read ended without data or record not found";
    None
  | None -> failwith "read never ended"

let at disk ~trk ~sd ~sec n =
  let got = found disk ~trk ~sd ~sec in
  if got <> n then
    Printf.eprintf "  t%d s%d #%d: got %s\n%!" trk sd sec
      (match got with Some g -> string_of_int g | None -> "none");
  got = n

(* WRITE SECTOR at (track, side, sector): the sector's [mark] byte becomes
   [v], every other byte 0. *)
let write disk ~trk ~sd ~sec v =
  let fdc, t0 = at_track disk ~trk in
  Wd2793.write fdc ~disk ~time:t0 side sd;
  Wd2793.write fdc ~disk ~time:t0 sector sec;
  Wd2793.write fdc ~disk ~time:t0 status write_sector;
  let rec feed time k polls =
    if k >= sector_bytes then time
    else if polls > 1_000_000 then failwith "write never took its bytes"
    else if drq fdc ~disk ~time then begin
      Wd2793.write fdc ~disk ~time data (if k = mark then v else 0);
      feed time (k + 1) polls
    end
    else feed (time + (byte_ticks / 8)) k (polls + 1)
  in
  let t = feed t0 0 0 in
  match poll ~from:t ~step:byte_ticks ~limit:100 (fun time -> idle fdc ~disk ~time) with
  | Some _ -> ()
  | None -> failwith "write never ended"

let () =
  (* 720KB: two sides of 9 whatever the boot sector says (openMSX keeps
     this for images whose BPB or media byte claims one side). *)
  let d = image ~sectors:1440 ~jmp:true ~bpb:(9, 1) ~media:0xf8 () in
  check "720KB, BPB says one side: still two sides"
    (at d ~trk:1 ~sd:0 ~sec:1 (Some 18) && at d ~trk:1 ~sd:1 ~sec:1 (Some 27));

  (* 360KB with a BPB: one side of 9. *)
  let d = image ~sectors:720 ~jmp:true ~bpb:(9, 1) () in
  check "360KB BPB one side: track 1 sector 1 is logical 9" (at d ~trk:1 ~sd:0 ~sec:1 (Some 9));
  check "360KB BPB one side: track 79 sector 9 is the last" (at d ~trk:79 ~sd:0 ~sec:9 (Some 719));
  check "360KB: track 80 is past the end" (at d ~trk:80 ~sd:0 ~sec:1 None);
  check "one side: side 1 counts on into the next track (openMSX physToLog)"
    (at d ~trk:0 ~sd:1 ~sec:1 (Some 9));

  (* No JMP: the FAT media byte decides. *)
  let d = image ~sectors:720 ~media:0xf8 () in
  check "media 0xF8: one side of 9" (at d ~trk:2 ~sd:0 ~sec:3 (Some 20));
  let d = image ~sectors:640 ~media:0xfa () in
  check "media 0xFA: one side of 8" (at d ~trk:1 ~sd:0 ~sec:1 (Some 8));
  (* openMSX SectorBasedDisk::readTrackImpl lays every track out with nine
     sectors, numbered through physToLog: on an 8-sector disk sector 9 of
     track 1 is logical 8 * 1 + 8 = 16, the first sector of track 2. *)
  check "8 sectors per track: sector 9 is the next track's first (openMSX)"
    (at d ~trk:1 ~sd:0 ~sec:9 (Some 16));
  let d = image ~sectors:1280 ~media:0xfb () in
  check "media 0xFB: two sides of 8" (at d ~trk:1 ~sd:1 ~sec:1 (Some 24));
  let d = image ~sectors:1280 ~jmp:true ~bpb:(8, 2) ~media:0xf8 () in
  check "a JMP boot sector wins over the media byte" (at d ~trk:1 ~sd:1 ~sec:1 (Some 24));

  (* Neither: 9 sectors, one side for 360KB and two for other sizes. *)
  let d = image ~sectors:720 ~media:0x00 () in
  check "360KB, no JMP, no media byte: one side" (at d ~trk:1 ~sd:0 ~sec:1 (Some 9));
  let d = image ~sectors:720 ~jmp:true ~bpb:(0, 1) () in
  check "BPB with 0 sectors per track: size decides" (at d ~trk:1 ~sd:0 ~sec:1 (Some 9));
  let d = image ~sectors:720 ~jmp:true ~bpb:(9, 256) () in
  check "BPB with 256 sides: size decides" (at d ~trk:1 ~sd:0 ~sec:1 (Some 9));
  let d = image ~sectors:720 ~jmp:true ~jmp_byte:0xe9 ~bpb:(8, 2) () in
  check "a 0xE9 JMP also marks a BPB" (at d ~trk:1 ~sd:0 ~sec:1 (Some 16));
  (* The BPB fields are 16-bit: 265 sectors per track is out of range even
     though its low byte is 9. 1000 sectors falls back to two sides. *)
  let d = image ~sectors:1000 ~jmp:true ~bpb:(265, 1) () in
  check "BPB fields are 16-bit" (at d ~trk:1 ~sd:0 ~sec:1 (Some 18));

  (* A track cut short by the end of the image has no sectors (openMSX
     empties the whole track when one sector is missing). 1000 sectors,
     two sides of 9: track 55 side 1 is logical 999-1007. *)
  let d = image ~sectors:1000 () in
  check "last complete track reads" (at d ~trk:55 ~sd:0 ~sec:9 (Some 998));
  check "track cut short: its first sector is gone too" (at d ~trk:55 ~sd:1 ~sec:1 None);

  (* WRITE SECTOR goes through the same lookup. *)
  let d = image ~sectors:720 ~jmp:true ~bpb:(9, 1) () in
  write d ~trk:3 ~sd:0 ~sec:4 0x77;
  check "360KB write lands on logical 30"
    (Char.code (Bytes.get d ((30 * sector_bytes) + mark)) = 0x77
     && Char.code (Bytes.get d ((57 * sector_bytes) + mark)) = 57);

  if !failures > 0 then begin
    Printf.eprintf "fdc_geometry_test: %d failure(s)\n" !failures;
    exit 1
  end;
  print_endline "fdc_geometry_test: all passed"
