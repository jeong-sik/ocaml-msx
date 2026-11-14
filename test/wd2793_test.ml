(* WD2793 timing against numbers worked out by hand from openMSX 21.0's
   integer clocks: a motor tick is 3436 master ticks, a turn 200000 motor
   ticks, a data byte 109964 ticks, 6 ms 20618179 ticks and 30 ms
   103090896 ticks. Register numbers are the window offsets from 0x7FF8. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let motor_tick = 3436
let byte_ticks = 109964
let ms6 = 20618179
let ms30 = 103090896
let turn = 200_000 * motor_tick

let status = 0 and track = 1 and sector = 2 and data = 3 and drive = 5 and lines = 7
let busy = 0x01 and drq_status = 0x02 and record_not_found = 0x10 and not_ready = 0x80
let motor_on_a = 0x80

(* A 720KB image whose every byte names its sector and offset. *)
let image () =
  Bytes.init (1440 * 512) (fun i -> Char.chr (((i / 512) * 7 + (i mod 512)) land 0xff))

let sector_byte ~logical k = ((logical * 7) + k) land 0xff
let intrq fdc ~disk ~time = Wd2793.read fdc ~disk ~time lines land 0x40 = 0
let drq fdc ~disk ~time = Wd2793.read fdc ~disk ~time lines land 0x80 = 0

(* Stop the power-on RESTORE, which leaves the track register at 0xFE,
   tell the controller the head is on track 0 and turn the motor on at
   [time]. *)
let ready_machine ~disk ~time =
  let fdc = Wd2793.create () in
  Wd2793.write fdc ~disk ~time status 0xd0;
  Wd2793.write fdc ~disk ~time track 0;
  Wd2793.write fdc ~disk ~time drive motor_on_a;
  fdc

(* Poll [cond] from [t] in [step] increments for at most [limit] polls. *)
let poll ~from ~step ~limit cond =
  let rec go t n = if n >= limit then None else if cond t then Some t else go (t + step) (n + 1) in
  go from 0

let () =
  (* Power-on: RESTORE runs at 30 ms a step with the motor off, so track 0
     never shows and the track register counts 255 steps down to 0. *)
  let disk = image () in
  let fdc = Wd2793.create () in
  check "power-on RESTORE is busy" (Wd2793.read fdc ~disk ~time:0 status land busy <> 0);
  check "still busy just before the 255th step ends"
    (Wd2793.read fdc ~disk ~time:((255 * ms30) - 1) status land busy <> 0);
  check "done at the 255th step" (Wd2793.read fdc ~disk ~time:(255 * ms30) status land busy = 0);
  let fdc = Wd2793.create () in
  check "no INTRQ before the RESTORE ends" (not (intrq fdc ~disk ~time:((255 * ms30) - 1)));
  check "INTRQ when the RESTORE ends" (intrq fdc ~disk ~time:(255 * ms30));
  (* SEEK to track 2 at 6 ms a step, motor on. *)
  let fdc = ready_machine ~disk ~time:0 in
  Wd2793.write fdc ~disk ~time:0 data 2;
  Wd2793.write fdc ~disk ~time:0 status 0x10;
  check "seek busy after one step" (Wd2793.read fdc ~disk ~time:(ms6 + 1) status land busy <> 0);
  check "seek INTRQ off before the second step ends" (not (intrq fdc ~disk ~time:((2 * ms6) - 1)));
  check "seek INTRQ at the second step" (intrq fdc ~disk ~time:(2 * ms6));
  check "track register 2" (Wd2793.read fdc ~disk ~time:(2 * ms6) track = 2);
  (* READ SECTOR 1 of track 0 with the motor turned on at 0: the angle is
     0, sector 1's C byte is byte 162, 162 * 32 = 5184 motor ticks away;
     the first DRQ comes 46 bytes later. *)
  let fdc = ready_machine ~disk ~time:0 in
  Wd2793.write fdc ~disk ~time:0 sector 1;
  Wd2793.write fdc ~disk ~time:0 status 0x80;
  let header = 5184 * motor_tick in
  let first = header + (46 * byte_ticks) in
  check "no DRQ just before the first byte" (not (drq fdc ~disk ~time:(first - 1)));
  check "DRQ at the first byte" (drq fdc ~disk ~time:first);
  check "status shows DRQ" (Wd2793.read fdc ~disk ~time:first status land drq_status <> 0);
  let ok = ref true in
  for k = 0 to 511 do
    let t = first + (k * byte_ticks) in
    if k < 511 && drq fdc ~disk ~time:(t - 1) && k > 0 then ok := false;
    if Wd2793.read fdc ~disk ~time:t data <> sector_byte ~logical:0 k then ok := false
  done;
  check "512 bytes of sector 0, one per byte time" !ok;
  let last = first + (511 * byte_ticks) in
  check "INTRQ after the last byte" (intrq fdc ~disk ~time:last);
  check "not busy after the last byte" (Wd2793.read fdc ~disk ~time:last status land busy = 0);
  (* Reading the bytes late loses the ones that passed. *)
  let fdc = ready_machine ~disk ~time:0 in
  Wd2793.write fdc ~disk ~time:0 sector 1;
  Wd2793.write fdc ~disk ~time:0 status 0x80;
  check "a late read gets the newest byte"
    (Wd2793.read fdc ~disk ~time:(first + (3 * byte_ticks)) data = sector_byte ~logical:0 3);
  check "and reports lost data" (Wd2793.read fdc ~disk ~time:(first + (3 * byte_ticks)) status land 0x04 <> 0);
  (* Track 1 side 1 sector 3 is logical 9 * 3 + 2 = 29. *)
  let fdc = ready_machine ~disk ~time:0 in
  Wd2793.write fdc ~disk ~time:0 data 1;
  Wd2793.write fdc ~disk ~time:0 status 0x10;
  let t1 = ms6 in
  Wd2793.write fdc ~disk ~time:t1 4 1;
  Wd2793.write fdc ~disk ~time:t1 sector 3;
  Wd2793.write fdc ~disk ~time:t1 status 0x80;
  (match poll ~from:t1 ~step:(byte_ticks / 4) ~limit:40_000 (fun t -> drq fdc ~disk ~time:t) with
   | None -> check "track 1 side 1 sector 3 gives DRQ within a turn" false
   | Some t ->
     check "track 1 side 1 sector 3 is logical sector 29"
       (Wd2793.read fdc ~disk ~time:t data = sector_byte ~logical:29 0));
  (* A sector the track does not have: RNF at the fifth index pulse. The
     angle at 0 is 0, so the fifth pulse starts five turns on. *)
  let fdc = ready_machine ~disk ~time:0 in
  Wd2793.write fdc ~disk ~time:0 sector 10;
  Wd2793.write fdc ~disk ~time:0 status 0x80;
  check "still searching just before the fifth pulse"
    (Wd2793.read fdc ~disk ~time:((5 * turn) - 1) status land busy <> 0);
  let st = Wd2793.read fdc ~disk ~time:(5 * turn) status in
  check "RNF at the fifth pulse" (st land busy = 0 && st land record_not_found <> 0);
  (* Drive B is empty: a read ends at once, not ready. *)
  let fdc = ready_machine ~disk ~time:0 in
  Wd2793.write fdc ~disk ~time:0 drive (motor_on_a lor 1);
  Wd2793.write fdc ~disk ~time:0 status 0x80;
  let st = Wd2793.read fdc ~disk ~time:0 status in
  check "drive B read ends at once, not ready" (st land busy = 0 && st land not_ready <> 0);
  (* Drive B never shows track 0, so RESTORE keeps stepping. *)
  Wd2793.write fdc ~disk ~time:0 status 0x00;
  check "drive B RESTORE still busy after 10 steps"
    (Wd2793.read fdc ~disk ~time:(10 * ms6) status land busy <> 0);
  (* With the motor off the disk stands still and has no sector to find. *)
  let fdc = Wd2793.create () in
  Wd2793.write fdc ~disk ~time:0 status 0xd0;
  Wd2793.write fdc ~disk ~time:0 sector 1;
  Wd2793.write fdc ~disk ~time:0 status 0x80;
  let st = Wd2793.read fdc ~disk ~time:0 status in
  check "motor off: RNF at once" (st land busy = 0 && st land record_not_found <> 0);
  (* WRITE SECTOR: the first DRQ comes 9 bytes after the header; the CPU
     answers each DRQ and the sector lands in the image. *)
  let disk = image () in
  let fdc = ready_machine ~disk ~time:0 in
  Wd2793.write fdc ~disk ~time:0 sector 2;
  Wd2793.write fdc ~disk ~time:0 status 0xa0;
  let header2 = (162 + 658) * 32 * motor_tick in
  let rec feed t k polls =
    if k >= 512 || polls > 100_000 then t
    else if drq fdc ~disk ~time:t then begin
      Wd2793.write fdc ~disk ~time:t data (k lxor 0x5a);
      feed t (k + 1) polls
    end
    else feed (t + (byte_ticks / 8)) k (polls + 1)
  in
  check "no write DRQ before 9 bytes" (not (drq fdc ~disk ~time:(header2 + (9 * byte_ticks) - 1)));
  let t_end = feed (header2 + (9 * byte_ticks)) 0 0 in
  check "write sector ends"
    (poll ~from:t_end ~step:byte_ticks ~limit:100
       (fun t -> Wd2793.read fdc ~disk ~time:t status land busy = 0) <> None);
  let ok = ref true in
  for k = 0 to 511 do
    if Char.code (Bytes.get disk (512 + k)) <> (k lxor 0x5a) land 0xff then ok := false
  done;
  check "written sector in the image" !ok;
  check "neighbour sector untouched" (Char.code (Bytes.get disk 1024) = sector_byte ~logical:2 0);
  (* A save in the middle of a read restores the same stream. *)
  let disk = image () in
  let fdc = ready_machine ~disk ~time:0 in
  Wd2793.write fdc ~disk ~time:0 sector 1;
  Wd2793.write fdc ~disk ~time:0 status 0x80;
  ignore (Wd2793.read fdc ~disk ~time:first data);
  let w = State_codec.writer () in
  Wd2793.write_state w fdc;
  let r = State_codec.reader (State_codec.finish w) in
  let restored = Wd2793.create () in
  Wd2793.read_state r restored;
  State_codec.end_of_input r;
  let t = first + byte_ticks in
  check "restored controller reads byte 1"
    (Wd2793.read restored ~disk ~time:t data = sector_byte ~logical:0 1);
  (* The header search skips an ID field less than four bytes ahead.
     Sector 2's FE is byte 819 and its C byte 820 (angle 26240). At angle
     26080 the head is at byte 815, four bytes before the FE: sector 2 is
     next. The angle rounds up, so one tick before 26080 motor ticks still
     reads 26080. *)
  let read_sector_2_from t0 =
    let fdc = ready_machine ~disk ~time:0 in
    Wd2793.write fdc ~disk ~time:t0 sector 2;
    Wd2793.write fdc ~disk ~time:t0 status 0x80;
    fdc
  in
  let t0 = (26080 * motor_tick) - 1 in
  let fdc = read_sector_2_from t0 in
  let first2 = t0 + ((26240 - 26080) * motor_tick) + (46 * byte_ticks) in
  check "sector 2 four bytes ahead: no DRQ before its first byte" (not (drq fdc ~disk ~time:(first2 - 1)));
  check "sector 2 four bytes ahead: DRQ at its first byte" (drq fdc ~disk ~time:first2);
  check "sector 2 four bytes ahead: byte 0 of logical sector 1"
    (Wd2793.read fdc ~disk ~time:first2 data = sector_byte ~logical:1 0);
  (* At angle 26081 the head is at byte 816: sector 2 comes round a turn
     later. *)
  let t0 = 26081 * motor_tick in
  let fdc = read_sector_2_from t0 in
  let first2 = t0 + ((200_000 + 26240 - 26081) * motor_tick) + (46 * byte_ticks) in
  check "sector 2 three bytes ahead: no DRQ before the next turn" (not (drq fdc ~disk ~time:(first2 - 1)));
  check "sector 2 three bytes ahead: DRQ a turn later" (drq fdc ~disk ~time:first2);
  (* The E flag waits 30 ms before the search: the angle is then 30004,
     past sector 1, which comes round a turn later. *)
  let fdc = ready_machine ~disk ~time:0 in
  Wd2793.write fdc ~disk ~time:0 sector 1;
  Wd2793.write fdc ~disk ~time:0 status 0x84;
  let settled = (ms30 + motor_tick - 1) / motor_tick in
  let first_e = ms30 + ((200_000 + 5184 - settled) * motor_tick) + (46 * byte_ticks) in
  check "E flag: no DRQ before the settled search finds sector 1" (not (drq fdc ~disk ~time:(first_e - 1)));
  check "E flag: DRQ after the settled search" (drq fdc ~disk ~time:first_e);
  (* READ ADDRESS at angle 0 reads sector 1's ID field, C H R N and the CRC
     over A1 A1 A1 FE C H R N, one byte per byte time from the C byte on,
     and copies C into the sector register. *)
  let crc_ccitt bytes =
    List.fold_left
      (fun crc b ->
        let crc = ref (crc lxor (b lsl 8)) in
        for _ = 1 to 8 do
          crc := if !crc land 0x8000 <> 0 then ((!crc lsl 1) lxor 0x1021) land 0xffff
            else (!crc lsl 1) land 0xffff
        done;
        !crc)
      0xffff bytes
  in
  let id_crc = crc_ccitt [ 0xa1; 0xa1; 0xa1; 0xfe; 0; 0; 1; 2 ] in
  let fdc = ready_machine ~disk ~time:0 in
  Wd2793.write fdc ~disk ~time:0 sector 7;
  Wd2793.write fdc ~disk ~time:0 status 0xc0;
  let id_at = 5184 * motor_tick in
  check "READ ADDRESS: no DRQ before the C byte has passed" (not (drq fdc ~disk ~time:(id_at + byte_ticks - 1)));
  let got = List.init 6 (fun k -> Wd2793.read fdc ~disk ~time:(id_at + ((k + 1) * byte_ticks)) data) in
  check "READ ADDRESS: C H R N CRC of sector 1"
    (got = [ 0; 0; 1; 2; id_crc lsr 8; id_crc land 0xff ]);
  check "READ ADDRESS: the ID CRC of 00 00 01 02 is CA6F" (id_crc = 0xca6f);
  check "READ ADDRESS: sector register holds C" (Wd2793.read fdc ~disk ~time:(id_at + (6 * byte_ticks)) sector = 0);
  let st = Wd2793.read fdc ~disk ~time:(id_at + (6 * byte_ticks)) status in
  check "READ ADDRESS: ends without a CRC error" (st land busy = 0 && st land 0x08 = 0);
  (* Register 5 bit 2 reads 0 once after a disk change. A new controller
     starts with the news of a disk. *)
  let fdc = Wd2793.create () in
  check "disk change: first read reports a change" (Wd2793.read fdc ~disk ~time:0 drive land 0x04 = 0);
  check "disk change: the read took the news" (Wd2793.read fdc ~disk ~time:0 drive land 0x04 <> 0);
  Wd2793.disk_changed fdc;
  check "disk change: reported again after a new disk" (Wd2793.read fdc ~disk ~time:0 drive land 0x04 = 0);
  (* FORCE INTERRUPT with bit 3 raises INTRQ at once and keeps it through
     status reads; a plain FORCE INTERRUPT drops it. *)
  let fdc = ready_machine ~disk ~time:0 in
  Wd2793.write fdc ~disk ~time:0 status 0xd8;
  check "D8: INTRQ at once" (intrq fdc ~disk ~time:0);
  ignore (Wd2793.read fdc ~disk ~time:1 status);
  check "D8: INTRQ survives a status read" (intrq fdc ~disk ~time:2);
  Wd2793.write fdc ~disk ~time:3 status 0xd0;
  check "D0: INTRQ off" (not (intrq fdc ~disk ~time:3));
  (* Type I status: the index bit while the angle is below 4000, track 0
     and write protect from the drive. The motor turned on at 0, so angle
     3999 lasts until 3999 motor ticks and 4000 starts one tick later. *)
  let fdc = ready_machine ~disk ~time:0 in
  let st = Wd2793.read fdc ~disk ~time:(3999 * motor_tick) status in
  check "index bit at angle 3999" (st land 0x02 <> 0);
  check "track 0 bit with the motor on" (st land 0x04 <> 0);
  check "drive A with a disk is not write protected" (st land 0x40 = 0);
  check "no index bit at angle 4000"
    (Wd2793.read fdc ~disk ~time:((3999 * motor_tick) + 1) status land 0x02 = 0);
  Wd2793.write fdc ~disk ~time:(4000 * motor_tick) drive 0;
  check "no track 0 bit with the motor off"
    (Wd2793.read fdc ~disk ~time:(4000 * motor_tick) status land 0x04 = 0);
  Wd2793.write fdc ~disk ~time:(4000 * motor_tick) drive (motor_on_a lor 1);
  let st = Wd2793.read fdc ~disk ~time:(4000 * motor_tick) status in
  check "drive B: write protected and not ready" (st land 0x40 <> 0 && st land not_ready <> 0);
  (* An empty drive A reads write protected only while the motor runs:
     the Philips interface needs the motor on for that signal. *)
  let empty = Bytes.empty in
  let fdc = ready_machine ~disk:empty ~time:0 in
  check "empty drive A, motor on: write protected"
    (Wd2793.read fdc ~disk:empty ~time:0 status land 0x40 <> 0);
  Wd2793.write fdc ~disk:empty ~time:0 drive 0;
  check "empty drive A, motor off: write protect reads 0"
    (Wd2793.read fdc ~disk:empty ~time:0 status land 0x40 = 0);
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "wd2793_test: all pass"
