(* The NMS 8250 disk interface, timed like openMSX 21.0. The comments name
   the openMSX function each part answers to; the code is written from the
   behaviour, not copied. *)

let ticks_per_cycle = 960
let main_freq = 3_579_545 * ticks_per_cycle
let never = max_int

(* A duration of [n] ms, cut down to whole ticks (EmuDuration::msec). *)
let msec n = n * main_freq / 1000

(* The drive's 1 MHz motor clock, rounded to whole ticks the way openMSX
   Clock<1000000> rounds its period. A turn is 200000 of its ticks (300
   rpm) and the index pulse lasts the first 4000. *)
let motor_tick = (main_freq + 500_000) / 1_000_000
let ticks_per_rotation = 200_000
let index_duration = ticks_per_rotation / 50
let max_track = 85

(* A track is 6250 bytes; one passes the head every [byte_ticks]
   (DynamicClock::setFreq(6250 * 5), rounded to whole ticks). *)
let track_length = 6250
let rotations_per_second = 5
let byte_ticks =
  let freq = track_length * rotations_per_second in
  (main_freq + (freq / 2)) / freq

let head_idle = 3 * main_freq
let settle_delay = msec 30
let step_delays = [| msec 6; msec 12; msec 20; msec 30 |]
let sector_size = 512
let sectors_per_raw_track = 9

(* Status register bits. Bits 1, 2 and 5 mean one thing after a type I
   command and another after the rest. *)
let busy = 0x01
let index_bit = 0x02
let drq_bit = 0x02
let track00 = 0x04
let lost_data = 0x04
let crc_error = 0x08
let seek_error = 0x10
let record_not_found = 0x10
let head_loaded = 0x20
let record_type = 0x20
let write_protected = 0x40
let not_ready = 0x80

(* Command register bits. *)
let step_rate = 0x03
let verify_or_settle = 0x04
let head_load = 0x08
let update_track = 0x10
let multi_sector = 0x10
let deleted_mark = 0x01
let irq_on_index = 0x04
let irq_now = 0x08

(* Window register 7: both lines are active low. *)
let not_intrq = 0x40
let not_drq = 0x80

(* --- CRC-16/CCITT, as a WD2793 computes it over marks and data --- *)

let crc_byte crc v =
  let c = ref (crc lxor (v lsl 8)) in
  for _ = 1 to 8 do
    c := if !c land 0x8000 <> 0 then ((!c lsl 1) lxor 0x1021) land 0xffff
      else (!c lsl 1) land 0xffff
  done;
  !c

let crc_init = 0xffff
let crc_of bytes = List.fold_left crc_byte crc_init bytes
let a1 = 0xa1
let id_mark = 0xfe
let data_mark = 0xfb
let index_mark = 0xfc
let gap = 0x4e

(* --- Where an image's sectors lie (Disk::detectGeometry, physToLog) --- *)

type geometry = { sides : int; sectors_per_track : int }

let dd_sectors_per_track = 9
let ds_dd_sectors = 1440
let ss_dd_sectors = 720
let media_8_sectors = 0x02
let media_two_sides = 0x01
let media_lowest = 0xf8
let bpb_sectors_per_track = 0x18
let bpb_sides = 0x1a
let jmp_near = 0xe9
let jmp_short = 0xeb

let geometry disk =
  let sectors = Bytes.length disk / sector_size in
  let byte i = Char.code (Bytes.get disk i) in
  let word i = byte i lor (byte (i + 1) lsl 8) in
  let fallback =
    { sectors_per_track = dd_sectors_per_track;
      sides = (if sectors = ss_dd_sectors then 1 else 2) }
  in
  let valid n = n >= 1 && n <= 255 in
  if sectors = ds_dd_sectors then { sides = 2; sectors_per_track = dd_sectors_per_track }
  else if sectors < 1 then fallback
  else if byte 0 = jmp_near || byte 0 = jmp_short then begin
    let sectors_per_track = word bpb_sectors_per_track and sides = word bpb_sides in
    if valid sectors_per_track && valid sides then { sides; sectors_per_track } else fallback
  end
  else if sectors < 2 then fallback
  else begin
    let media = byte sector_size in
    if media >= media_lowest then
      { sectors_per_track =
          (if media land media_8_sectors <> 0 then 8 else dd_sectors_per_track);
        sides = (if media land media_two_sides <> 0 then 2 else 1) }
    else fallback
  end

let logical_sector g ~track ~side ~sector =
  (g.sectors_per_track * (side + (g.sides * track))) + (sector - 1)

(* --- The raw track under the head (RawTrack) --- *)

type sector = {
  addr_idx : int;  (* the C byte, just after the ID mark *)
  cylinder : int;
  head : int;
  number : int;
  size_code : int;
  addr_crc_err : bool;
  data_idx : int option;  (* first data byte, when a data mark follows *)
  deleted : bool;
}

let no_sector =
  { addr_idx = 0; cylinder = 0; head = 0; number = 0; size_code = 0;
    addr_crc_err = false; data_idx = None; deleted = false }

type drive = {
  mutable head_pos : int;
  mutable side : int;
  mutable motor : bool;
  mutable start_angle : int;
  mutable motor_base : int;  (* the motor clock's last whole tick *)
  raw : Bytes.t;
  mutable idam : int list;  (* ID mark positions, ascending *)
  mutable valid : bool;  (* [raw] holds the track under the head *)
  mutable dirty : bool;  (* [raw] has bytes the image does not *)
  mutable changed : bool;  (* DiskChanger's flag *)
}

let wrap i = ((i mod track_length) + track_length) mod track_length
let raw_get d i = Char.code (Bytes.get d.raw (wrap i))

let raw_set d i v ~idam =
  let i = wrap i in
  Bytes.set d.raw i (Char.chr v);
  let others = List.filter (( <> ) i) d.idam in
  d.idam <- (if idam then List.sort compare (i :: others) else others)

let raw_crc d crc start len =
  let c = ref crc in
  for k = 0 to len - 1 do c := crc_byte !c (raw_get d (start + k)) done;
  !c

(* The sector whose ID mark is at [i] (RawTrack::decodeSectorImpl). A data
   mark counts when it starts within 43 bytes of the ID field's end. *)
let decode d i =
  if raw_get d i <> id_mark then None
  else begin
    let a = i + 1 in
    let crc = raw_crc d (crc_of [ a1; a1; a1; id_mark ]) a 4 in
    let stored = (raw_get d (a + 4) lsl 8) lor raw_get d (a + 5) in
    let addr_crc_err = stored <> crc in
    let found = ref None in
    if not addr_crc_err then begin
      let k = ref 0 in
      while !found = None && !k < 43 do
        let j = a + 6 + !k in
        let mark = raw_get d (j + 3) in
        if raw_get d j = a1 && raw_get d (j + 1) = a1 && raw_get d (j + 2) = a1
           && (mark = data_mark || mark = data_mark - 3)
        then found := Some (j + 4, mark <> data_mark);
        incr k
      done
    end;
    Some
      { addr_idx = a; cylinder = raw_get d a; head = raw_get d (a + 1);
        number = raw_get d (a + 2); size_code = raw_get d (a + 3); addr_crc_err;
        data_idx = Option.map fst !found;
        deleted = (match !found with Some (_, del) -> del | None -> false) }
  end

(* The track in the standard IBM layout (SectorBasedDisk::readTrack): gap,
   index mark, then nine 658-byte sectors numbered 1-9, each a 512-byte
   sector of the image. A track that would need a sector the image does
   not have is empty, as an unformatted track reads. *)
let build_track d disk =
  Bytes.fill d.raw 0 track_length (Char.chr gap);
  d.idam <- [];
  let total = Bytes.length disk / sector_size in
  if total > 0 then begin
    let g = geometry disk in
    let logical j = logical_sector g ~track:d.head_pos ~side:d.side ~sector:(j + 1) in
    let complete = ref true in
    for j = 0 to sectors_per_raw_track - 1 do
      if logical j >= total then complete := false
    done;
    if !complete then begin
      let idx = ref 0 in
      let put v = Bytes.set d.raw !idx (Char.chr v); incr idx in
      let fill n v = for _ = 1 to n do put v done in
      fill 80 gap; fill 12 0; fill 3 0xc2; put index_mark; fill 50 gap;
      for j = 0 to sectors_per_raw_track - 1 do
        fill 12 0; fill 3 a1;
        d.idam <- d.idam @ [ !idx ];
        put id_mark; put d.head_pos; put d.side; put (j + 1); put 2;
        let c = raw_crc d crc_init (!idx - 8) 8 in
        put (c lsr 8); put (c land 0xff);
        fill 22 gap; fill 12 0; fill 3 a1; put data_mark;
        Bytes.blit disk (logical j * sector_size) d.raw !idx sector_size;
        idx := !idx + sector_size;
        let c = raw_crc d crc_init (!idx - (sector_size + 4)) (sector_size + 4) in
        put (c lsr 8); put (c land 0xff);
        fill 84 gap
      done;
      fill 182 gap
    end
  end

(* A written track back into the image (SectorBasedDisk::writeTrackImpl):
   every sector with a data block whose number is on the track; track,
   head, size and CRCs are not looked at. A sector the image does not
   have stops the write there. *)
let store_track d disk =
  let total = Bytes.length disk / sector_size in
  let g = geometry disk in
  let rec go = function
    | [] -> Ok ()
    | i :: rest ->
      (match decode d i with
       | Some ({ data_idx = Some data; number; _ })
         when number >= 1 && number <= g.sectors_per_track ->
         let l = logical_sector g ~track:d.head_pos ~side:d.side ~sector:number in
         if l >= total then Error ()
         else begin
           for k = 0 to sector_size - 1 do
             Bytes.set disk ((l * sector_size) + k) (Char.chr (raw_get d (data + k)))
           done;
           go rest
         end
       | _ -> go rest)
  in
  go d.idam

let flush d disk =
  if d.valid && d.dirty then begin
    d.dirty <- false;
    store_track d disk
  end
  else Ok ()

let invalidate d disk =
  ignore (flush d disk);
  d.valid <- false

(* RealDrive::getTrack: a turning disk shows the track under the head. *)
let get_track d disk =
  if d.motor && not d.valid then begin
    build_track d disk;
    d.valid <- true;
    d.dirty <- false
  end

(* RealDrive::getCurrentAngle: the angle stands still while the motor is
   off. *)
let angle d time =
  if d.motor then
    let turned = (time - d.motor_base + motor_tick - 1) / motor_tick in
    (d.start_angle + turned) mod ticks_per_rotation
  else d.start_angle

let set_motor_now d disk time on =
  if not on then invalidate d disk;
  d.start_angle <- angle d time;
  d.motor <- on;
  d.motor_base <- time - ((time - d.motor_base) mod motor_tick)

(* --- The controller --- *)

type drive_select = Drive_a | Drive_b | No_drive

type event =
  | Seek
  | Type2_loaded
  | Type2_not_found
  | Type2_rotated
  | Check_write
  | Pre_write_sector
  | Write_sector
  | Post_write_sector
  | Type3_loaded
  | Type3_rotated
  | Write_track
  | Read_track

type t = {
  a : drive;
  mutable selected : drive_select;
  mutable mux_side : int;
  mutable mux_motor : bool;
  mutable side_reg : int;
  mutable drive_reg : int;
  mutable status : int;
  mutable command : int;
  mutable track_reg : int;
  mutable sector_reg : int;
  mutable data_reg : int;
  mutable direction_in : bool;
  mutable drq_time : int;  (* DRQ is up from this time on *)
  mutable irq_time : int;  (* INTRQ is up from this time on *)
  mutable immediate_irq : bool;
  mutable hld_time : int;
  mutable data_current : int;
  mutable data_available : int;
  mutable data_reg_written : bool;
  mutable data_out_reg : int;
  mutable last_was_a1 : bool;
  mutable last_was_crc : bool;
  mutable crc : int;
  mutable pulse5 : int;
  mutable found : sector;
  mutable event : (event * int) option;
}

(* What the selected drive shows. Drive B and "none" are the empty
   DummyDrive: never ready, never at track 0, write protected. *)
let on_a t = t.selected = Drive_a
let inserted t disk = on_a t && Bytes.length disk > 0
let at_track00 t = on_a t && t.a.motor && t.a.head_pos = 0

let protected t disk =
  if on_a t then t.a.motor && Bytes.length disk = 0 else true

let index_pulse t disk time =
  on_a t && t.a.motor && inserted t disk && angle t.a time < index_duration

(* RealDrive::getTimeTillIndexPulse: the start of the [count]th index pulse
   from now. *)
let index_after t disk time count =
  if not (on_a t && t.a.motor && inserted t disk) then never
  else
    let left = ticks_per_rotation - angle t.a time in
    time + (left * motor_tick) + (ticks_per_rotation * motor_tick * (count - 1))

(* RealDrive::getNextSector: the next ID field at least four bytes ahead,
   and when its C byte passes the head. *)
let next_sector t disk time =
  if not (on_a t) then None
  else begin
    let d = t.a in
    get_track d disk;
    let ang = angle d time in
    let start = ((ang * track_length) + ticks_per_rotation - 1) / ticks_per_rotation + 4 in
    let later, earlier = List.partition (fun i -> i >= start) d.idam in
    match List.find_map (decode d) (later @ earlier) with
    | None -> None
    | Some s ->
      let at = ((s.addr_idx * ticks_per_rotation) + track_length - 1) / track_length in
      let delta = at - ang in
      let delta = if delta < 4 then delta + ticks_per_rotation else delta in
      Some (time + (delta * motor_tick), s)
  end

let read_byte t disk i =
  if on_a t then begin
    get_track t.a disk;
    if t.a.valid then raw_get t.a i else 0
  end
  else 0

let write_byte t disk i v ~idam =
  if on_a t then begin
    get_track t.a disk;
    raw_set t.a i v ~idam;
    t.a.dirty <- true;
    Ok ()
  end
  else Error ()

let flush_selected t disk = if on_a t then flush t.a disk else Ok ()

let drq t time = time >= t.drq_time
let irq t time = t.immediate_irq || t.irq_time <= time
let head_active t time = t.hld_time <> never && t.hld_time <= time && time < t.hld_time + head_idle

let schedule t ev time = t.event <- Some (ev, time)

let end_command t time =
  if head_active t time then t.hld_time <- time;
  t.drq_time <- never;
  t.irq_time <- 0;
  t.status <- t.status land lnot busy

let reading_command t =
  t.command land 0xe0 = 0x80 || t.command land 0xf0 = 0xc0 || t.command land 0xf0 = 0xe0

let writing_command t = t.command land 0xe0 = 0xa0 || t.command land 0xf0 = 0xf0

(* Type I: RESTORE, SEEK, STEP. *)
let rec seek t disk time =
  if t.track_reg = t.data_reg then end_command t time
  else begin
    t.direction_in <- t.data_reg > t.track_reg;
    step t disk time
  end

and step t disk time =
  if t.command land update_track <> 0 || t.command land 0xe0 = 0 then
    t.track_reg <- (t.track_reg + if t.direction_in then 1 else -1) land 0xff;
  if (not t.direction_in) && at_track00 t then begin
    t.track_reg <- 0;
    end_command t time
  end
  else begin
    if on_a t then begin
      invalidate t.a disk;
      t.a.head_pos <-
        (if t.direction_in then min max_track (t.a.head_pos + 1) else max 0 (t.a.head_pos - 1))
    end;
    schedule t Seek (time + step_delays.(t.command land step_rate))
  end

let start_type1 t disk time =
  t.status <- (t.status land lnot (seek_error lor crc_error)) lor busy;
  t.hld_time <- (if t.command land head_load <> 0 then time else never);
  match t.command land 0xf0 with
  | 0x00 -> t.track_reg <- 0xff; t.data_reg <- 0; seek t disk time
  | 0x10 -> seek t disk time
  | 0x20 | 0x30 -> step t disk time
  | 0x40 | 0x50 -> t.direction_in <- true; step t disk time
  | _ -> t.direction_in <- false; step t disk time

(* Type II: READ SECTOR, WRITE SECTOR. *)
let rec type2_loaded t disk time =
  if t.command land 0xe0 = 0xa0 && protected t disk then begin
    t.status <- t.status lor write_protected;
    end_command t time
  end
  else begin
    t.pulse5 <- index_after t disk time 5;
    type2_search t disk time
  end

and type2_search t disk time =
  match next_sector t disk time with
  | Some (at, s) when at < t.pulse5 ->
    t.found <- s;
    schedule t Type2_rotated at
  | _ ->
    if t.pulse5 < never then schedule t Type2_not_found t.pulse5
    else type2_not_found t time

and type2_not_found t time =
  t.status <- t.status lor record_not_found;
  end_command t time

let start_read_sector t time data =
  t.crc <- crc_of [ a1; a1; a1; (if t.found.deleted then data_mark - 3 else data_mark) ];
  let gap_bytes = data - t.found.addr_idx in
  let gap_bytes = if gap_bytes < 0 then gap_bytes + track_length else gap_bytes in
  t.drq_time <- time + ((gap_bytes + 2) * byte_ticks);
  t.data_current <- data;
  t.data_available <- 128 lsl (t.found.size_code land 3)

(* The ID field ends 7 bytes after its C byte; DRQ comes 2 bytes later and
   the controller checks for the first byte 8 bytes after that. *)
let start_write_sector t time =
  t.drq_time <- time + (9 * byte_ticks);
  schedule t Check_write (t.drq_time + (8 * byte_ticks))

let type2_rotated t disk time =
  t.status <- (if t.found.addr_crc_err then t.status lor crc_error else t.status land lnot crc_error);
  if t.found.addr_crc_err || t.found.cylinder <> t.track_reg || t.found.number <> t.sector_reg then
    type2_search t disk time
  else
    match t.found.data_idx with
    | None -> type2_search t disk time
    | Some data ->
      if t.command land 0xe0 = 0x80 then start_read_sector t time data
      else start_write_sector t time

let start_type2 t disk time =
  t.status <- (t.status land lnot (lost_data lor record_not_found lor record_type lor write_protected)) lor busy;
  t.data_reg_written <- false;
  if not (inserted t disk) then end_command t time
  else begin
    t.hld_time <- time;
    if t.command land verify_or_settle <> 0 then schedule t Type2_loaded (time + settle_delay)
    else type2_loaded t disk time
  end

let fail_not_ready t time =
  t.status <- t.status lor not_ready;
  end_command t time

let check_start_write t time =
  if not t.data_reg_written then begin
    t.status <- t.status lor lost_data;
    end_command t time
  end
  else begin
    t.data_current <- t.found.addr_idx + 6 + 22;
    schedule t Pre_write_sector (time + (12 * byte_ticks));
    t.drq_time <- never;
    t.data_available <- 16
  end

(* 12 zero bytes and three A1 bytes, then the data mark. *)
let pre_write_sector t disk time =
  t.data_available <- t.data_available - 1;
  if t.data_available > 0 then begin
    match write_byte t disk t.data_current (if t.data_available >= 4 then 0 else a1) ~idam:false with
    | Error () -> fail_not_ready t time
    | Ok () ->
      t.data_current <- t.data_current + 1;
      schedule t Pre_write_sector (time + byte_ticks);
      t.drq_time <- never
  end
  else begin
    let mark = if t.command land deleted_mark <> 0 then data_mark - 3 else data_mark in
    match write_byte t disk t.data_current mark ~idam:false with
    | Error () -> fail_not_ready t time
    | Ok () ->
      t.data_current <- t.data_current + 1;
      t.crc <- crc_byte (crc_of [ a1; a1; a1 ]) mark;
      t.data_out_reg <- t.data_reg;
      t.data_reg_written <- false;
      t.data_available <- 128 lsl (t.found.size_code land 3);
      t.drq_time <- time;
      schedule t Write_sector (time + byte_ticks)
  end

let write_sector_data t disk time =
  match write_byte t disk t.data_current t.data_out_reg ~idam:false with
  | Error () -> fail_not_ready t time
  | Ok () ->
    t.data_current <- t.data_current + 1;
    t.crc <- crc_byte t.crc t.data_out_reg;
    t.data_available <- t.data_available - 1;
    if t.data_available > 0 then begin
      if t.data_reg_written then begin
        t.data_out_reg <- t.data_reg;
        t.data_reg_written <- false
      end
      else begin
        t.data_out_reg <- 0;
        t.status <- t.status lor lost_data
      end;
      t.drq_time <- time;
      schedule t Write_sector (time + byte_ticks);
      if t.data_available = 1 then t.drq_time <- never
    end
    else begin
      t.data_available <- 3;
      schedule t Post_write_sector (time + byte_ticks);
      t.drq_time <- never
    end

(* Two CRC bytes and one 0xFE, then the track goes to the image. *)
let post_write_sector t disk time =
  t.data_available <- t.data_available - 1;
  if t.data_available > 0 then begin
    let v = if t.data_available = 2 then t.crc lsr 8 else t.crc land 0xff in
    match write_byte t disk t.data_current v ~idam:false with
    | Error () -> fail_not_ready t time
    | Ok () ->
      t.data_current <- t.data_current + 1;
      schedule t Post_write_sector (time + byte_ticks);
      t.drq_time <- never
  end
  else
    match write_byte t disk t.data_current id_mark ~idam:false with
    | Error () -> fail_not_ready t time
    | Ok () ->
      t.data_current <- t.data_current + 1;
      (match flush_selected t disk with
       | Error () -> fail_not_ready t time
       | Ok () ->
         if t.command land multi_sector = 0 then end_command t time
         else begin
           t.drq_time <- never;
           t.sector_reg <- (t.sector_reg + 1) land 0xff;
           type2_loaded t disk time
         end)

(* Type III: READ ADDRESS, READ TRACK, WRITE TRACK. *)
let type3_loaded t disk time =
  if t.command land 0xf0 = 0xf0 && protected t disk then begin
    t.status <- t.status lor write_protected;
    end_command t time
  end
  else if t.command land 0xf0 = 0xc0 then begin
    match next_sector t disk time with
    | None ->
      t.status <- t.status lor record_not_found;
      end_command t time
    | Some (at, s) ->
      t.found <- s;
      t.data_current <- s.addr_idx;
      t.data_available <- 6;
      t.sector_reg <- read_byte t disk s.addr_idx;
      schedule t Type3_rotated at
  end
  else begin
    let at = index_after t disk time 1 in
    if at = never then end_command t time else schedule t Type3_rotated at
  end

(* RawTrack::applyWd2793ReadTrackQuirk: READ TRACK returns the first A1 of
   each ID and data mark as 0x14. The change stays in the held track until
   the command ends. *)
let read_track_quirk d =
  List.iter
    (fun i ->
      if raw_get d (i - 3) = a1 && raw_get d (i - 2) = a1 && raw_get d (i - 1) = a1
         && raw_get d i = id_mark
      then begin
        raw_set d (i - 3) 0x14 ~idam:false;
        match decode d i with
        | Some { data_idx = Some data; _ }
          when raw_get d (data - 4) = a1 && raw_get d (data - 3) = a1
               && raw_get d (data - 2) = a1 ->
          raw_set d (data - 4) 0x14 ~idam:false
        | _ -> ()
      end)
    d.idam

let type3_rotated t disk time =
  match t.command land 0xf0 with
  | 0xc0 -> t.drq_time <- time + byte_ticks
  | 0xe0 ->
    if on_a t then begin
      get_track t.a disk;
      read_track_quirk t.a
    end;
    t.data_current <- 0;
    t.data_available <- track_length;
    schedule t Read_track (time + (track_length * byte_ticks));
    t.drq_time <- time + byte_ticks
  | _ ->
    if not t.data_reg_written then begin
      t.status <- t.status lor lost_data;
      end_command t time
    end
    else begin
      t.data_current <- 0;
      t.data_available <- track_length;
      t.last_was_a1 <- false;
      t.last_was_crc <- false;
      t.data_out_reg <- t.data_reg;
      t.data_reg_written <- false;
      t.drq_time <- time;
      schedule t Write_track (time + byte_ticks)
    end

(* WRITE TRACK turns F5 into A1 (and restarts the CRC), F6 into C2, F7
   into the two CRC bytes; an FE right after an A1 is an ID mark. *)
let write_track_data t disk time =
  let prev_a1 = t.last_was_a1 in
  t.last_was_a1 <- false;
  let crc_low = ref 0 and idam = ref false in
  if t.last_was_crc then t.last_was_crc <- false
  else begin
    match t.data_out_reg with
    | 0xf5 ->
      t.data_out_reg <- a1;
      t.last_was_a1 <- true;
      t.crc <- crc_of [ a1; a1 ]
    | 0xf6 -> t.data_out_reg <- 0xc2
    | 0xf7 ->
      t.data_out_reg <- t.crc lsr 8;
      crc_low := t.crc land 0xff;
      t.last_was_crc <- true
    | 0xfe -> idam := prev_a1
    | _ -> ()
  end;
  match write_byte t disk t.data_current t.data_out_reg ~idam:!idam with
  | Error () -> fail_not_ready t time
  | Ok () ->
    t.data_current <- t.data_current + 1;
    t.crc <- crc_byte t.crc t.data_out_reg;
    t.data_available <- t.data_available - 1;
    if t.data_available > 0 then begin
      t.drq_time <- time;
      schedule t Write_track (time + byte_ticks);
      if not t.last_was_crc then begin
        if t.data_reg_written then begin
          t.data_out_reg <- t.data_reg;
          t.data_reg_written <- false
        end
        else begin
          t.data_out_reg <- 0;
          t.status <- t.status lor lost_data
        end
      end
      else begin
        t.data_out_reg <- !crc_low;
        t.drq_time <- never
      end
    end
    else
      match flush_selected t disk with
      | Error () -> fail_not_ready t time
      | Ok () -> end_command t time

let start_type3 t disk time =
  t.status <- (t.status land lnot (lost_data lor record_not_found lor record_type)) lor busy;
  if not (inserted t disk) then end_command t time
  else begin
    if t.command land 0xf0 = 0xf0 then t.drq_time <- time;
    t.hld_time <- time;
    if t.command land verify_or_settle <> 0 then schedule t Type3_loaded (time + settle_delay)
    else type3_loaded t disk time
  end

(* Type IV: FORCE INTERRUPT. *)
let start_type4 t disk time =
  let flags = t.command land 0x0f in
  if flags = 0 then t.immediate_irq <- false;
  if flags land irq_on_index <> 0 && inserted t disk then t.irq_time <- index_after t disk time 1;
  if flags land irq_now <> 0 then t.immediate_irq <- true;
  t.drq_time <- never;
  t.status <- t.status land lnot busy

let run t disk ev time =
  match ev with
  | Seek -> if t.command land 0x80 = 0 then
      (if t.command land 0xe0 = 0 then seek t disk time else end_command t time)
  | Type2_loaded -> if t.command land 0xc0 = 0x80 then type2_loaded t disk time
  | Type2_not_found -> if t.command land 0xc0 = 0x80 then type2_not_found t time
  | Type2_rotated -> if t.command land 0xc0 = 0x80 then type2_rotated t disk time
  | Check_write -> if t.command land 0xe0 = 0xa0 then check_start_write t time
  | Pre_write_sector -> if t.command land 0xe0 = 0xa0 then pre_write_sector t disk time
  | Write_sector -> if t.command land 0xe0 = 0xa0 then write_sector_data t disk time
  | Post_write_sector -> if t.command land 0xe0 = 0xa0 then post_write_sector t disk time
  | Type3_loaded ->
    if t.command land 0xc0 = 0xc0 && t.command land 0xf0 <> 0xd0 then type3_loaded t disk time
  | Type3_rotated ->
    if t.command land 0xc0 = 0xc0 && t.command land 0xf0 <> 0xd0 then type3_rotated t disk time
  | Write_track -> if t.command land 0xf0 = 0xf0 then write_track_data t disk time
  | Read_track ->
    if t.command land 0xf0 = 0xe0 then begin
      t.a.valid <- false;
      end_command t time
    end

(* Run what the controller did on its own up to [time] (the scheduler's
   sync before an uncached memory access). *)
let rec catch_up t disk time =
  match t.event with
  | Some (ev, at) when at <= time ->
    t.event <- None;
    run t disk ev at;
    catch_up t disk time
  | _ -> ()

let set_command t disk time v =
  if writing_command t then ignore (flush_selected t disk);
  t.event <- None;
  t.command <- v;
  t.irq_time <- never;
  match v land 0xf0 with
  | 0x00 | 0x10 | 0x20 | 0x30 | 0x40 | 0x50 | 0x60 | 0x70 -> start_type1 t disk time
  | 0x80 | 0x90 | 0xa0 | 0xb0 -> start_type2 t disk time
  | 0xd0 -> start_type4 t disk time
  | _ -> start_type3 t disk time

let status t disk time =
  if t.command land 0x80 = 0 || t.command land 0xf0 = 0xd0 then begin
    t.status <- t.status land lnot (index_bit lor track00 lor head_loaded lor write_protected);
    if index_pulse t disk time then t.status <- t.status lor index_bit;
    if at_track00 t then t.status <- t.status lor track00;
    if head_active t time then t.status <- t.status lor head_loaded;
    if protected t disk then t.status <- t.status lor write_protected
  end
  else
    t.status <- (if drq t time then t.status lor drq_bit else t.status land lnot drq_bit);
  t.status <- (if inserted t disk then t.status land lnot not_ready else t.status lor not_ready);
  if t.irq_time <= time then t.irq_time <- never;
  t.status

let take_byte t disk =
  t.data_reg <- read_byte t disk t.data_current;
  t.data_current <- t.data_current + 1;
  t.crc <- crc_byte t.crc t.data_reg;
  t.data_available <- t.data_available - 1;
  t.drq_time <- t.drq_time + byte_ticks

let data t disk time =
  if reading_command t && drq t time then begin
    take_byte t disk;
    while t.data_available > 0 && drq t time do
      t.status <- t.status lor lost_data;
      take_byte t disk
    done;
    if t.data_available = 0 then begin
      if t.command land 0xe0 = 0x80 then begin
        let hi = read_byte t disk t.data_current in
        let lo = read_byte t disk (t.data_current + 1) in
        t.data_current <- t.data_current + 2;
        t.status <-
          (if (hi lsl 8) lor lo = t.crc then t.status land lnot crc_error
           else t.status lor crc_error);
        if t.found.deleted then t.status <- t.status lor record_type;
        if t.command land multi_sector = 0 then end_command t time
        else begin
          t.drq_time <- never;
          t.sector_reg <- (t.sector_reg + 1) land 0xff;
          type2_loaded t disk time
        end
      end
      else begin
        if t.command land 0xf0 = 0xe0 then t.a.valid <- false;
        if t.command land 0xf0 = 0xc0 then
          t.status <-
            (if t.found.addr_crc_err then t.status lor crc_error
             else t.status land lnot crc_error);
        end_command t time
      end
    end
  end;
  t.data_reg

let set_data t time v =
  t.data_reg <- v;
  if drq t time && writing_command t then begin
    t.data_reg_written <- true;
    t.drq_time <- never
  end

(* DriveMultiplexer and RealDrive::setMotor (NMS 8250: the motor stops as
   soon as it is switched off). *)
let drive_motor t disk time on =
  if on_a t && t.a.motor <> on then set_motor_now t.a disk time on

let select t disk time s =
  if t.selected <> s then begin
    drive_motor t disk time false;
    t.selected <- s;
    if on_a t then begin
      invalidate t.a disk;
      t.a.side <- t.mux_side
    end;
    drive_motor t disk time t.mux_motor
  end

let set_side t disk side =
  t.mux_side <- side;
  if on_a t then begin
    invalidate t.a disk;
    t.a.side <- side
  end

let disk_change_bit = 0x04

let read t ~disk ~time r =
  catch_up t disk time;
  match r land 7 with
  | 0 -> status t disk time
  | 1 -> t.track_reg
  | 2 -> t.sector_reg
  | 3 -> data t disk time
  | 4 -> t.side_reg
  | 5 ->
    let changed = on_a t && t.a.changed in
    if changed then t.a.changed <- false;
    (t.drive_reg land lnot disk_change_bit) lor (if changed then 0 else disk_change_bit)
  | 6 -> 0xff
  | _ ->
    0xff
    land (if irq t time then lnot not_intrq else 0xff)
    land (if drq t time then lnot not_drq else 0xff)

let drive_of_reg v =
  match v land 3 with 0 | 2 -> Drive_a | 1 -> Drive_b | _ -> No_drive

let write t ~disk ~time r v =
  catch_up t disk time;
  let v = v land 0xff in
  match r land 7 with
  | 0 -> set_command t disk time v
  | 1 -> t.track_reg <- v
  | 2 -> t.sector_reg <- v
  | 3 -> set_data t time v
  | 4 ->
    t.side_reg <- v;
    set_side t disk (v land 1)
  | 5 ->
    t.drive_reg <- v;
    select t disk time (drive_of_reg v);
    t.mux_motor <- v land 0x80 <> 0;
    drive_motor t disk time t.mux_motor
  | _ -> ()

let image_written t =
  t.a.valid <- false;
  t.a.dirty <- false

let disk_changed t =
  image_written t;
  t.a.changed <- true

let restore_slowest = 0x03

let create () =
  let t =
    { a =
        { head_pos = 0; side = 0; motor = false; start_angle = 0; motor_base = 0;
          raw = Bytes.make track_length (Char.chr gap); idam = []; valid = false;
          dirty = false; changed = true };
      selected = Drive_a; mux_side = 0; mux_motor = false; side_reg = 0; drive_reg = 0;
      status = 0; command = 0; track_reg = 0; sector_reg = 1; data_reg = 0;
      direction_in = true; drq_time = never; irq_time = never; immediate_irq = false;
      hld_time = never; data_current = 0; data_available = 0; data_reg_written = false;
      data_out_reg = 0; last_was_a1 = false; last_was_crc = false; crc = crc_init;
      pulse5 = never; found = no_sector; event = None }
  in
  set_command t Bytes.empty 0 restore_slowest;
  t

(* --- Save state --- *)

let event_code = function
  | Seek -> 1 | Type2_loaded -> 2 | Type2_not_found -> 3 | Type2_rotated -> 4
  | Check_write -> 5 | Pre_write_sector -> 6 | Write_sector -> 7
  | Post_write_sector -> 8 | Type3_loaded -> 9 | Type3_rotated -> 10
  | Write_track -> 11 | Read_track -> 12

let event_of_code = function
  | 1 -> Seek | 2 -> Type2_loaded | 3 -> Type2_not_found | 4 -> Type2_rotated
  | 5 -> Check_write | 6 -> Pre_write_sector | 7 -> Write_sector
  | 8 -> Post_write_sector | 9 -> Type3_loaded | 10 -> Type3_rotated
  | 11 -> Write_track | 12 -> Read_track
  | n -> State_codec.fail (Printf.sprintf "unknown WD2793 event %d" n)

let select_code = function Drive_a -> 0 | Drive_b -> 1 | No_drive -> 2

let select_of_code = function
  | 0 -> Drive_a | 1 -> Drive_b | 2 -> No_drive
  | n -> State_codec.fail (Printf.sprintf "unknown drive select %d" n)

let write_state w t =
  let module C = State_codec in
  let i = C.put_int w and b = C.put_bool w in
  let d = t.a in
  i d.head_pos; i d.side; b d.motor; i d.start_angle; i d.motor_base;
  C.put_bytes w d.raw;
  i (List.length d.idam); List.iter i d.idam;
  b d.valid; b d.dirty; b d.changed;
  i (select_code t.selected); i t.mux_side; b t.mux_motor; i t.side_reg; i t.drive_reg;
  i t.status; i t.command; i t.track_reg; i t.sector_reg; i t.data_reg; b t.direction_in;
  i t.drq_time; i t.irq_time; b t.immediate_irq; i t.hld_time; i t.data_current;
  i t.data_available; b t.data_reg_written; i t.data_out_reg; b t.last_was_a1;
  b t.last_was_crc; i t.crc; i t.pulse5;
  let s = t.found in
  i s.addr_idx; i s.cylinder; i s.head; i s.number; i s.size_code; b s.addr_crc_err;
  i (match s.data_idx with Some x -> x | None -> -1); b s.deleted;
  match t.event with
  | None -> i 0
  | Some (ev, at) -> i (event_code ev); i at

let read_state r t =
  let module C = State_codec in
  let int ~max = C.get_int r ~min:0 ~max in
  let byte () = int ~max:0xff in
  let bool () = C.get_bool r in
  let time () = int ~max:max_int in
  (* indices run past the track end while a command walks off it *)
  let index () = C.get_int r ~min:min_int ~max:max_int in
  let d = t.a in
  d.head_pos <- int ~max:max_track;
  d.side <- int ~max:1;
  d.motor <- bool ();
  d.start_angle <- int ~max:(ticks_per_rotation - 1);
  d.motor_base <- time ();
  let raw = C.get_bytes r in
  if Bytes.length raw <> track_length then C.fail "WD2793 track is not 6250 bytes";
  Bytes.blit raw 0 d.raw 0 track_length;
  let n = int ~max:track_length in
  d.idam <- List.init n (fun _ -> int ~max:(track_length - 1));
  d.valid <- bool ();
  d.dirty <- bool ();
  d.changed <- bool ();
  t.selected <- select_of_code (int ~max:2);
  t.mux_side <- int ~max:1;
  t.mux_motor <- bool ();
  t.side_reg <- byte ();
  t.drive_reg <- byte ();
  t.status <- byte ();
  t.command <- byte ();
  t.track_reg <- byte ();
  t.sector_reg <- byte ();
  t.data_reg <- byte ();
  t.direction_in <- bool ();
  t.drq_time <- time ();
  t.irq_time <- time ();
  t.immediate_irq <- bool ();
  t.hld_time <- time ();
  t.data_current <- index ();
  t.data_available <- int ~max:track_length;
  t.data_reg_written <- bool ();
  t.data_out_reg <- byte ();
  t.last_was_a1 <- bool ();
  t.last_was_crc <- bool ();
  t.crc <- int ~max:0xffff;
  t.pulse5 <- time ();
  let addr_idx = index () in
  let cylinder = byte () in
  let head = byte () in
  let number = byte () in
  let size_code = byte () in
  let addr_crc_err = bool () in
  let data_idx = match index () with -1 -> None | x -> Some x in
  let deleted = bool () in
  t.found <- { addr_idx; cylinder; head; number; size_code; addr_crc_err; data_idx; deleted };
  t.event <-
    (match int ~max:12 with
     | 0 -> None
     | code ->
       let ev = event_of_code code in
       Some (ev, time ()))
