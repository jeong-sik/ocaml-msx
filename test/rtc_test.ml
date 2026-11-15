(* RTC (RP5C01) registers at power-on, read through ports 0xB4/0xB5 the
   way C-BIOS games do. openMSX starts the register SRAM at 0xFF and sets
   only the time from the host clock, so the alarm block and the RAM
   blocks 2 and 3 read 0xF. Replicart (C-BIOS) reads block 2 register 3
   and takes a different path on 0. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let m = Msx.create ~machine:{ Msx.ram_kb = 64; vram_kb = 128; roms = [ ""; ""; "" ] }
let mode_reg = 13
let timer_enable = 0x08

let read block r =
  Msx.port_out m 0xb4 mode_reg;
  Msx.port_out m 0xb5 (timer_enable lor block);
  Msx.port_out m 0xb4 r;
  Msx.port_in m 0xb5 land 0x0f

let () =
  for r = 0 to 12 do
    check (Printf.sprintf "block 2 register %d reads 0xF" r) (read 2 r = 0x0f);
    check (Printf.sprintf "block 3 register %d reads 0xF" r) (read 3 r = 0x0f);
    check (Printf.sprintf "time register %d reads 0" r) (read 0 r = 0)
  done;
  (* The alarm block shows its masks; register 10 (24-hour mode) reads 1
     and the leap-year counter (register 11) is part of the time. *)
  let alarm = [ 0x0; 0x0; 0xf; 0x7; 0xf; 0x3; 0x7; 0xf; 0x3; 0x0; 0x1; 0x0; 0x0 ] in
  List.iteri (fun r v -> check (Printf.sprintf "alarm register %d" r) (read 1 r = v)) alarm

(* The clock itself, driven at chosen CPU cycles. openMSX rounds the 16384 Hz
   period to 209739 units of 1/(3579545 * 960) s; [cycle_of_ticks n] is the
   first CPU cycle at which n ticks have passed. *)
let units_per_tick = 209739
let units_per_cycle = 960
let cycle_of_ticks n = ((n * units_per_tick) + units_per_cycle - 1) / units_per_cycle
let ticks_per_second = 16384
let at_seconds s = cycle_of_ticks (s * ticks_per_second)
let at_half_seconds h = cycle_of_ticks (h * ticks_per_second / 2)
let reg_mode = 13
let reg_test = 14
let reg_reset = 15

let time_exn ~year ~month ~day ~hour ~minute ~second =
  match Rtc.time ~year ~month ~day ~hour ~minute ~second with
  | Ok t -> t
  | Error e -> failwith e

(* (year since 1980, month, day, hour digits, minute, second, weekday) as
   the time block shows them. *)
let shown rtc ~cycle =
  let r n = Rtc.read rtc ~cycle n in
  let pair n = r n + (10 * r (n + 1)) in
  (pair 11, pair 9, pair 7, pair 4, pair 2, pair 0, r 6)

let show (y, mo, d, h, mi, s, w) = Printf.sprintf "%02d-%02d-%02d %02d:%02d:%02d w%d" y mo d h mi s w

let expect name got want =
  if got <> want then begin
    incr failures;
    Printf.eprintf "FAIL %s: got %s want %s\n%!" name (show got) (show want)
  end

let started ?(twelve_hour = false) time =
  let rtc = Rtc.create () in
  if twelve_hour then begin
    Rtc.write rtc ~cycle:0 reg_mode (timer_enable lor 1);
    Rtc.write rtc ~cycle:0 10 0;
    Rtc.write rtc ~cycle:0 reg_mode timer_enable
  end;
  Rtc.set_time rtc ~cycle:0 time;
  rtc

let () =
  (* A zero clock: second 0 until the 16384th tick, then 1. *)
  let rtc = Rtc.create () in
  let last = at_seconds 1 - 1 in
  expect "zero clock one cycle before the first second" (shown rtc ~cycle:last) (0, 0, 0, 0, 0, 0, 0);
  expect "zero clock at the first second" (shown rtc ~cycle:(last + 1)) (0, 0, 0, 0, 0, 1, 0);
  expect "zero clock after an hour" (shown rtc ~cycle:(at_seconds 3600)) (0, 0, 0, 1, 0, 0, 0);
  (* Every carry at once: 1999-12-31 (Friday) to 2000-01-01 (Saturday). *)
  let rtc = started (time_exn ~year:1999 ~month:12 ~day:31 ~hour:23 ~minute:59 ~second:59) in
  expect "new year's eve" (shown rtc ~cycle:0) (19, 12, 31, 23, 59, 59, 5);
  expect "new year" (shown rtc ~cycle:(at_seconds 1)) (20, 1, 1, 0, 0, 0, 6);
  Rtc.write rtc ~cycle:(at_seconds 1) reg_mode (timer_enable lor 1);
  check "2000 is a leap year" (Rtc.read rtc ~cycle:(at_seconds 1) 11 = 0);
  let rtc = started (time_exn ~year:2024 ~month:2 ~day:28 ~hour:23 ~minute:59 ~second:59) in
  expect "leap February" (shown rtc ~cycle:(at_seconds 1)) (44, 2, 29, 0, 0, 0, 4);
  let rtc = started (time_exn ~year:2023 ~month:2 ~day:28 ~hour:23 ~minute:59 ~second:59) in
  expect "plain February" (shown rtc ~cycle:(at_seconds 1)) (43, 3, 1, 0, 0, 0, 3);
  check "February 29 2023 is rejected"
    (Rtc.time ~year:2023 ~month:2 ~day:29 ~hour:0 ~minute:0 ~second:0
     = Error "day 29 is outside 1-28");
  check "1979 is rejected"
    (Result.is_error (Rtc.time ~year:1979 ~month:1 ~day:1 ~hour:0 ~minute:0 ~second:0));
  (* 12-hour mode: PM hours read 20 + (hour - 12). *)
  let rtc = started ~twelve_hour:true (time_exn ~year:2026 ~month:11 ~day:15 ~hour:13 ~minute:59 ~second:59) in
  expect "1 PM in 12-hour mode" (shown rtc ~cycle:0) (46, 11, 15, 21, 59, 59, 0);
  expect "2 PM in 12-hour mode" (shown rtc ~cycle:(at_seconds 1)) (46, 11, 15, 22, 0, 0, 0);
  let rtc = started ~twelve_hour:true (time_exn ~year:2026 ~month:11 ~day:15 ~hour:11 ~minute:59 ~second:59) in
  expect "11 AM in 12-hour mode" (shown rtc ~cycle:0) (46, 11, 15, 11, 59, 59, 0);
  expect "noon in 12-hour mode" (shown rtc ~cycle:(at_seconds 1)) (46, 11, 15, 20, 0, 0, 0);
  (* TEST bit 0: the seconds step once per tick. *)
  let rtc = Rtc.create () in
  Rtc.write rtc ~cycle:0 reg_test 0x1;
  expect "test seconds after 10 ticks" (shown rtc ~cycle:(cycle_of_ticks 10)) (0, 0, 0, 0, 0, 10, 0);
  expect "test seconds after 70 ticks" (shown rtc ~cycle:(cycle_of_ticks 70)) (0, 0, 0, 0, 1, 10, 0);
  (* TEST bit 2: the days step once per tick, and so does the weekday. *)
  let rtc = started (time_exn ~year:2026 ~month:1 ~day:30 ~hour:0 ~minute:0 ~second:0) in
  Rtc.write rtc ~cycle:0 reg_test 0x4;
  expect "test days after 3 ticks" (shown rtc ~cycle:(cycle_of_ticks 3)) (46, 2, 2, 0, 0, 0, 1);
  (* RESET bit 1 clears the fraction counted so far. *)
  let rtc = Rtc.create () in
  ignore (Rtc.read rtc ~cycle:(at_half_seconds 1) 0);
  Rtc.write rtc ~cycle:(at_half_seconds 1) reg_reset 0x2;
  expect "fraction reset at 0.5 s, read at 1.0 s" (shown rtc ~cycle:(at_seconds 1)) (0, 0, 0, 0, 0, 0, 0);
  expect "fraction reset at 0.5 s, read at 1.5 s" (shown rtc ~cycle:(at_half_seconds 3)) (0, 0, 0, 0, 0, 1, 0);
  (* The reset does not count the ticks since the last access first. *)
  let rtc = Rtc.create () in
  ignore (Rtc.read rtc ~cycle:0 0);
  Rtc.write rtc ~cycle:(at_half_seconds 1) reg_reset 0x2;
  expect "reset without an access before it" (shown rtc ~cycle:(at_seconds 1)) (0, 0, 0, 0, 0, 1, 0);
  (* RESET bit 0 clears alarm registers 2-8. *)
  let rtc = Rtc.create () in
  Rtc.write rtc ~cycle:0 reg_mode (timer_enable lor 1);
  Rtc.write rtc ~cycle:0 reg_reset 0x1;
  check "alarm reset clears registers 2-8"
    (List.for_all (fun r -> Rtc.read rtc ~cycle:0 r = 0) [ 2; 3; 4; 5; 6; 7; 8 ]
     && Rtc.read rtc ~cycle:0 10 = 1);
  (* With the timer off the ticks are dropped, not saved for later. *)
  let rtc = Rtc.create () in
  Rtc.write rtc ~cycle:0 reg_mode 0;
  expect "timer off" (shown rtc ~cycle:(at_seconds 2)) (0, 0, 0, 0, 0, 0, 0);
  Rtc.write rtc ~cycle:(at_seconds 2) reg_mode timer_enable;
  expect "timer on again, 0.5 s later" (shown rtc ~cycle:(at_half_seconds 5)) (0, 0, 0, 0, 0, 0, 0);
  expect "timer on again, 1 s later" (shown rtc ~cycle:(at_seconds 3)) (0, 0, 0, 0, 0, 1, 0);
  (* Writing 07-31 to 12-31 one digit at a time passes through 02-31,
     which must not roll over into March. *)
  let rtc = started (time_exn ~year:2026 ~month:7 ~day:31 ~hour:12 ~minute:0 ~second:0) in
  Rtc.write rtc ~cycle:0 9 2;
  expect "02-31 on the way" (shown rtc ~cycle:0) (46, 2, 31, 12, 0, 0, 5);
  Rtc.write rtc ~cycle:0 10 1;
  expect "12-31" (shown rtc ~cycle:0) (46, 12, 31, 12, 0, 0, 5);
  (* Setting the time mid-second starts a whole second from there. *)
  let rtc = Rtc.create () in
  ignore (Rtc.read rtc ~cycle:(at_half_seconds 1) 0);
  Rtc.set_time rtc ~cycle:(at_half_seconds 1) (time_exn ~year:2026 ~month:11 ~day:15 ~hour:0 ~minute:0 ~second:0);
  expect "set at 0.5 s, read at 1.0 s" (shown rtc ~cycle:(at_seconds 1)) (46, 11, 15, 0, 0, 0, 0);
  expect "set at 0.5 s, read at 1.5 s" (shown rtc ~cycle:(at_half_seconds 3)) (46, 11, 15, 0, 0, 1, 0);
  (* A month register of 00 rolls with December's length. *)
  let rtc = Rtc.create () in
  expect "zero date after a day" (shown rtc ~cycle:(at_seconds 86400)) (0, 0, 1, 0, 0, 0, 1);
  (* Save mid-second and restore: the clock goes on as if never saved. *)
  let saved_at = at_half_seconds 3 in
  let rtc = started (time_exn ~year:2026 ~month:11 ~day:15 ~hour:10 ~minute:20 ~second:30) in
  ignore (Rtc.read rtc ~cycle:saved_at 0);
  let w = State_codec.writer () in
  Rtc.write_registers w rtc;
  Rtc.write_clock w rtc;
  let state = State_codec.finish w in
  let restore ~cycle =
    let r = State_codec.reader state in
    let copy = Rtc.create () in
    Rtc.read_registers r copy;
    Rtc.read_clock r copy ~cycle;
    copy
  in
  (* The CPU ran on after the last access; the restore gets that later time
     but must still count from the saved tick. *)
  let copy = restore ~cycle:(cycle_of_ticks (ticks_per_second * 19 / 10)) in
  expect "restored clock at 2.0 s" (shown copy ~cycle:(at_seconds 2)) (shown rtc ~cycle:(at_seconds 2));
  expect "restored clock at 2.5 s" (shown copy ~cycle:(at_half_seconds 5)) (shown rtc ~cycle:(at_half_seconds 5));
  check "a saved tick after the CPU time is rejected"
    (match restore ~cycle:0 with
     | exception State_codec.Invalid_state _ -> true
     | _ -> false)

(* Through the machine: the RTC counts the cycles the CPU runs, and a save
   state carries the clock. 59 frames are 3524424 cycles (second 30), 60
   frames 3584160 (second 31). *)
let () =
  let machine () =
    let m = Msx.create ~machine:{ Msx.ram_kb = 64; vram_kb = 128; roms = [ ""; ""; "" ] } in
    Msx.set_rtc_time m (time_exn ~year:2026 ~month:11 ~day:15 ~hour:10 ~minute:20 ~second:30);
    m
  in
  let seconds m =
    Msx.port_out m 0xb4 0;
    let ones = Msx.port_in m 0xb5 land 0x0f in
    Msx.port_out m 0xb4 1;
    ones + (10 * (Msx.port_in m 0xb5 land 0x0f))
  in
  let m = machine () in
  Msx.step m ~frames:59;
  check "machine clock after 59 frames" (seconds m = 30);
  let state = Msx.serialize m in
  check "save state is format 4" (String.sub state 0 11 = "OCAML-MSX\000\004");
  Msx.step m ~frames:1;
  check "machine clock after 60 frames" (seconds m = 31);
  match Msx.restore ~state with
  | Error e -> failwith ("restore: " ^ e)
  | Ok restored ->
    Msx.step restored ~frames:1;
    check "restored machine clock after 60 frames" (seconds restored = 31);
    check "restored machine serializes like the original"
      (Msx.serialize restored = Msx.serialize m)

let () =
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "rtc_test: all pass"
