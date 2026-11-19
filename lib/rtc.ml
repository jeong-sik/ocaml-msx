(* RP5C01 real-time clock, written from the behaviour of openMSX 21.0
   RP5C01.cc (EmuTime mode); no code is taken from it.

   The only state is what the chip holds: the 52 register nibbles as last
   written, the mode and test registers, the fraction of a second and the
   last tick already counted into the registers. The clock values (seconds,
   minutes, ...) are read back from the time registers whenever ticks are
   counted: openMSX keeps them in separate fields, but those fields always
   equal what its regs2Time would read from the registers, so a second copy
   would only add a way for the two to disagree. *)

let block_size = 13
let time_block = 0
let alarm_block = 1
let first_ram_block = 2
let blocks = 4

type register = Data of int | Mode | Test | Reset

let register_of_nibble n =
  match n land 0x0f with 13 -> Mode | 14 -> Test | 15 -> Reset | r -> Data r

(* Mode register bits. *)
let block_select = 0x3
let timer_enable = 0x8

(* Test register bits: each makes its counter step once per tick instead of
   on the carry from the counter below it. *)
let test_seconds = 0x1
let test_minutes = 0x2
let test_days = 0x4
let test_years = 0x8

(* Reset register bits. *)
let reset_alarm = 0x1
let reset_fraction = 0x2

(* Alarm block registers. 12-hour mode is register 10 = 0; register 11
   counts years since the last leap year (0 = leap year). *)
let hour_mode_reg = 10
let leap_reg = 11

(* Bits a register keeps; reads return 0 in the others. *)
let masks =
  [| [| 0xf; 0x7; 0xf; 0x7; 0xf; 0x3; 0x7; 0xf; 0x3; 0xf; 0x1; 0xf; 0xf |];
     [| 0x0; 0x0; 0xf; 0x7; 0xf; 0x3; 0x7; 0xf; 0x3; 0x0; 0x1; 0x3; 0x0 |];
     Array.make block_size 0xf;
     Array.make block_size 0xf |]

let tick_hz = 16384
let seconds_per_minute = 60
let minutes_per_hour = 60
let hours_per_day = 24
let days_per_week = 7
let months_per_year = 12
let years_shown = 100
let leap_cycle = 4
let first_year = 1980

(* openMSX counts time in units of 1/(3579545 * 960) s and rounds the tick
   period to a whole number of those units (209739). Using the same rounding
   puts every tick on the same CPU cycle as openMSX; the exact 16384 Hz
   period drifts from it by about 2 ticks a minute. *)
let cpu_hz = 3579545
let units_per_cycle = 960
let units_per_tick = ((cpu_hz * units_per_cycle) + (tick_hz / 2)) / tick_hz
let ticks_at cycle = cycle * units_per_cycle / units_per_tick

type t = {
  regs : int array;  (** raw nibbles, block-major; reads apply [masks] *)
  mutable mode : int;
  mutable test : int;
  mutable fraction : int;  (** ticks into the current second *)
  mutable counted : int;  (** ticks since cycle 0 already in [regs] *)
}

let index block r = (block * block_size) + r
let leap_index = index alarm_block leap_reg

(* openMSX starts the register SRAM at 0xFF when no saved file exists and
   then writes only the time block and the leap-year counter; C-BIOS leaves
   the rest so, and Replicart (C-BIOS) reads block 2 register 3. *)
let create () =
  { regs =
      Array.init (blocks * block_size) (fun i ->
          if i < block_size || i = leap_index then 0 else 0x0f);
    mode = timer_enable;
    test = 0;
    fraction = 0;
    counted = 0 }

(* [day] and [month] count from 0; a register value of 00 reads as -1. *)
type clock = {
  second : int;
  minute : int;
  hour : int;
  weekday : int;
  day : int;
  month : int;
  year : int;
  leap : int;
}

let twelve_hour t = t.regs.(index alarm_block hour_mode_reg) = 0

(* In 12-hour mode the tens digit carries the PM flag: 1 PM reads 21. *)
let pm_offset = 20
let noon = 12

let clock_of_regs t =
  let digit r = t.regs.(index time_block r) in
  let pair r = digit r + (10 * digit (r + 1)) in
  let hour = pair 4 in
  { second = pair 0;
    minute = pair 2;
    hour = (if twelve_hour t && hour >= pm_offset then hour - pm_offset + noon else hour);
    weekday = digit 6;
    day = pair 7 - 1;
    month = pair 9 - 1;
    year = pair 11;
    leap = t.regs.(leap_index) }

let set_regs t c =
  let set r v = t.regs.(index time_block r) <- v in
  let pair r v = set r (v mod 10); set (r + 1) (v / 10) in
  pair 0 c.second;
  pair 2 c.minute;
  pair 4 (if twelve_hour t && c.hour >= noon then c.hour - noon + pm_offset else c.hour);
  set 6 c.weekday;
  pair 7 (c.day + 1);
  pair 9 (c.month + 1);
  pair 11 c.year;
  t.regs.(leap_index) <- c.leap

let month_days = [| 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |]
let february = 1

(* [month] counts from 0 and may lie outside 0-11. A month register of 00
   (month -1) takes December's length here; openMSX reads outside its table
   in that case. *)
let days_in_month month leap =
  let m = ((month mod months_per_year) + months_per_year) mod months_per_year in
  if m = february && leap = 0 then 29 else month_days.(m)

(* Count the ticks since the last access into the time registers. *)
let sync t ~cycle =
  let now = ticks_at cycle in
  let elapsed = now - t.counted in
  t.counted <- now;
  let c = clock_of_regs t in
  let carry flag normal = if t.test land flag <> 0 then elapsed else normal in
  let fraction = t.fraction + (if t.mode land timer_enable <> 0 then elapsed else 0) in
  let second = c.second + carry test_seconds (fraction / tick_hz) in
  let minute = c.minute + carry test_minutes (second / seconds_per_minute) in
  let hour = c.hour + (minute / minutes_per_hour) in
  let days = carry test_days (hour / hours_per_day) in
  (* Months roll over only when a day is carried: setting a date one digit
     at a time passes through dates such as 02-31, which must stay as
     written. The leap counter used is the one before the carry. *)
  let rec roll day month =
    let n = days_in_month month c.leap in
    if day >= n then roll (day - n) (month + 1) else (day, month)
  in
  let day, month = if days = 0 then (c.day, c.month) else roll (c.day + days) c.month in
  let years = carry test_years (month / months_per_year) in
  t.fraction <- fraction mod tick_hz;
  set_regs t
    { second = second mod seconds_per_minute;
      minute = minute mod minutes_per_hour;
      hour = hour mod hours_per_day;
      weekday = (c.weekday + days) mod days_per_week;
      day;
      month = month mod months_per_year;
      year = (c.year + years) mod years_shown;
      leap = (c.leap + years) mod leap_cycle }

let block t = t.mode land block_select
let counts t = block t < first_ram_block

let read t ~cycle reg =
  match register_of_nibble reg with
  | Mode -> t.mode
  | Test | Reset -> 0x0f (* write-only *)
  | Data r ->
    if counts t then sync t ~cycle;
    t.regs.(index (block t) r) land masks.(block t).(r)

let write t ~cycle reg v =
  let v = v land 0x0f in
  match register_of_nibble reg with
  | Mode -> sync t ~cycle; t.mode <- v
  | Test -> sync t ~cycle; t.test <- v
  | Reset ->
    (* No sync first: ticks since the last access still reach the
       fraction at the next one. *)
    if v land reset_alarm <> 0 then
      for r = 2 to 8 do t.regs.(index alarm_block r) <- 0 done;
    if v land reset_fraction <> 0 then t.fraction <- 0
  | Data r ->
    if counts t then sync t ~cycle;
    t.regs.(index (block t) r) <- v land masks.(block t).(r)

type time = {
  t_year : int;
  t_month : int;  (** 1-12 *)
  t_day : int;  (** 1-31 *)
  t_hour : int;
  t_minute : int;
  t_second : int;
}

let range name v lo hi =
  if v < lo || v > hi then Error (Printf.sprintf "%s %d is outside %d-%d" name v lo hi)
  else Ok ()

let time ~year ~month ~day ~hour ~minute ~second =
  let ( let* ) = Result.bind in
  let* () = range "year" year first_year (first_year + years_shown - 1) in
  let* () = range "month" month 1 months_per_year in
  let* () = range "day" day 1 (days_in_month (month - 1) (year mod leap_cycle)) in
  let* () = range "hour" hour 0 (hours_per_day - 1) in
  let* () = range "minute" minute 0 (minutes_per_hour - 1) in
  let* () = range "second" second 0 (seconds_per_minute - 1) in
  Ok { t_year = year; t_month = month; t_day = day; t_hour = hour; t_minute = minute;
       t_second = second }

(* Day of the week, 0 = Sunday, for a Gregorian date (Sakamoto's method). *)
let weekday_of ~year ~month ~day =
  let offsets = [| 0; 3; 2; 5; 0; 3; 5; 1; 4; 6; 2; 4 |] in
  let y = if month < 3 then year - 1 else year in
  (y + (y / 4) - (y / 100) + (y / 400) + offsets.(month - 1) + day) mod days_per_week

let set_time t ~cycle tm =
  t.counted <- ticks_at cycle;
  t.fraction <- 0;
  set_regs t
    { second = tm.t_second;
      minute = tm.t_minute;
      hour = tm.t_hour;
      weekday = weekday_of ~year:tm.t_year ~month:tm.t_month ~day:tm.t_day;
      day = tm.t_day - 1;
      month = tm.t_month - 1;
      year = tm.t_year - first_year;
      leap = tm.t_year mod leap_cycle }

let write_registers w t =
  State_codec.put_int w t.mode;
  State_codec.put_int_array w t.regs

let read_registers r t =
  t.mode <- State_codec.get_int r ~min:0 ~max:15;
  State_codec.fill_int_array r ~min:0 ~max:15 t.regs

let write_clock w t =
  State_codec.put_int w t.test;
  State_codec.put_int w t.fraction;
  State_codec.put_int w t.counted

let read_clock r t ~cycle =
  t.test <- State_codec.get_int r ~min:0 ~max:15;
  t.fraction <- State_codec.get_int r ~min:0 ~max:(tick_hz - 1);
  t.counted <- State_codec.get_int r ~min:0 ~max:(ticks_at cycle)

let start_clock t ~cycle = t.counted <- ticks_at cycle
