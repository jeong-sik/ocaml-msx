(* Konami SCC, the sound chip inside Konami's SCC MegaROM carts: five
   channels, each playing a 32-byte signed waveform. The model follows openMSX
   src/sound/SCC.cc in Real mode (the MegaROM chip, not the SCC-I).

   - Registers, [addr land 0xFF] of the CPU window 0x9800-0x9FFF:
     0x00-0x7F waveforms of channels 1-4 (channel 5 plays channel 4's);
     0x80-0x9F periods (12 bits, low byte first), volumes (4 bits) and the
     enable mask, the 16-byte block shown twice; 0xE0-0xFF the deformation
     register. Reads give the waveform below 0x80 and 0xFF above; a read at
     0xE0 or above writes 0xFF to the deformation register.
   - Timing: one sample per 32 CPU cycles. A channel counts 32 per sample and
     moves to the next waveform byte every period + 1 counts, so it plays
     3579545 / (32 * (period + 1)) Hz. A period of 8 or less stops it.
   - Level: a byte plays as (sample * volume) asr 4, and a new volume shows
     from the next byte. A disabled channel plays 0.
   - Deformation: bit 0 keeps period bits 8-11 only, bit 1 bits 0-7 only;
     bit 5 restarts the waveform on a period write; bits 6-7 make waveforms
     read-only and rotate what the CPU reads back. *)

let channels = 5
let wave_len = 32
let ticks_per_sample = 2 (* a sample lasts two 16-cycle mixer ticks *)

(* Below this period the counter does not run (SCC::setFreqVol). *)
let min_running_period = 9
let count_step = 32

type t = {
  wave : int array array;
  adjusted : int array array;  (** [(wave * volume) asr 4] *)
  org_period : int array;  (** the period as written *)
  period : int array;  (** after the deformation bits 0-1 *)
  volume : int array;
  incr : int array;
  count : int array;
  pos : int array;
  out : int array;
  mutable enable : int;
  mutable deform : int;
  rotate : bool array;
  read_only : bool array;
  mutable deform_cycles : int;
      (** CPU cycles since the deformation timer last restarted, counted in
          mixer ticks; it sets how far a rotating waveform has turned. *)
  mutable tick_in_sample : int;
  mutable level : int;
}

let adjust sample volume = (sample * volume) asr 4
let signed_byte v = if v land 0x80 <> 0 then (v land 0xff) - 0x100 else v land 0xff

let set_volume t ch v =
  t.volume.(ch) <- v land 0x0f;
  for p = 0 to wave_len - 1 do
    t.adjusted.(ch).(p) <- adjust t.wave.(ch).(p) t.volume.(ch)
  done

let effective_period t per =
  if t.deform land 0x02 <> 0 then per land 0xff
  else if t.deform land 0x01 <> 0 then per lsr 8
  else per

let set_period t ch per =
  t.org_period.(ch) <- per;
  let per = effective_period t per in
  t.period.(ch) <- per;
  t.incr.(ch) <- (if per < min_running_period then 0 else count_step);
  t.count.(ch) <- 0;
  if t.deform land 0x20 <> 0 then begin
    t.pos.(ch) <- 0;
    t.deform_cycles <- 0
  end;
  t.out.(ch) <- t.adjusted.(ch).(t.pos.(ch))

(* SCC::setFreqVol: the 16-byte block at 0x80 and 0x90. *)
let set_freq_vol t a v =
  let a = a land 0x0f in
  if a < 0x0a then begin
    let ch = a / 2 and org = t.org_period.(a / 2) in
    let per = if a land 1 = 1 then ((v land 0x0f) lsl 8) lor (org land 0xff) else (org land 0xf00) lor v in
    set_period t ch per
  end
  else if a < 0x0f then set_volume t (a - 0x0a) v
  else t.enable <- v

let set_deform_mode t v =
  t.deform <- v;
  let set lo hi ~rotate ~read_only =
    for ch = lo to hi do
      t.rotate.(ch) <- rotate;
      t.read_only.(ch) <- read_only
    done
  in
  match v land 0xc0 with
  | 0x00 -> set 0 4 ~rotate:false ~read_only:false
  | 0x40 -> set 0 4 ~rotate:true ~read_only:true
  | 0x80 ->
    set 0 2 ~rotate:false ~read_only:false;
    set 3 4 ~rotate:true ~read_only:true
  | _ ->
    set 0 2 ~rotate:true ~read_only:true;
    set 3 4 ~rotate:false ~read_only:true

let set_deform t v =
  if v <> t.deform then begin
    t.deform_cycles <- 0;
    set_deform_mode t v
  end

let write_wave t ch a v =
  if not t.read_only.(ch) then begin
    let p = a land (wave_len - 1) and s = signed_byte v in
    t.wave.(ch).(p) <- s;
    t.adjusted.(ch).(p) <- adjust s t.volume.(ch);
    if ch = 3 then begin
      t.wave.(4).(p) <- s;
      t.adjusted.(4).(p) <- adjust s t.volume.(4)
    end
  end

let write t a v =
  let a = a land 0xff and v = v land 0xff in
  if a < 0x80 then write_wave t (a lsr 5) a v
  else if a < 0xa0 then set_freq_vol t a v
  else if a >= 0xe0 then set_deform t v

let read_wave t ch a =
  let shift =
    if not t.rotate.(ch) then 0
    else
      let period_ch = if ch = 3 && t.deform land 0xc0 = 0x40 then 4 else ch in
      t.deform_cycles / (t.period.(period_ch) + 1)
  in
  t.wave.(ch).((a + shift) land (wave_len - 1)) land 0xff

let read t a =
  let a = a land 0xff in
  if a >= 0xe0 then set_deform t 0xff;
  if a < 0x80 then read_wave t (a lsr 5) a else 0xff

(* SCC::powerUp: reset, waveforms 0xFF, volumes 15, periods 0. *)
let power_up t =
  set_deform_mode t 0;
  t.enable <- 0;
  Array.iter (fun w -> Array.fill w 0 wave_len (-1)) t.wave;
  for ch = 0 to channels - 1 do set_volume t ch 15 done;
  Array.fill t.pos 0 channels 0;
  for ch = 0 to channels - 1 do set_period t ch 0 done;
  t.deform_cycles <- 0;
  t.tick_in_sample <- 0;
  t.level <- 0

let create () =
  let t =
    {
      wave = Array.init channels (fun _ -> Array.make wave_len 0);
      adjusted = Array.init channels (fun _ -> Array.make wave_len 0);
      org_period = Array.make channels 0;
      period = Array.make channels 0;
      volume = Array.make channels 0;
      incr = Array.make channels 0;
      count = Array.make channels 0;
      pos = Array.make channels 0;
      out = Array.make channels 0;
      enable = 0;
      deform = 0;
      rotate = Array.make channels false;
      read_only = Array.make channels false;
      deform_cycles = 0;
      tick_in_sample = 0;
      level = 0;
    }
  in
  power_up t;
  t

(* SCC::generateChannels for one sample. *)
let sample t =
  let sum = ref 0 in
  for ch = 0 to channels - 1 do
    let span = t.period.(ch) + 1 in
    if t.enable land (1 lsl ch) <> 0 && (t.volume.(ch) <> 0 || t.out.(ch) <> 0) then begin
      sum := !sum + t.out.(ch);
      t.count.(ch) <- t.count.(ch) + t.incr.(ch);
      while t.count.(ch) >= span do
        t.count.(ch) <- t.count.(ch) - span;
        t.pos.(ch) <- (t.pos.(ch) + 1) land (wave_len - 1);
        t.out.(ch) <- t.adjusted.(ch).(t.pos.(ch))
      done
    end
    else begin
      let c = t.count.(ch) + t.incr.(ch) in
      t.count.(ch) <- c mod span;
      t.pos.(ch) <- (t.pos.(ch) + (c / span)) land (wave_len - 1);
      t.out.(ch) <- 0
    end
  done;
  !sum

let tick t =
  t.deform_cycles <- t.deform_cycles + Mixer.cycles_per_tick;
  if t.tick_in_sample = 0 then t.level <- sample t;
  t.tick_in_sample <- (t.tick_in_sample + 1) mod ticks_per_sample;
  t.level

let write_state w t =
  Array.iter (Array.iter (State_codec.put_int w)) t.wave;
  State_codec.put_int_array w t.org_period;
  State_codec.put_int_array w t.period;
  State_codec.put_int_array w t.volume;
  State_codec.put_int w t.enable;
  State_codec.put_int w t.deform;
  State_codec.put_int_array w t.count;
  State_codec.put_int_array w t.pos;
  State_codec.put_int_array w t.out;
  State_codec.put_int w t.deform_cycles;
  State_codec.put_int w t.tick_in_sample;
  State_codec.put_int w t.level

(* The period in use is saved as well as the written one: a later change of
   deformation bits 0-1 does not recompute it (SCC::setDeformReg). The
   adjusted waves, step sizes and rotation flags are rebuilt. *)
let read_state r t =
  Array.iter (fun w -> Array.iteri (fun p _ -> w.(p) <- State_codec.get_int r ~min:(-128) ~max:127) w) t.wave;
  State_codec.fill_int_array r ~min:0 ~max:0xfff t.org_period;
  State_codec.fill_int_array r ~min:0 ~max:0xfff t.period;
  State_codec.fill_int_array r ~min:0 ~max:15 t.volume;
  t.enable <- State_codec.get_int r ~min:0 ~max:0xff;
  set_deform_mode t (State_codec.get_int r ~min:0 ~max:0xff);
  for ch = 0 to channels - 1 do
    set_volume t ch t.volume.(ch);
    t.incr.(ch) <- (if t.period.(ch) < min_running_period then 0 else count_step)
  done;
  for ch = 0 to channels - 1 do
    t.count.(ch) <- State_codec.get_int r ~min:0 ~max:t.period.(ch)
  done;
  State_codec.fill_int_array r ~min:0 ~max:(wave_len - 1) t.pos;
  State_codec.fill_int_array r ~min:(-128) ~max:127 t.out;
  t.deform_cycles <- State_codec.get_int r ~min:0 ~max:max_int;
  t.tick_in_sample <- State_codec.get_int r ~min:0 ~max:(ticks_per_sample - 1);
  let bound = channels * 128 in
  t.level <- State_codec.get_int r ~min:(-bound) ~max:bound
