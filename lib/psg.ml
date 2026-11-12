(* AY-3-8910 PSG: three square-wave tones, one noise source and one
   envelope, mixed to mono 16-bit PCM at 44.1 kHz.

   The model follows openMSX src/sound/AY8910.cc. The chip runs at half the
   CPU clock and produces one internal sample every 8 of its clocks, so one
   "chip sample" per 16 CPU cycles (223.7 kHz).
   - Tone (ToneGenerator): a counter counts chip samples up to the period
     (R#0-R#5, 12 bits, 0 counts as 1) and flips the output.
   - Noise (NoiseGenerator): a 17-bit LFSR, stepped every 2 * R#6 chip
     samples (5 bits, 0 counts as 1); its bit 0 is the noise output.
   - Envelope (Envelope): 32 steps, one every R#11/R#12 chip samples
     (0 counts as 1); the AY repeats each of its 16 levels twice.
   - Mixer (generateChannels): a channel sounds while
     (tone or tone disabled) and (noise or noise disabled), so with both
     disabled the volume register alone sets a constant level.
   - Volume (volumeTab): 3 dB per step; level 0 is silence.

   Output: each 44.1 kHz sample is the mean of the chip samples since the
   previous one. Levels are unsigned (silence is 0, three channels at
   level 15 make 24573); removing the DC offset is the client's job. *)

let cpu_hz = 3_579_545
let cycles_per_chip_sample = 16
let sample_rate = 44_100

(* One chip sample moves the output phase by this much; an output sample is
   due every [cpu_hz] of phase. Exact: chip samples are 16 CPU cycles. *)
let phase_step = cycles_per_chip_sample * sample_rate

(* 2^(-(15 - i) / 2) scaled to 8191: 3 dB per level, three channels fit in
   16 bits. *)
let volume = [| 0; 64; 90; 128; 181; 256; 362; 512; 724; 1024; 1448; 2048; 2896; 4096; 5792; 8191 |]

type tone = { mutable period : int; mutable count : int; mutable high : bool }

type t = {
  regs : int array;
  tones : tone array;
  mutable noise_period : int;
  mutable noise_count : int;
  mutable random : int;
  (* Envelope: [env_period] and [env_count] are in half steps, as openMSX
     keeps them: the count gains 2 per chip sample. *)
  mutable env_period : int;
  mutable env_count : int;
  mutable env_step : int;
  mutable env_attack : int;
  mutable env_hold : bool;
  mutable env_alternate : bool;
  mutable env_holding : bool;
  (* Resampler: CPU cycles short of a chip sample, the output phase, and the
     chip samples summed for the next output sample. *)
  mutable cycle_rest : int;
  mutable phase : int;
  mutable acc : int;
  mutable acc_n : int;
  out : Buffer.t;
}

let tone_period t ch = max 1 (t.regs.(2 * ch) + (256 * (t.regs.((2 * ch) + 1) land 0x0f)))
let noise_period t = 2 * max 1 (t.regs.(6) land 0x1f)
let env_period t = 2 * max 1 (t.regs.(11) + (256 * t.regs.(12)))

(* A period write keeps the count below the new period (openMSX
   Generator::setPeriod: the chip counts up until count >= period). *)
let set_tone_period t ch =
  let tone = t.tones.(ch) in
  tone.period <- tone_period t ch;
  tone.count <- min tone.count (tone.period - 1)

let set_noise_period t =
  t.noise_period <- noise_period t;
  t.noise_count <- min t.noise_count (t.noise_period - 1)

let set_env_period t =
  t.env_period <- env_period t;
  t.env_count <- min t.env_count (t.env_period - 1)

(* Envelope::setShape. Continue = 0 maps to the equivalent Continue = 1
   shape. The step restarts; the period counter keeps running. *)
let set_shape t v =
  t.env_attack <- (if v land 0x04 <> 0 then 0x1f else 0);
  if v land 0x08 = 0 then begin
    t.env_hold <- true;
    t.env_alternate <- t.env_attack <> 0
  end
  else begin
    t.env_hold <- v land 0x01 <> 0;
    t.env_alternate <- v land 0x02 <> 0
  end;
  t.env_step <- 0x1f;
  t.env_holding <- false

let create () =
  let t =
    {
      regs = Array.make 16 0;
      tones = Array.init 3 (fun _ -> { period = 1; count = 0; high = false });
      noise_period = 2;
      noise_count = 0;
      random = 1;
      env_period = 2;
      env_count = 0;
      env_step = 0x1f;
      env_attack = 0;
      env_hold = true;
      env_alternate = false;
      env_holding = false;
      cycle_rest = 0;
      phase = 0;
      acc = 0;
      acc_n = 0;
      out = Buffer.create 4096;
    }
  in
  set_shape t 0;
  t

let reg t r = t.regs.(r)

let write t r v =
  let v = v land 0xff in
  t.regs.(r) <- v;
  match r with
  | 0 | 1 | 2 | 3 | 4 | 5 -> set_tone_period t (r / 2)
  | 6 -> set_noise_period t
  | 11 | 12 -> set_env_period t
  | 13 -> set_shape t v
  | 7 | 8 | 9 | 10 | 14 | 15 -> ()
  | _ -> invalid_arg "Psg.write: register out of range"

(* Envelope::doSteps. *)
let env_steps t steps =
  if not t.env_holding then begin
    t.env_step <- t.env_step - steps;
    if t.env_step < 0 then
      if t.env_hold then begin
        if t.env_alternate then t.env_attack <- t.env_attack lxor 0x1f;
        t.env_holding <- true;
        t.env_step <- 0
      end
      else begin
        (* An odd number of wraps inverts the direction. *)
        if t.env_alternate && t.env_step land 0x10 <> 0 then
          t.env_attack <- t.env_attack lxor 0x1f;
        t.env_step <- t.env_step land 0x1f
      end
  end

(* The level the four generators give now, then one chip sample on. *)
let chip_sample t =
  let mixer = t.regs.(7) in
  let noise_on = t.random land 1 = 1 in
  let env_level = volume.((t.env_step lxor t.env_attack) lsr 1) in
  let level = ref 0 in
  for ch = 0 to 2 do
    let tone_off = mixer land (0x01 lsl ch) <> 0
    and noise_off = mixer land (0x08 lsl ch) <> 0 in
    if (t.tones.(ch).high || tone_off) && (noise_on || noise_off) then begin
      let a = t.regs.(8 + ch) in
      level := !level + if a land 0x10 <> 0 then env_level else volume.(a land 0x0f)
    end
  done;
  for ch = 0 to 2 do
    let tone = t.tones.(ch) in
    tone.count <- tone.count + 1;
    if tone.count >= tone.period then begin
      tone.count <- 0;
      tone.high <- not tone.high
    end
  done;
  t.noise_count <- t.noise_count + 1;
  if t.noise_count >= t.noise_period then begin
    t.noise_count <- 0;
    let b = t.random land 1 in
    t.random <- (t.random lsr 1) lxor (b lsl 13) lxor (b lsl 16)
  end;
  t.env_count <- t.env_count + 2;
  if t.env_count >= t.env_period then begin
    let steps = t.env_count / t.env_period in
    t.env_count <- t.env_count - (steps * t.env_period);
    env_steps t steps
  end;
  !level

let advance t ~cycles =
  let total = t.cycle_rest + cycles in
  let n = total / cycles_per_chip_sample in
  t.cycle_rest <- total - (n * cycles_per_chip_sample);
  for _ = 1 to n do
    t.acc <- t.acc + chip_sample t;
    t.acc_n <- t.acc_n + 1;
    t.phase <- t.phase + phase_step;
    if t.phase >= cpu_hz then begin
      t.phase <- t.phase - cpu_hz;
      Buffer.add_int16_le t.out (t.acc / t.acc_n);
      t.acc <- 0;
      t.acc_n <- 0
    end
  done

let clear_output t = Buffer.clear t.out
let output t = Buffer.contents t.out

let write_regs w t = State_codec.put_int_array w t.regs

let read_regs r t =
  State_codec.fill_int_array r ~min:0 ~max:255 t.regs;
  for ch = 0 to 2 do set_tone_period t ch done;
  set_noise_period t;
  set_env_period t;
  set_shape t t.regs.(13)

let write_state w t =
  Array.iter
    (fun tone ->
      State_codec.put_int w tone.count;
      State_codec.put_bool w tone.high)
    t.tones;
  State_codec.put_int w t.noise_count;
  State_codec.put_int w t.random;
  State_codec.put_int w t.env_count;
  State_codec.put_int w t.env_step;
  State_codec.put_int w t.env_attack;
  State_codec.put_bool w t.env_hold;
  State_codec.put_bool w t.env_alternate;
  State_codec.put_bool w t.env_holding;
  State_codec.put_int w t.cycle_rest;
  State_codec.put_int w t.phase;
  State_codec.put_int w t.acc;
  State_codec.put_int w t.acc_n

(* Counters are read back below their periods, which [read_regs] has set
   from the registers. *)
let read_state r t =
  Array.iter
    (fun tone ->
      tone.count <- State_codec.get_int r ~min:0 ~max:(tone.period - 1);
      tone.high <- State_codec.get_bool r)
    t.tones;
  t.noise_count <- State_codec.get_int r ~min:0 ~max:(t.noise_period - 1);
  t.random <- State_codec.get_int r ~min:1 ~max:0x1ffff;
  t.env_count <- State_codec.get_int r ~min:0 ~max:(t.env_period - 1);
  t.env_step <- State_codec.get_int r ~min:0 ~max:0x1f;
  let attack = State_codec.get_int r ~min:0 ~max:0x1f in
  if attack <> 0 && attack <> 0x1f then State_codec.fail "invalid PSG envelope attack";
  t.env_attack <- attack;
  t.env_hold <- State_codec.get_bool r;
  t.env_alternate <- State_codec.get_bool r;
  t.env_holding <- State_codec.get_bool r;
  t.cycle_rest <- State_codec.get_int r ~min:0 ~max:(cycles_per_chip_sample - 1);
  t.phase <- State_codec.get_int r ~min:0 ~max:(cpu_hz - 1);
  t.acc <- State_codec.get_int r ~min:0 ~max:max_int;
  t.acc_n <- State_codec.get_int r ~min:0 ~max:max_int
