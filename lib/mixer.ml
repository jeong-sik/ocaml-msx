(* Resampler from chip ticks to 44.1 kHz mono PCM.

   The sound chips run in ticks of 16 CPU cycles: one PSG chip sample, half
   an SCC sample. [ticks] turns CPU cycles into whole ticks; [push] takes the
   summed chip level of one tick and emits an output sample whenever 1/44100 s
   of ticks has gone by; that sample
   is the mean of the tick levels since the previous one (a box filter, as
   openMSX's resampler sees a held level), halved. The halving keeps the PSG
   (0..24573) and an SCC (-16200..16200 in PSG units) inside 16 bits. *)

let cpu_hz = 3_579_545
let cycles_per_tick = 16
let sample_rate = 44_100

(* One tick moves the output phase by this much; an output sample is due
   every [cpu_hz] of phase. Exact: ticks are whole CPU cycles. *)
let phase_step = cycles_per_tick * sample_rate
let output_divisor = 2

type t = {
  (* CPU cycles short of a tick, the output phase, and the tick levels summed
     for the next output sample. *)
  mutable cycle_rest : int;
  mutable phase : int;
  mutable acc : int;
  mutable acc_n : int;
  out : Buffer.t;
}

let create () = { cycle_rest = 0; phase = 0; acc = 0; acc_n = 0; out = Buffer.create 4096 }

(* Rounds towards minus infinity, so negative levels step like positive ones. *)
let floor_div a b = if a >= 0 then a / b else -((-a + b - 1) / b)

let ticks t ~cycles =
  let total = t.cycle_rest + cycles in
  let n = total / cycles_per_tick in
  t.cycle_rest <- total - (n * cycles_per_tick);
  n

let push t level =
  t.acc <- t.acc + level;
  t.acc_n <- t.acc_n + 1;
  t.phase <- t.phase + phase_step;
  if t.phase >= cpu_hz then begin
    t.phase <- t.phase - cpu_hz;
    Buffer.add_int16_le t.out (floor_div t.acc (t.acc_n * output_divisor));
    t.acc <- 0;
    t.acc_n <- 0
  end

let clear_output t = Buffer.clear t.out
let output t = Buffer.contents t.out

let write_state w t =
  State_codec.put_int w t.cycle_rest;
  State_codec.put_int w t.phase;
  State_codec.put_int w t.acc;
  State_codec.put_int w t.acc_n

let read_state r t =
  t.cycle_rest <- State_codec.get_int r ~min:0 ~max:(cycles_per_tick - 1);
  t.phase <- State_codec.get_int r ~min:0 ~max:(cpu_hz - 1);
  t.acc <- State_codec.get_int r ~min:min_int ~max:max_int;
  t.acc_n <- State_codec.get_int r ~min:0 ~max:max_int
