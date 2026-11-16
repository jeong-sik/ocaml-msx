(* Resampler from chip ticks to 44.1 kHz mono PCM.

   The sound chips run in ticks of 16 CPU cycles: one PSG chip sample, half
   an SCC sample. [ticks] turns CPU cycles into whole ticks; [push] takes the
   summed chip level of one tick, and an output sample falls due whenever
   1/44100 s of ticks has gone by.

   The output follows openMSX's sound path (src/sound/ResampleHQ.cc,
   MSXMixer.cc). The model is ours, built from the same two stages; no
   coefficients are copied.
   - Band limit: a windowed-sinc low-pass reads the tick levels around the
     output instant. A box average of the ticks keeps too little of
     10-22 kHz, and noise effects lose their edge.
   - DC removal: y(n) = x(n) - x(n-1) + R * y(n-1) with R = 511/512, a
     high-pass near 14 Hz. A held level fades to silence, as it does through
     the coupling capacitor of real hardware.

   The output is halved after both stages. That keeps the PSG (0..24573) and
   an SCC (-16200..16200 in PSG units) inside 16 bits.

   Integers carry the whole path. Coefficients are Q16 and computed once, so
   a run gives the same samples on every host and after a restore. *)

let cpu_hz = 3_579_545
let cycles_per_tick = 16
let sample_rate = 44_100

(* One tick moves the output phase by this much; an output sample is due
   every [cpu_hz] of phase. Exact: ticks are whole CPU cycles. *)
let phase_step = cycles_per_tick * sample_rate

(* openMSX's "hq" resampler keeps 80% of the output band. *)
let cutoff_hz = 17_640.

(* Half the kernel span, in output periods: 24 periods are 121.75 ticks. *)
let half_width_outputs = 24.
let kaiser_beta = 8.

(* Ticks the kernel reads per output sample: 2 * ceil(121.75). *)
let taps = 244

(* Fractional positions of the output instant inside a tick. *)
let phases = 256
let coef_bits = 16
let coef_one = 1 lsl coef_bits

(* R = 1 - 2^-9 = 511/512. *)
let dc_shift = 9

(* Q16 back to levels, then the halving. *)
let output_shift = coef_bits + 1
let sample_min = -32768
let sample_max = 32767

(* Bound on a tick level, so a restored history cannot overflow the sum:
   taps * coef_one * level_bound stays below 2^54. *)
let level_bound = 1 lsl 30
let sum_bound = 1 lsl 48
let dc_bound = 1 lsl 50

let ticks_per_output = float cpu_hz /. float phase_step
let half_width_ticks = half_width_outputs *. ticks_per_output

(* Modified Bessel function I0 by its power series. 32 terms reach double
   precision for arguments up to [kaiser_beta]. *)
let bessel_i0 x =
  let sum = ref 1. and term = ref 1. in
  for k = 1 to 32 do
    let f = x /. (2. *. float k) in
    term := !term *. f *. f;
    sum := !sum +. !term
  done;
  !sum

(* The kernel at [d] ticks from the output instant. *)
let kernel d =
  let x = d /. half_width_ticks in
  if Float.abs x >= 1. then 0.
  else
    let fc = cutoff_hz *. float cycles_per_tick /. float cpu_hz in
    let a = 2. *. fc *. d in
    let sinc = if a = 0. then 1. else sin (Float.pi *. a) /. (Float.pi *. a) in
    let window = bessel_i0 (kaiser_beta *. sqrt (1. -. (x *. x))) /. bessel_i0 kaiser_beta in
    2. *. fc *. sinc *. window

(* Coefficients by phase, [phases * taps] long. Tap m of a phase weighs the
   (m + 1)-th oldest of the last [taps] ticks; a tick's level stands at the
   middle of its 16 cycles. Each phase sums to exactly [coef_one], so a held
   level passes unchanged: rounding leftovers go to the largest tap. *)
let table =
  let t = Array.make (phases * taps) 0 in
  for ph = 0 to phases - 1 do
    let frac = 0.5 -. ((float ph +. 0.5) /. float phases) in
    let centre = float (taps - 1) /. 2. in
    let w = Array.init taps (fun m -> kernel (centre -. float m +. frac)) in
    let total = Array.fold_left ( +. ) 0. w in
    let q = Array.map (fun c -> Float.to_int (Float.round (c /. total *. float coef_one))) w in
    let rest = coef_one - Array.fold_left ( + ) 0 q in
    let top = ref 0 in
    Array.iteri (fun m c -> if c > q.(!top) then top := m) q;
    q.(!top) <- q.(!top) + rest;
    Array.blit q 0 t (ph * taps) taps
  done;
  t

type t = {
  (* CPU cycles short of a tick, and the output phase. *)
  mutable cycle_rest : int;
  mutable phase : int;
  (* The last [taps] tick levels, stored twice so the kernel reads them in
     one run: the oldest is at [next], the newest at [next + taps - 1]. *)
  history : int array;
  mutable next : int;
  (* The previous band-limited value and the DC filter output, both Q16. *)
  mutable band_prev : int;
  mutable dc : int;
  out : Buffer.t;
}

let create () =
  {
    cycle_rest = 0;
    phase = 0;
    history = Array.make (2 * taps) 0;
    next = 0;
    band_prev = 0;
    dc = 0;
    out = Buffer.create 4096;
  }

let ticks t ~cycles =
  let total = t.cycle_rest + cycles in
  let n = total / cycles_per_tick in
  t.cycle_rest <- total - (n * cycles_per_tick);
  n

(* The output instant fell [remainder / phase_step] of a tick before the end
   of the newest tick. The kernel is centred 121.5 ticks earlier still, so
   every tick it reads has arrived: the output lags the chips by that much
   (24 output samples). *)
let emit t remainder =
  let base = remainder * phases / phase_step * taps in
  let band = ref 0 in
  for m = 0 to taps - 1 do
    band := !band + (Array.unsafe_get table (base + m) * Array.unsafe_get t.history (t.next + m))
  done;
  t.dc <- !band - t.band_prev + t.dc - (t.dc asr dc_shift);
  t.band_prev <- !band;
  Buffer.add_int16_le t.out (Int.max sample_min (Int.min sample_max (t.dc asr output_shift)))

let push t level =
  t.history.(t.next) <- level;
  t.history.(t.next + taps) <- level;
  t.next <- (if t.next = taps - 1 then 0 else t.next + 1);
  t.phase <- t.phase + phase_step;
  if t.phase >= cpu_hz then begin
    t.phase <- t.phase - cpu_hz;
    emit t t.phase
  end

let clear_output t = Buffer.clear t.out
let output t = Buffer.contents t.out

let write_state w t =
  State_codec.put_int w t.cycle_rest;
  State_codec.put_int w t.phase;
  for m = 0 to taps - 1 do
    State_codec.put_int w t.history.(t.next + m)
  done;
  State_codec.put_int w t.band_prev;
  State_codec.put_int w t.dc

(* v8 and v9 kept a box average instead: its running sum covers less than
   one output sample and has no filter history, so the filters restart from
   silence. *)
let read_state r t =
  t.cycle_rest <- State_codec.get_int r ~min:0 ~max:(cycles_per_tick - 1);
  t.phase <- State_codec.get_int r ~min:0 ~max:(cpu_hz - 1);
  (* With [next] at 0 the kernel reads the lower copy, and each push writes
     its upper slot before the kernel gets there: only the lower copy needs
     the saved levels. *)
  t.next <- 0;
  if State_codec.version r >= 11 then begin
    for m = 0 to taps - 1 do
      t.history.(m) <- State_codec.get_int r ~min:(-level_bound) ~max:level_bound
    done;
    t.band_prev <- State_codec.get_int r ~min:(-sum_bound) ~max:sum_bound;
    t.dc <- State_codec.get_int r ~min:(-dc_bound) ~max:dc_bound
  end
  else begin
    ignore (State_codec.get_int r ~min:min_int ~max:max_int);
    ignore (State_codec.get_int r ~min:0 ~max:max_int);
    Array.fill t.history 0 taps 0;
    t.band_prev <- 0;
    t.dc <- 0
  end
