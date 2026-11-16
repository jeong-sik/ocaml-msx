(* Mixer: chip ticks to 44.1 kHz. The expected behaviour is openMSX's sound
   path (src/sound/ResampleHQ.cc, MSXMixer.cc): a band-limited low-pass that
   keeps 80% of the output band, then a DC filter with R = 511/512. The
   output is halved.

   A sine is fed as tick levels; its 44.1 kHz amplitude is read from the RMS,
   which does not depend on where the output samples fall on the wave. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let cpu_hz = 3_579_545
let tick_hz = float cpu_hz /. float Mixer.cycles_per_tick
let frame_cycles = 262 * 228

let samples m =
  let s = Mixer.output m in
  Array.init (String.length s / 2) (fun i -> String.get_int16_le s (2 * i))

(* Feed [n] ticks whose levels are [f tick] and return the output. *)
let feed m n f =
  Mixer.clear_output m;
  for i = 0 to n - 1 do
    Mixer.push m (f i)
  done;
  samples m

let ticks_of_seconds s = Float.to_int (s *. tick_hz)

(* Amplitude of a sine at [hz] with tick amplitude [amp], after the output
   has settled: the RMS of the last half second, times sqrt 2. *)
let sine_amplitude hz amp =
  let m = Mixer.create () in
  let level i = Float.to_int (Float.round (amp *. sin (2. *. Float.pi *. hz *. (float i +. 0.5) /. tick_hz))) in
  let s = feed m (ticks_of_seconds 1.0) level in
  let tail = Array.sub s (Array.length s / 2) (Array.length s / 2) in
  let sq = Array.fold_left (fun a v -> a +. (float v *. float v)) 0. tail in
  sqrt (2. *. sq /. float (Array.length tail))

let () =
  (* Sample count: floor(ticks * 705600 / 3579545). *)
  let m = Mixer.create () in
  let n = Mixer.ticks m ~cycles:cpu_hz in
  check "one second of cycles is 223721 ticks" (n = 223_721);
  check "223721 ticks give floor(223721 * 705600 / 3579545) samples"
    (Array.length (feed m n (fun _ -> 0)) = 223_721 * 705_600 / cpu_hz);
  (* A frame is 3733 or 3734 ticks, 735.95 output samples on average. *)
  let m = Mixer.create () in
  let total = ref 0 and ticks = ref 0 and ok = ref true in
  for _ = 1 to 600 do
    let t = Mixer.ticks m ~cycles:frame_cycles in
    ticks := !ticks + t;
    let k = Array.length (feed m t (fun _ -> 0)) in
    if k < 735 || k > 737 then ok := false;
    total := !total + k
  done;
  check "each frame is 735 to 737 samples" !ok;
  check "600 frames give floor(ticks * 705600 / 3579545) samples" (!total = !ticks * 705_600 / cpu_hz);

  (* A step from silence to [level]. Output k stands for the instant
     (k + 1) * 5.073 ticks; the kernel is centred 121.5 ticks before it, so
     output 22 is centred 4.8 ticks before the step and output 23 0.25 ticks
     after it: 23 is the first past half the halved step. *)
  let level = 20_000 in
  let s = feed (Mixer.create ()) (ticks_of_seconds 1.0) (fun _ -> level) in
  let first = ref (-1) in
  Array.iteri (fun i v -> if !first < 0 && v >= level / 4 then first := i) s;
  check (Printf.sprintf "output lags the step by 23 samples (got %d)" !first) (!first = 23);
  (* A band-limited step rings ahead of itself; ten samples early the ringing
     stays under 1% of the halved step. *)
  check "near silence before the step"
    (Array.for_all (fun v -> abs v < level / 2 / 100) (Array.sub s 0 10));
  let peak = Array.fold_left max min_int s in
  check (Printf.sprintf "step peaks near half the level (got %d)" peak)
    (peak >= level / 2 && peak < level / 2 * 112 / 100);
  (* After the step, each sample is 511/512 of the one before:
     (511/512)^512 = 0.3675 over 512 samples. *)
  let ratio = float s.(1512) /. float s.(1000) in
  check (Printf.sprintf "a held level fades by 511/512 per sample (got %.4f)" ratio)
    (ratio > 0.362 && ratio < 0.373);
  check "a held level ends in silence"
    (Array.for_all (( = ) 0) (Array.sub s (Array.length s - 1000) 1000));
  let neg = feed (Mixer.create ()) (ticks_of_seconds 1.0) (fun _ -> -level) in
  check "a held negative level ends in silence"
    (Array.for_all (( = ) 0) (Array.sub neg (Array.length neg - 1000) 1000));

  (* Pass band and stop band: halved amplitude 4000 for a tick amplitude of
     8000. A box average lets 30 kHz through at 0.39 of its amplitude. *)
  let amp = 8000. and half = 4000. in
  List.iter
    (fun hz ->
      let a = sine_amplitude hz amp in
      check (Printf.sprintf "%.0f Hz passes (amplitude %.1f)" hz a) (Float.abs (a -. half) < half *. 0.01))
    [ 1000.; 10_000.; 15_000. ];
  let a = sine_amplitude 17_640. amp in
  check (Printf.sprintf "17640 Hz is the cutoff, about half (amplitude %.1f)" a) (a > half *. 0.4 && a < half *. 0.6);
  (* From 20.5 kHz on, what is left is the rounding of the output (amplitude
     1): more than 66 dB down. *)
  List.iter
    (fun hz ->
      let a = sine_amplitude hz amp in
      check (Printf.sprintf "%.0f Hz is cut (amplitude %.1f)" hz a) (a < 2.))
    [ 20_500.; 22_500.; 24_000.; 30_000. ];

  (* Timing: output k stands for (k + 1) * 3579545 / 705600 ticks, less the
     121.5-tick lag; tick i's level stands at i + 0.5. At 5 kHz each sample
     must match the sine at that instant. The DC filter shifts 5 kHz by
     0.0028 rad (11 units); missing the in-tick phase would cost up to one
     tick, 0.14 rad (560 units). The run passes tick 715909 (3.2 s), where
     an output instant falls exactly on a tick boundary. *)
  let hz = 5000. in
  let m = Mixer.create () in
  let s = feed m (ticks_of_seconds 3.5) (fun i ->
    Float.to_int (Float.round (amp *. sin (2. *. Float.pi *. hz *. (float i +. 0.5) /. tick_hz)))) in
  let worst = ref 0. in
  for k = 4000 to Array.length s - 1 do
    let p = (float (k + 1) *. float cpu_hz /. 705_600.) -. 121.5 in
    let expected = half *. sin (2. *. Float.pi *. hz *. p /. tick_hz) in
    worst := Float.max !worst (Float.abs (float s.(k) -. expected))
  done;
  check (Printf.sprintf "each sample is the sine at its instant (worst %.1f)" !worst) (!worst < 40.);

  (* Restore continues with the same samples. *)
  let noise i = ((i * 7919) + (i * i * 31)) land 0x3fff in
  let m = Mixer.create () in
  ignore (feed m 100_003 noise);
  let w = State_codec.writer () in
  Mixer.write_state w m;
  let r = State_codec.reader (State_codec.finish w) in
  let q = Mixer.create () in
  ignore (feed q 77 noise);
  Mixer.read_state r q;
  State_codec.end_of_input r;
  let more i = noise (i + 100_003) in
  check "restored mixer continues with the same samples" (feed m 50_000 more = feed q 50_000 more);

  (* A v9 resampler block holds the box average: cycles, phase, sum, count.
     The filters restart from silence. *)
  let w = State_codec.writer () in
  List.iter (State_codec.put_int w) [ 5; 1234; 777_000; 3 ];
  let current = State_codec.finish w in
  let header = 11 + 16 in
  let payload = String.sub current header (String.length current - header) in
  let v9 = "OCAML-MSX\000\009" ^ Digest.string payload ^ payload in
  let r = State_codec.reader v9 in
  let q = Mixer.create () in
  ignore (feed q 10_000 (fun _ -> 9_000));
  Mixer.read_state r q;
  State_codec.end_of_input r;
  check "v9 block restarts the filters from silence" (Array.for_all (( = ) 0) (feed q 5_000 (fun _ -> 0)));
  check "v9 block keeps the cycle remainder" (Mixer.ticks q ~cycles:11 = 1);

  (* Out-of-range levels saturate instead of wrapping. *)
  let s = feed (Mixer.create ()) 1_000 (fun _ -> 200_000) in
  check "a level past 16 bits saturates at 32767" (Array.for_all (( = ) 32767) (Array.sub s 30 30));

  if !failures > 0 then exit 1;
  print_endline "mixer_test: all pass"
