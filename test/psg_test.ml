(* AY-3-8910 PSG 소리의 계약 증명. 레지스터를 쓰고 CPU 사이클만큼 돌린 뒤
   44.1 kHz 출력 샘플로 판정한다.

   기대값의 출처는 openMSX src/sound/AY8910.cc 다.
   - 칩 샘플은 CPU 16 사이클마다 하나(PSG 클록 = CPU/2, 그 1/8).
   - 톤은 주기(칩 샘플)마다 뒤집힌다: R#0/R#1 = 254 면
     3579545 / (32 * 254) = 440.4 Hz.
   - 믹서: (톤 | 톤끔) & (노이즈 | 노이즈끔). 둘 다 끄면 볼륨이 그대로
     직류로 나온다.
   - 볼륨표는 3 dB 간격: 15 → 8191, 13 → 4096, 10 → 1448, 0 → 0.
     믹서가 절반으로 줄여 내보내므로 출력에서는 15 → 4095 다.
   - 엔벨로프는 32단계, 한 단계가 R#11/R#12 칩 샘플이다. 모양:
     0x00 \___, 0x0D /‾‾‾, 0x08 \\\\, 0x0E /\/\.
   - 노이즈는 17비트 LFSR, 주기는 2 * R#6 칩 샘플이다. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let cpu_hz = 3_579_545
let frame_cycles = 262 * 228

(* A PSG feeding its own mixer. The mixer halves the level: a channel at
   volume 15 (8191) comes out as [top]. *)
type rig = { chip : Psg.t; mix : Mixer.t }

let top = 8191 / 2
let rig () = { chip = Psg.create (); mix = Mixer.create () }

(* Samples made by running [cycles] CPU cycles. *)
let run p cycles =
  Mixer.clear_output p.mix;
  for _ = 1 to Mixer.ticks p.mix ~cycles do
    Mixer.push p.mix (Psg.chip_sample p.chip)
  done;
  let s = Mixer.output p.mix in
  Array.init (String.length s / 2) (fun i -> String.get_int16_le s (2 * i))

let write p r v = Psg.write p.chip r v

let psg regs =
  let p = rig () in
  List.iter (fun (r, v) -> write p r v) regs;
  p

let save p =
  let w = State_codec.writer () in
  Psg.write_regs w p.chip;
  Psg.write_state w p.chip;
  Mixer.write_state w p.mix;
  State_codec.reader (State_codec.finish w)

let load r q =
  Psg.read_regs r q.chip;
  Psg.read_state r q.chip;
  Mixer.read_state r q.mix

(* Upward crossings of the half level. *)
let rising samples level =
  let n = ref 0 in
  for i = 1 to Array.length samples - 1 do
    if samples.(i - 1) < level / 2 && samples.(i) >= level / 2 then incr n
  done;
  !n

let all f a = Array.for_all f a
let chip = 16

(* The envelope level of each of the first [steps] steps, when a step lasts
   [ep] chip samples: the output sample in the middle of the step, clear of
   the averaging at its edges. Tone and noise are off, so channel A is the
   envelope level itself. *)
let env_levels shape ~ep ~steps =
  let p = psg [ (7, 0x3f); (8, 0x10); (11, ep land 0xff); (12, ep lsr 8); (13, shape) ] in
  Array.init steps (fun _ ->
    let s = run p (ep * chip) in
    s.(Array.length s / 2))

let () =
  (* Tone A, 440 Hz *)
  let p = psg [ (0, 254); (1, 0); (7, 0x3e); (8, 15) ] in
  let s = run p cpu_hz in
  (* 3579545 cycles are 223721 whole chip samples: 44099 output samples. *)
  check "one second of cycles gives floor(223721 * 705600 / 3579545) samples"
    (Array.length s = cpu_hz / chip * 705600 / cpu_hz);
  let edges = rising s top in
  check (Printf.sprintf "tone A at 440 Hz (got %d)" edges) (edges = 440 || edges = 441);
  check "tone high level is volume 15" (Array.exists (( = ) top) s);
  check "tone low level is silence" (Array.exists (( = ) 0) s);
  check "tone never exceeds volume 15" (all (fun v -> v >= 0 && v <= top) s);

  (* Period 0 counts as 1 (Generator::setPeriod) *)
  let tone tp = run (psg [ (0, tp); (7, 0x3e); (8, 15) ]) 20_000 in
  check "tone period 0 sounds like period 1" (tone 0 = tone 1);
  let env ep = run (psg [ (7, 0x3f); (8, 0x10); (11, ep); (13, 0x0e) ]) 20_000 in
  check "envelope period 0 runs like period 1" (env 0 = env 1);
  let noise np = run (psg [ (6, np); (7, 0x37); (8, 15) ]) 20_000 in
  check "noise period 0 runs like period 1" (noise 0 = noise 1);
  (* Coarse period bits: R#1 low nibble is bits 8-11 *)
  let p = psg [ (0, 0); (1, 0x12); (7, 0x3e); (8, 15) ] in
  let edges = rising (run p cpu_hz) top in
  (* period 0x200 = 512: 3579545 / (32 * 512) = 218.5 Hz *)
  check (Printf.sprintf "tone A at 218 Hz with R#1 = 0x12 (got %d)" edges)
    (edges = 218 || edges = 219);

  let p = psg [ (0, 0); (1, 0x08); (7, 0x3e); (8, 15) ] in
  let edges = rising (run p cpu_hz) top in
  (* period 0x800 = 2048: 3579545 / (32 * 2048) = 54.6 Hz *)
  check (Printf.sprintf "tone A at 54 Hz with R#1 = 0x08 (got %d)" edges) (edges = 54 || edges = 55);

  (* Channels B and C use their own registers and enable bits *)
  let p = psg [ (2, 254); (3, 0); (7, 0x3d); (9, 15) ] in
  check "tone B" (let e = rising (run p cpu_hz) top in e = 440 || e = 441);
  let p = psg [ (4, 254); (5, 0); (7, 0x3b); (10, 15) ] in
  check "tone C" (let e = rising (run p cpu_hz) top in e = 440 || e = 441);

  (* Volume 0 is silence; disabled tone and noise give a DC level *)
  let p = psg [ (0, 254); (7, 0x3e); (8, 0) ] in
  check "volume 0 is silent" (all (( = ) 0) (run p 100_000));
  (* openMSX volumeTab: level i is 2^(-(15 - i) / 2) of full scale *)
  for vol = 0 to 15 do
    let level =
      if vol = 0 then 0 else Float.to_int (Float.round (8191. *. (2. ** (-.Float.of_int (15 - vol) /. 2.)))) / 2
    in
    let p = psg [ (7, 0x3f); (8, vol) ] in
    check (Printf.sprintf "both disabled, volume %d is DC %d" vol level)
      (all (( = ) level) (run p 50_000))
  done;
  let p = psg [ (7, 0x3f); (8, 15); (9, 15); (10, 15) ] in
  check "three channels at 15 sum to 24573, halved" (all (( = ) (24573 / 2)) (run p 50_000));

  (* Envelope shapes, 32 steps of 100 chip samples *)
  let lv = env_levels 0x00 ~ep:100 ~steps:40 in
  check "\\___ starts at the top" (lv.(0) = top);
  check "\\___ falls" (lv.(10) < lv.(0) && lv.(20) < lv.(10));
  check "\\___ holds silence after 32 steps" (lv.(33) = 0 && lv.(39) = 0);
  let lv = env_levels 0x0d ~ep:100 ~steps:40 in
  check "/‾‾ starts silent" (lv.(0) = 0);
  check "/‾‾ holds the top after 32 steps" (lv.(33) = top && lv.(39) = top);
  let lv = env_levels 0x08 ~ep:100 ~steps:70 in
  check "\\\\\\\\ repeats every 32 steps" (lv.(1) = lv.(33) && lv.(33) = lv.(65));
  check "\\\\\\\\ restarts at the top" (lv.(32) = top && lv.(31) = 0);
  let lv = env_levels 0x0e ~ep:100 ~steps:70 in
  check "/\\/\\ rises, then falls" (lv.(0) = 0 && lv.(31) = top && lv.(32) = top && lv.(63) = 0);
  check "/\\/\\ repeats every 64 steps" (lv.(5) = lv.(69) && lv.(2) = lv.(66));
  (* Continue = 0 shapes end silent; 0x0B and 0x0A use hold/alternate *)
  let lv = env_levels 0x04 ~ep:100 ~steps:40 in
  check "/___ rises, then drops to silence and holds" (lv.(0) = 0 && lv.(31) = top && lv.(32) = 0 && lv.(39) = 0);
  let lv = env_levels 0x0b ~ep:100 ~steps:40 in
  check "\\‾‾ falls, then holds the top" (lv.(0) = top && lv.(31) = 0 && lv.(32) = top && lv.(39) = top);
  let lv = env_levels 0x0a ~ep:100 ~steps:70 in
  check "\\/\\/ falls, then rises" (lv.(0) = top && lv.(31) = 0 && lv.(32) = 0 && lv.(63) = top && lv.(64) = top);
  (* R#12 is the high byte of the envelope period: R#11 = 0, R#12 = 1 makes
     a step 256 chip samples long *)
  let lv = env_levels 0x00 ~ep:256 ~steps:33 in
  check "envelope period from R#12" (lv.(0) = top && lv.(1) = top && lv.(2) = 5792 / 2 && lv.(31) = 0);
  (* The AY repeats each of its 16 levels twice: steps 30 and 31 match. *)
  let lv = env_levels 0x0d ~ep:100 ~steps:32 in
  check "AY envelope pairs its 32 steps" (lv.(30) = lv.(31) && lv.(28) = lv.(29) && lv.(29) < lv.(30));
  (* Rewriting the shape restarts it *)
  let p = psg [ (7, 0x3f); (8, 0x10); (11, 100); (13, 0x00) ] in
  ignore (run p (40 * 100 * chip));
  write p 13 0x00;
  let s = run p (50 * chip) in
  check "shape write restarts the envelope at the top" (s.(Array.length s / 2) = top);

  (* Noise on channel A: deterministic, neither stuck nor constant *)
  let noise () =
    let p = psg [ (6, 1); (7, 0x37); (8, 15) ] in
    run p 200_000
  in
  let a = noise () in
  check "noise is deterministic" (a = noise ());
  let mean = Array.fold_left ( + ) 0 a / Array.length a in
  check (Printf.sprintf "noise sits near half level (mean %d)" mean)
    (mean > top / 4 && mean < top * 3 / 4);
  check "noise moves" (Array.exists (( = ) 0) a && Array.exists (fun v -> v > top / 2) a);
  (* The LFSR bit sequence (openMSX NoiseGenerator::doNextEvent: random =
     (random >> 1) ^ ((random & 1) << 13) ^ ((random & 1) << 16), seed 1).
     R#6 = 31 holds each bit 62 chip samples; the output sample in the
     middle of bit k is that bit at volume 15. *)
  let p = psg [ (6, 31); (7, 0x37); (8, 15) ] in
  let s = run p (200 * 62 * chip) in
  let random = ref 1 and ok = ref true in
  for k = 0 to 199 do
    let mid = ((62 * k) + 31) * 705600 / cpu_hz in
    if s.(mid) <> (!random land 1) * top then ok := false;
    let b = !random land 1 in
    random := (!random lsr 1) lxor (b lsl 13) lxor (b lsl 16)
  done;
  check "noise follows the 17-bit LFSR, one bit per 2 * R#6 chip samples" !ok;
  (* Noise off, tone off but noise enable bit for another channel only *)
  let p = psg [ (6, 1); (7, 0x2f); (8, 15) ] in
  check "noise on channel B does not reach channel A" (all (( = ) top) (run p 50_000));

  (* Sample count: floor(chip samples * 705600 / 3579545). A frame is 3733
     or 3734 chip samples, 735.95 output samples on average: 735, 736 or,
     when the phase has built up, 737. *)
  let p = psg [] in
  let total = ref 0 and ok = ref true in
  for _ = 1 to 600 do
    let n = Array.length (run p frame_cycles) in
    if n < 735 || n > 737 then ok := false;
    total := !total + n
  done;
  check "each frame is 735 to 737 samples" !ok;
  let chips = 600 * frame_cycles / chip in
  check "600 frames give floor(chips * 705600 / 3579545) samples"
    (!total = chips * 705600 / cpu_hz);

  (* Save mid-note and restore: the continuation is identical *)
  let p = psg [ (0, 77); (6, 3); (7, 0x30); (8, 0x10); (9, 12); (10, 7); (11, 33); (13, 0x0e) ] in
  ignore (run p 123_457);
  let r = save p in
  let q = rig () in
  load r q;
  State_codec.end_of_input r;
  check "restored PSG continues with the same samples" (run p 300_000 = run q 300_000);

  (* A shorter tone period keeps the running count below it
     (Generator::setPeriod), so the state saves and restores. *)
  let p = psg [ (0, 0); (1, 0x0f); (7, 0x3e); (8, 15) ] in
  ignore (run p (1000 * chip));
  write p 1 0;
  write p 0 10;
  let r = save p in
  let q = rig () in
  let restored =
    match load r q with
    | () -> true
    | exception State_codec.Invalid_state _ -> false
  in
  check "a shortened period saves and restores" (restored && run p 50_000 = run q 50_000);

  (* Formats 1-7 keep only the registers: reading them sets up the tones,
     noise and envelope like writing them in order *)
  let regs = [ (0, 77); (1, 1); (6, 9); (7, 0x30); (8, 0x10); (9, 12); (11, 40); (12, 0); (13, 0x0a) ] in
  let p = psg regs in
  let w = State_codec.writer () in
  Psg.write_regs w p.chip;
  let r = State_codec.reader (State_codec.finish w) in
  let q = rig () in
  Psg.read_regs r q.chip;
  State_codec.end_of_input r;
  check "registers alone restore like writing them" (run p 300_000 = run q 300_000);

  (* Through the machine: port 0xA0 latches the register, 0xA1 writes it *)
  let m = Msx.create ~machine:{ Msx.ram_kb = 64; vram_kb = 128; roms = [ ""; ""; "" ] } in
  let psg_out r v = Msx.port_out m 0xa0 r; Msx.port_out m 0xa1 v in
  psg_out 0 254;
  psg_out 1 0;
  psg_out 7 0x3e;
  psg_out 8 15;
  Msx.step m ~frames:1;
  let a = Msx.audio m in
  check "Msx.audio holds one frame of samples"
    (String.length a >= 2 * 735 && String.length a <= 2 * 737);
  check "Msx.audio is the tone"
    (let v = Array.init (String.length a / 2) (fun i -> String.get_int16_le a (2 * i)) in
     Array.exists (( = ) top) v && Array.exists (( = ) 0) v);
  check "port 0xA2 reads the register back" (Msx.port_out m 0xa0 1; Msx.port_in m 0xa2 = 0);
  Msx.step m ~frames:2;
  check "Msx.audio is the last step only"
    (let n = String.length (Msx.audio m) / 2 in n >= 1470 && n <= 1472);

  if !failures > 0 then exit 1;
  print_endline "psg_test: all pass"
