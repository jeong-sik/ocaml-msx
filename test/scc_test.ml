(* Konami SCC 의 계약 증명. 레지스터 창(addr land 0xFF)에 쓰고 믹서 틱
   (CPU 16 사이클) 단위 레벨로 판정한다.

   기대값의 출처는 openMSX src/sound/SCC.cc(Real 모드)와
   src/memory/RomKonamiSCC.cc 다.
   - 샘플은 CPU 32 사이클마다 하나, 채널은 주기 + 1 카운트마다 파형의 다음
     바이트로 간다: 3579545 / (32 * (주기 + 1)) Hz.
   - 레벨은 (바이트 * 볼륨) asr 4, 다섯 채널의 합이다.
   - 채널 5 는 채널 4 의 파형을 튼다. 주기 8 이하면 멈춘다.
   - 0x9000 에 0x3F 를 쓰면 0x9800-0x9FFF 가 SCC 레지스터 창이 된다. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let cpu_hz = 3_579_545
let ticks_per_second = cpu_hz / Mixer.cycles_per_tick

let levels t n = Array.init n (fun _ -> Scc.tick t)

let wave t ch bytes = List.iteri (fun i b -> Scc.write t ((ch * 32) + i) b) bytes
let fill t ch b = wave t ch (List.init 32 (fun _ -> b))
let square t ch = wave t ch (List.init 32 (fun i -> if i < 16 then 0x7f else 0x80))
let period t ch p = Scc.write t (0x80 + (2 * ch)) (p land 0xff); Scc.write t (0x81 + (2 * ch)) (p lsr 8)
let volume t ch v = Scc.write t (0x8a + ch) v
let enable t mask = Scc.write t 0x8f mask

let rising a =
  let n = ref 0 in
  for i = 1 to Array.length a - 1 do
    if a.(i - 1) < 0 && a.(i) >= 0 then incr n
  done;
  !n

let all f a = Array.for_all f a

let () =
  (* Power-up: waveforms read 0xFF, every channel off *)
  let t = Scc.create () in
  check "power-up waveform reads 0xFF" (Scc.read t 0x00 = 0xff && Scc.read t 0x7f = 0xff);
  check "power-up is silent" (all (( = ) 0) (levels t 1000));
  (* Volumes power up at 15: an enabled channel plays the 0xFF waveform *)
  let t = Scc.create () in
  period t 0 100;
  enable t 0x01;
  check "power-up volume 15 plays 0xFF as (-1 * 15) asr 4 = -1" ((levels t 10).(9) = -1);

  (* A square wave at period 253: 3579545 / (32 * 254) = 440.4 Hz *)
  let t = Scc.create () in
  square t 0;
  period t 0 253;
  volume t 0 15;
  enable t 0x01;
  let a = levels t ticks_per_second in
  let e = rising a in
  check (Printf.sprintf "channel 1 at 440 Hz (got %d)" e) (e = 440 || e = 441);
  check "square highs are (127 * 15) asr 4 = 119" (Array.exists (( = ) 119) a);
  check "square lows are (-128 * 15) asr 4 = -120" (Array.exists (( = ) (-120)) a);
  check "a sample lasts two ticks" (a.(0) = a.(1));

  (* Coarse period: R#0x81 low nibble is bits 8-11. 0x3FF: 109.2 Hz *)
  let t = Scc.create () in
  square t 0;
  period t 0 0x3ff;
  volume t 0 15;
  enable t 0x01;
  let e = rising (levels t ticks_per_second) in
  check (Printf.sprintf "period 0x3FF at 109 Hz (got %d)" e) (e = 109 || e = 110);

  (* Level: (byte * volume) asr 4 *)
  let dc byte vol =
    let t = Scc.create () in
    fill t 0 byte;
    period t 0 100;
    volume t 0 vol;
    enable t 0x01;
    let a = levels t 200 in
    a.(199)
  in
  check "0x7F at volume 15 is 119" (dc 0x7f 15 = 119);
  check "0x7F at volume 8 is 63" (dc 0x7f 8 = 63);
  check "0x80 at volume 15 is -120" (dc 0x80 15 = -120);
  check "0xFF at volume 1 is (-1) asr 4 = -1" (dc 0xff 1 = -1);
  check "volume 0 is silence" (dc 0x7f 0 = 0);

  (* Five channels sum; channel 5 plays channel 4's waveform *)
  let t = Scc.create () in
  for ch = 0 to 3 do fill t ch 0x7f done;
  for ch = 0 to 4 do period t ch 100; volume t ch 15 done;
  enable t 0x1f;
  check "five channels at 119 sum to 595" ((levels t 100).(99) = 595);
  let t = Scc.create () in
  fill t 3 0x40;
  period t 4 100;
  volume t 4 15;
  enable t 0x10;
  check "channel 5 plays channel 4's waveform" ((levels t 100).(99) = (0x40 * 15) asr 4);
  (* Enable bit 0 off: channel 1 plays 0 though it runs *)
  check "a disabled channel plays 0"
    (let t = Scc.create () in
     fill t 0 0x7f; period t 0 100; volume t 0 15; enable t 0x00;
     all (( = ) 0) (levels t 100));

  (* Period 8 or less stops the counter: the first byte holds *)
  let ramp t = wave t 0 (List.init 32 (fun i -> i * 4)) in
  let held p =
    let t = Scc.create () in
    ramp t;
    period t 0 p;
    volume t 0 15;
    enable t 0x01;
    let a = levels t 2000 in
    Array.for_all (( = ) a.(0)) a
  in
  check "period 8 stops the waveform" (held 8);
  check "period 9 runs it" (not (held 9));

  (* A new volume shows from the next byte; a period write restarts the
     current byte at once *)
  let t = Scc.create () in
  fill t 0 0x7f;
  period t 0 4000;
  volume t 0 15;
  enable t 0x01;
  ignore (levels t 10);
  volume t 0 8;
  check "volume change waits for the next byte" ((levels t 2).(1) = 119);
  period t 0 4000;
  check "period write replays the byte at the new volume" ((levels t 2).(1) = 63);

  (* A period write restarts the current byte's count: at period 3199 a byte
     lasts 100 samples (200 ticks); a rewrite 50 samples in holds byte 0 for
     another 100 *)
  let t = Scc.create () in
  ramp t;
  period t 0 3199;
  volume t 0 15;
  enable t 0x01;
  ignore (levels t 100);
  period t 0 3199;
  let a = levels t 220 in
  check "period write restarts the byte's count" (a.(150) = 0 && a.(219) = (4 * 15) asr 4);
  (* A disabled channel keeps stepping: at period 31 one byte per sample, so
     after 10 samples off it resumes at byte 10 (the first sample on plays 0,
     the muted output) *)
  let t = Scc.create () in
  ramp t;
  period t 0 31;
  volume t 0 15;
  ignore (levels t 20);
  enable t 0x01;
  let a = levels t 4 in
  check "a disabled channel keeps its position moving" (a.(0) = 0 && a.(2) = (11 * 4 * 15) asr 4);

  (* Deformation bit 5: a period write restarts the waveform *)
  let t = Scc.create () in
  ramp t;
  period t 0 20;
  volume t 0 15;
  enable t 0x01;
  ignore (levels t 500);
  Scc.write t 0xe0 0x20;
  period t 0 20;
  check "deform bit 5 restarts at byte 0" ((levels t 1).(0) = 0);
  (* Bits 0-1 cut the period: bit 1 keeps bits 0-7, bit 0 keeps bits 8-11 *)
  let freq deform p =
    let t = Scc.create () in
    square t 0;
    Scc.write t 0xe0 deform;
    period t 0 p;
    volume t 0 15;
    enable t 0x01;
    rising (levels t ticks_per_second)
  in
  check "deform bit 1: period 0x1FD plays as 0xFD" (freq 0x02 0x1fd = freq 0x00 0xfd);
  check "deform bit 0: period 0x900 plays as 0x09" (freq 0x01 0x900 = freq 0x00 0x09);
  check "deform bits 0+1 act as bit 1" (freq 0x03 0x1fd = freq 0x00 0xfd);

  (* Register block 0x80-0x8F shows again at 0x90 *)
  let t = Scc.create () in
  fill t 0 0x7f;
  Scc.write t 0x90 100;
  Scc.write t 0x9a 15;
  Scc.write t 0x9f 0x01;
  check "0x90-0x9F mirror 0x80-0x8F" ((levels t 100).(99) = 119);

  (* Reads: waveform below 0x80, 0xFF above; reading 0xE0 sets deform 0xFF,
     which makes every waveform read-only *)
  let t = Scc.create () in
  Scc.write t 0x05 0x12;
  check "waveform reads back" (Scc.read t 0x05 = 0x12);
  Scc.write t 0x60 0x21;
  check "0x80-0xDF read 0xFF" (Scc.read t 0x80 = 0xff && Scc.read t 0xa0 = 0xff);
  Scc.write t 0xc0 0x40;
  Scc.write t 0x06 0x33;
  check "0xC0-0xDF writes are ignored" (Scc.read t 0x06 = 0x33);
  check "channel 4's waveform reads at 0x60" (Scc.write t 0x61 0x34; Scc.read t 0x61 = 0x34);
  ignore (Scc.read t 0xe0);
  Scc.write t 0x05 0x56;
  Scc.write t 0x61 0x78;
  check "after a 0xE0 read the waveforms are read-only"
    (Scc.write t 0xe0 0x00; Scc.read t 0x05 = 0x12 && Scc.read t 0x61 = 0x34);
  (* Deform 0x40 rotates what the CPU reads: one byte per period + 1 cycles,
     counted from the deformation write. Period 10: 32 cycles turn it by 2 *)
  let t = Scc.create () in
  ramp t;
  period t 0 10;
  ignore (levels t 10);
  Scc.write t 0xe0 0x40;
  ignore (levels t 2);
  check "deform 0x40 rotates the read-back waveform"
    (Scc.read t 0x00 = 2 * 4 && Scc.read t 0x01 = 3 * 4);

  (* Save mid-note and restore: the continuation is identical *)
  let t = Scc.create () in
  ramp t;
  wave t 1 (List.init 32 (fun i -> 0x70 - (i * 3)));
  period t 0 77;
  period t 1 300;
  volume t 0 12;
  volume t 1 9;
  enable t 0x03;
  Scc.write t 0xe0 0x02;
  period t 1 0x1ff;
  ignore (levels t 12_345);
  let w = State_codec.writer () in
  Scc.write_state w t;
  let r = State_codec.reader (State_codec.finish w) in
  let q = Scc.create () in
  Scc.read_state r q;
  State_codec.end_of_input r;
  check "restored SCC continues with the same levels" (levels t 50_000 = levels q 50_000);
  (* The rotation timer is saved too: a rotating waveform reads back the same *)
  let t = Scc.create () in
  ramp t;
  period t 0 10;
  Scc.write t 0xe0 0x40;
  ignore (levels t 7);
  let w = State_codec.writer () in
  Scc.write_state w t;
  let r = State_codec.reader (State_codec.finish w) in
  let q = Scc.create () in
  Scc.read_state r q;
  check "restored SCC rotation reads back the same" (Scc.read t 0x00 = Scc.read q 0x00);

  (* Through the machine: a Konami SCC cart in slot 2 *)
  let rom = String.make 0x20000 '\x00' in
  let m = Msx.create ~machine:{ Msx.ram_kb = 64; vram_kb = 128; roms = [ ""; ""; "" ] } in
  Msx.load_cartridge ~mapper:Msx.Konami_scc m rom;
  Msx.port_out m 0xa8 0x28;
  Msx.mem_write m 0x9800 0x12;
  check "without 0x3F at 0x9000 the window is ROM" (Msx.mem_read m 0x9800 = 0x00);
  Msx.mem_write m 0x9000 0x3f;
  check "0x3F at 0x9000 opens the SCC window" (Msx.mem_read m 0x9800 = 0xff);
  Msx.mem_write m 0x9800 0x12;
  check "a write lands in the SCC" (Msx.mem_read m 0x9800 = 0x12);
  check "the window repeats every 256 bytes" (Msx.mem_read m 0x9f00 = 0x12);
  for i = 0 to 31 do Msx.mem_write m (0x9800 + i) (if i < 16 then 0x7f else 0x80) done;
  Msx.mem_write m 0x9880 253;
  Msx.mem_write m 0x9881 0;
  Msx.mem_write m 0x988a 15;
  Msx.mem_write m 0x988f 0x01;
  Msx.step m ~frames:60;
  let a = Msx.audio m in
  let v = Array.init (String.length a / 2) (fun i -> String.get_int16_le a (2 * i)) in
  (* The PSG is silent, so the output is 27 * level / 2: 119 -> 1606 *)
  check "Msx.audio carries the SCC at weight 27, halved"
    (Array.exists (( = ) (27 * 119 / 2)) v && Array.exists (( = ) (-27 * 120 / 2)) v);
  Msx.mem_write m 0x9000 0x00;
  check "another bank value closes the window" (Msx.mem_read m 0x9800 = 0x00);

  if !failures > 0 then exit 1;
  print_endline "scc_test: all pass"
