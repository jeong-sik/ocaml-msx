(* ASCII-16 메가롬 뱅킹 증명. 16KB 뱅크마다 첫 바이트를 뱅크 번호로 채운
   256KB 합성 ROM 으로, 뱅크 레지스터 창(0x6000-0x67FF / 0x7000-0x77FF)
   쓰기가 각 세그먼트(0x4000 / 0x8000)에 어떤 뱅크를 보이게 하는지 잰다.
   Plain 의 32KB 동작도 같이 잡아 뱅킹 추가가 예전 카트리지를 바꾸지
   않았음을 증명한다. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

(* 뱅크 b 의 첫 바이트가 b, 나머지 0 — 뱅크가 바뀌면 곧바로 드러난다. *)
let synthetic_256k () =
  let b = Buffer.create (16 * 0x4000) in
  for bank = 0 to 15 do
    Buffer.add_char b (Char.chr bank);
    Buffer.add_string b (String.make (0x4000 - 1) '\000')
  done;
  Buffer.contents b

let machine : Msx.machine = { ram_kb = 64; vram_kb = 128; roms = [ ""; ""; "" ] }

let with_all_pages_on_cart m =
  (* ppi_a = 0xAA: 전 페이지 slot 2 — calslt 가 부팅 후 만드는 상태. *)
  Msx.port_out m 0xA8 0xAA

let () =
  let rom = synthetic_256k () in
  check "synthetic rom is 256KB" (String.length rom = 16 * 0x4000);

  (* Plain: 32KB 넘는 이미지도 앞 32KB 만 보인다 (기존 동작). *)
  let m = Msx.create ~machine in
  with_all_pages_on_cart m;
  Msx.load_cartridge m rom;
  check "plain 0x4000 reads bank0 head" (Msx.mem_read m 0x4000 = 0);
  check "plain 0x8000 reads bank1 head" (Msx.mem_read m 0x8000 = 1);
  check "plain 0x8001 is bank1 body" (Msx.mem_read m 0x8001 = 0);
  Msx.mem_write m 0x6000 5;
  check "plain ignores bank writes" (Msx.mem_read m 0x4000 = 0);

  (* Ascii16: 초기 뱅크 0·1 — 첫 32KB 가 연속으로 보인다. *)
  let m = Msx.create ~machine in
  with_all_pages_on_cart m;
  Msx.load_cartridge ~mapper:Msx.Ascii16 m rom;
  check "ascii16 boots on bank0/bank1"
    (Msx.mem_read m 0x4000 = 0 && Msx.mem_read m 0x8000 = 1);
  Msx.mem_write m 0x6000 5;
  check "0x6000 write picks bank5 for 0x4000" (Msx.mem_read m 0x4000 = 5);
  check "0x8000 keeps bank1" (Msx.mem_read m 0x8000 = 1);
  Msx.mem_write m 0x7000 15;
  check "0x7000 write picks bank15 for 0x8000" (Msx.mem_read m 0x8000 = 15);
  check "0x4000 keeps bank5" (Msx.mem_read m 0x4000 = 5);

  (* 레지스터 창은 0x6000-0x67FF / 0x7000-0x77FF — 창 밖 쓰기는 무시. *)
  Msx.mem_write m 0x6800 9;
  check "0x6800 is outside the window" (Msx.mem_read m 0x4000 = 5);
  Msx.mem_write m 0x7800 9;
  check "0x7800 is outside the window" (Msx.mem_read m 0x8000 = 15);
  Msx.mem_write m 0x6700 8;
  check "any address in 0x6000-0x67FF hits the register"
    (Msx.mem_read m 0x4000 = 8);

  (* 뱅크 번호는 뱅크 수로 접는다 — 16 뱅크에 0x13(19) 을 쓰면 3. *)
  Msx.mem_write m 0x6000 0x13;
  check "bank folds mod 16" (Msx.mem_read m 0x4000 = 3);

  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end;
  print_endline "megarom: all checks passed"
