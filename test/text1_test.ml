(* TEXT1 (SCREEN 0 WIDTH 40) 렌더 증명. 패턴 테이블 주소는 R#4 의 하위 6비트
   × 0x800 (openMSX VDP::updatePatternBase 의 R#4 << 11) 이다. MSXgl 샘플
   s_hello 는 R#4 = 1 (PGT = 0x0800) 로 "Hello MSX world!" 를 쓰는데, 렌더러가
   R#4 × 0x100 번지에서 글꼴을 읽어 화면이 통째로 배경색이었다. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let set_reg t r v =
  Vdp.io_write t ~port:0x99 v;
  Vdp.io_write t ~port:0x99 (0x80 lor r)

let vram_w t a v =
  Vdp.io_write t ~port:0x99 (a land 0xff);
  Vdp.io_write t ~port:0x99 (0x40 lor ((a lsr 8) land 0x3f));
  Vdp.io_write t ~port:0x98 v

let px t x y =
  let rgb = Vdp.frame_rgb t in
  let i = ((y * 256) + x) * 3 in
  (Char.code rgb.[i], Char.code rgb.[i + 1], Char.code rgb.[i + 2])

let is_col t x y c = px t x y = Vdp.palette_rgb t c

(* 글자 0x41 의 첫 행 패턴 0xFC (왼쪽 6픽셀 전부 켜짐), 글자 0 은 비어 있다.
   이름표는 0x0000, 글꼴은 [r4] × 0x800. 화면 왼쪽 8픽셀은 테두리다. *)
let text1 ~r4 =
  let t = Vdp.create () in
  set_reg t 0 0x00;
  set_reg t 1 0x50;
  set_reg t 2 0x00;
  set_reg t 4 r4;
  set_reg t 7 0xf4;
  vram_w t 0 0x41;
  vram_w t ((r4 * 0x800) + (0x41 * 8)) 0xfc;
  t

let () =
  let t = text1 ~r4:1 in
  check "R4=1: glyph at 0x0800 lights its first cell (8,0) with fg 15" (is_col t 8 0 15);
  check "R4=1: glyph at 0x0800 lights the last pixel of the cell (13,0)" (is_col t 13 0 15);
  check "R4=1: next cell is empty, bg 4" (is_col t 14 0 4);
  check "R4=1: second row of the cell is bg 4" (is_col t 8 1 4);
  let t = text1 ~r4:2 in
  check "R4=2: glyph at 0x1000 lights (8,0)" (is_col t 8 0 15);
  let t = text1 ~r4:0 in
  check "R4=0: glyph at 0x0000 lights (8,0)" (is_col t 8 0 15);
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "text1_test: all pass"
