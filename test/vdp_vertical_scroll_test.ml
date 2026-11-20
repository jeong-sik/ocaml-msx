(* R#23(세로 스크롤)을 타일·글자·스프라이트 그림에 적용한다.

   R#23 은 비트맵 화면만 움직이고 타일 화면(SCREEN 1·2·3·4)·글자 화면은 움직이지
   않았다. 이제 그림 줄 y 는 (y + R#23) mod 256 줄에서 오고, 24행 밖(24..31
   행)은 이름 테이블 뒤쪽 바이트를 그대로 읽는다. 글자 화면(SCREEN 0)은 행은 그대로
   두고 행 안의 줄만 (줄 + R#23) mod 8 로 옮긴다. 스프라이트는 배경과 같이 움직인다:
   화면 줄 y 에는 줄 (y + R#23) mod 256 에 걸린 스프라이트가 보인다. 글자 화면의
   패턴 테이블 주소는 R#4 의 하위 6비트 × 0x800 이다(예전 식은 R#4 의 하위 3비트 × 0x100).

   기대값은 같은 구성을 카트 ROM 으로 만들어(VRAM 을 의사 난수로 채우고 R#23 을
   쓴 뒤 `JR $`) openMSX(my8250cbios, throttle on)에서 12초에 찍은 화면의
   256×192 영역 MD5 다. 같은 ROM 을 이 에뮬레이터로 돌린 화면은 바이트까지 같았다.
   SCREEN 4·5 는 스프라이트를 끈 구성만 둔다: 스프라이트 모드 2 의 무작위
   스프라이트 모음은 R#23 과 무관하게 openMSX 와 다른 점이 남아 있다. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let set_reg t r v =
  Vdp.io_write t ~port:0x99 v;
  Vdp.io_write t ~port:0x99 (0x80 lor r)

let set_vram_addr t a =
  set_reg t 14 ((a lsr 14) land 7);
  Vdp.io_write t ~port:0x99 (a land 0xff);
  Vdp.io_write t ~port:0x99 (((a lsr 8) land 0x3f) lor 0x40)

(* x <- 5x + 17 mod 256: 주기 256 이라 모든 바이트값이 나온다. *)
let fill t (addr, n, seed) =
  set_vram_addr t addr;
  let d = ref seed in
  for _ = 1 to n do
    d := ((!d * 5) + 17) land 0xff;
    Vdp.io_write t ~port:0x98 !d
  done

type scenario = {
  name : string;
  r0 : int;
  r1 : int;  (* 표시를 켠 값 *)
  r8 : int;
  regs : (int * int) list;
  fills : (int * int * int) list;
  scroll : int;
  md5 : string;  (* openMSX 화면 256×192 RGB 의 MD5 *)
}

let run s =
  let t = Vdp.create () in
  set_reg t 1 0x00;
  set_reg t 0 s.r0;
  List.iter (fun (r, v) -> set_reg t r v) s.regs;
  List.iter (fun (r, v) -> set_reg t r v) [ (8, s.r8); (9, 0x00); (10, 0); (14, 0) ];
  if not (List.mem_assoc 11 s.regs) then set_reg t 11 0;
  set_reg t 23 0;
  List.iter (fill t) s.fills;
  set_reg t 23 s.scroll;
  set_reg t 1 s.r1;
  let w, h = Vdp.frame_dims t in
  check (Printf.sprintf "%s: 256x192" s.name) (w = 256 && h = 192);
  let got = Digest.to_hex (Digest.string (Vdp.frame_rgb t)) in
  check (Printf.sprintf "%s: md5 %s, openMSX %s" s.name got s.md5) (got = s.md5)

let scenarios =
  [
    { name = "text1 R#23=0"; r0 = 0x00; r1 = 0x50; r8 = 0x08;
      regs = [ (2, 0x00); (4, 0x01); (7, 0xf4) ];
      fills = [ (0x0000, 1280, 3); (0x0800, 2048, 7) ];
      scroll = 0; md5 = "4e902ad16d3b4d52034d352c6ca2eae8" };
    { name = "text1 R#23=37"; r0 = 0x00; r1 = 0x50; r8 = 0x08;
      regs = [ (2, 0x00); (4, 0x01); (7, 0xf4) ];
      fills = [ (0x0000, 1280, 3); (0x0800, 2048, 7) ];
      scroll = 37; md5 = "12413ecbc2e39175b9ac3dd4264d98f5" };
    { name = "text1 R#23=200"; r0 = 0x00; r1 = 0x50; r8 = 0x08;
      regs = [ (2, 0x00); (4, 0x01); (7, 0xf4) ];
      fills = [ (0x0000, 1280, 3); (0x0800, 2048, 7) ];
      scroll = 200; md5 = "4e902ad16d3b4d52034d352c6ca2eae8" };
    { name = "text1h R#23=37"; r0 = 0x00; r1 = 0x50; r8 = 0x08;
      regs = [ (2, 0x00); (4, 0x09); (7, 0xf4) ];
      fills = [ (0x0000, 1280, 3); (0x4800, 2048, 7) ];
      scroll = 37; md5 = "12413ecbc2e39175b9ac3dd4264d98f5" };
    { name = "g1 R#23=37"; r0 = 0x00; r1 = 0x42; r8 = 0x08;
      regs = [ (2, 0x06); (3, 0x80); (4, 0x00); (5, 0x36); (6, 0x07); (7, 0x04) ];
      fills = [ (0x0000, 16384, 29); (0x1800, 1024, 3); (0x0000, 2048, 7); (0x2000, 32, 11); (0x3800, 2048, 13); (0x1b00, 128, 17) ];
      scroll = 37; md5 = "ccec936615e31bee22eab2f41acdda97" };
    { name = "g1 R#23=200"; r0 = 0x00; r1 = 0x42; r8 = 0x08;
      regs = [ (2, 0x06); (3, 0x80); (4, 0x00); (5, 0x36); (6, 0x07); (7, 0x04) ];
      fills = [ (0x0000, 16384, 29); (0x1800, 1024, 3); (0x0000, 2048, 7); (0x2000, 32, 11); (0x3800, 2048, 13); (0x1b00, 128, 17) ];
      scroll = 200; md5 = "26428ea258a1709ed4e167cd5ba8ed87" };
    { name = "mc R#23=37"; r0 = 0x00; r1 = 0x4a; r8 = 0x08;
      regs = [ (2, 0x06); (4, 0x00); (5, 0x36); (6, 0x07); (7, 0x04) ];
      fills = [ (0x0000, 16384, 29); (0x1800, 1024, 3); (0x0000, 2048, 7); (0x3800, 2048, 13); (0x1b00, 128, 17) ];
      scroll = 37; md5 = "b0d323c16fb1485e0889f3fceace91bf" };
    { name = "mc R#23=200"; r0 = 0x00; r1 = 0x4a; r8 = 0x08;
      regs = [ (2, 0x06); (4, 0x00); (5, 0x36); (6, 0x07); (7, 0x04) ];
      fills = [ (0x0000, 16384, 29); (0x1800, 1024, 3); (0x0000, 2048, 7); (0x3800, 2048, 13); (0x1b00, 128, 17) ];
      scroll = 200; md5 = "4be3f8f0dca7e2ea5c05d4d0b6b6b71b" };
    { name = "g2 R#23=37"; r0 = 0x02; r1 = 0x42; r8 = 0x08;
      regs = [ (2, 0x06); (3, 0xff); (4, 0x03); (5, 0x36); (6, 0x07); (7, 0x04) ];
      fills = [ (0x0000, 16384, 29); (0x1800, 1024, 3); (0x0000, 6144, 7); (0x2000, 6144, 11); (0x3800, 2048, 13); (0x1b00, 128, 17) ];
      scroll = 37; md5 = "21cd3644a1de2b2d04714c3d243bf854" };
    { name = "g2 R#23=200"; r0 = 0x02; r1 = 0x42; r8 = 0x08;
      regs = [ (2, 0x06); (3, 0xff); (4, 0x03); (5, 0x36); (6, 0x07); (7, 0x04) ];
      fills = [ (0x0000, 16384, 29); (0x1800, 1024, 3); (0x0000, 6144, 7); (0x2000, 6144, 11); (0x3800, 2048, 13); (0x1b00, 128, 17) ];
      scroll = 200; md5 = "769a4648f4e71ea176b3cdd8642d6e65" };
    { name = "g3(no sprites) R#23=0"; r0 = 0x04; r1 = 0x42; r8 = 0x0a;
      regs = [ (2, 0x07); (3, 0xff); (4, 0x03); (5, 0x37); (6, 0x07); (7, 0x04) ];
      fills = [ (0x0000, 16384, 29); (0x1c00, 1024, 3); (0x0000, 6144, 7); (0x2000, 6144, 11); (0x3800, 2048, 13); (0x1800, 512, 19); (0x1a00, 128, 17) ];
      scroll = 0; md5 = "e0c951509ac224b846a3ece8e0f6e222" };
    { name = "g3(no sprites) R#23=37"; r0 = 0x04; r1 = 0x42; r8 = 0x0a;
      regs = [ (2, 0x07); (3, 0xff); (4, 0x03); (5, 0x37); (6, 0x07); (7, 0x04) ];
      fills = [ (0x0000, 16384, 29); (0x1c00, 1024, 3); (0x0000, 6144, 7); (0x2000, 6144, 11); (0x3800, 2048, 13); (0x1800, 512, 19); (0x1a00, 128, 17) ];
      scroll = 37; md5 = "0de5e04fb132500f76274d330044561e" };
    { name = "g3(no sprites) R#23=200"; r0 = 0x04; r1 = 0x42; r8 = 0x0a;
      regs = [ (2, 0x07); (3, 0xff); (4, 0x03); (5, 0x37); (6, 0x07); (7, 0x04) ];
      fills = [ (0x0000, 16384, 29); (0x1c00, 1024, 3); (0x0000, 6144, 7); (0x2000, 6144, 11); (0x3800, 2048, 13); (0x1800, 512, 19); (0x1a00, 128, 17) ];
      scroll = 200; md5 = "bd377c8bcf616c5e1ff13bdd61460366" };
    { name = "g4(no sprites) R#23=37"; r0 = 0x06; r1 = 0x42; r8 = 0x0a;
      regs = [ (2, 0x1f); (5, 0xef); (6, 0x1e); (7, 0x04); (11, 0x01) ];
      fills = [ (0x0000, 32768, 3); (0xf000, 2048, 13); (0xf400, 512, 19); (0xf600, 128, 17) ];
      scroll = 37; md5 = "e9ff958f5c459d0f1329be712c3659d0" };
    { name = "g4(no sprites) R#23=200"; r0 = 0x06; r1 = 0x42; r8 = 0x0a;
      regs = [ (2, 0x1f); (5, 0xef); (6, 0x1e); (7, 0x04); (11, 0x01) ];
      fills = [ (0x0000, 32768, 3); (0xf000, 2048, 13); (0xf400, 512, 19); (0xf600, 128, 17) ];
      scroll = 200; md5 = "bf9be74ed240354ec227c56dcb64752e" };
  ]

let () =
  List.iter run scenarios;
  if !failures > 0 then begin
    Printf.eprintf "vdp_vertical_scroll_test: %d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "vdp_vertical_scroll_test: all pass"
