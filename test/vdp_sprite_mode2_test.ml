(* 스프라이트: 모드 1(SCREEN 2)과 모드 2(SCREEN 4·5) 그림.

   모드 2 의 CC 규칙(컬러 테이블 bit 6)은 openMSX SpriteConverter::drawMode2 를
   따른다. 앞쪽에 CC=0 스프라이트가 없는 CC=1 스프라이트는 보이지 않고, 그 밖의
   스프라이트의 점은 바로 뒤에 이어지는 CC=1 스프라이트들 가운데 같은 점을 켠 것의
   컬러와 OR 된다. CC=0 스프라이트가 사이에 끼면 OR 는 거기서 끊긴다. 예전에는
   앞선 스프라이트의 픽셀 어느 것과도 OR 해서 SCREEN 4·5 의 스프라이트가 openMSX 와
   픽셀 약 2% 달랐다.

   기대값은 VRAM 을 의사 난수로 채우고(스프라이트 32개 모두 켜짐, 컬러 테이블 난수)
   `JR $` 하는 카트 ROM 을 openMSX(my8250cbios, throttle on)에서 12초에 찍은 화면의
   256×192 영역 MD5 다. 같은 ROM 을 이 에뮬레이터로 돌린 화면은 바이트까지 같았다.
   R#23 은 스프라이트도 같이 움직이는지를 함께 본다. *)

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
    { name = "g3mag R#23=37"; r0 = 0x04; r1 = 0x43; r8 = 0x08;
      regs = [ (2, 0x07); (3, 0xff); (4, 0x03); (5, 0x37); (6, 0x07); (7, 0x04) ];
      fills = [ (0x0000, 16384, 29); (0x1c00, 1024, 3); (0x0000, 6144, 7); (0x2000, 6144, 11); (0x3800, 2048, 13); (0x1800, 512, 19); (0x1a00, 128, 17) ];
      scroll = 37; md5 = "061f568886440a4a518c76b326bf276f" };
    { name = "g4mag R#23=0"; r0 = 0x06; r1 = 0x43; r8 = 0x08;
      regs = [ (2, 0x1f); (5, 0xef); (6, 0x1e); (7, 0x04); (11, 0x01) ];
      fills = [ (0x0000, 32768, 3); (0xf000, 2048, 13); (0xf400, 512, 19); (0xf600, 128, 17) ];
      scroll = 0; md5 = "327990fb692471a1cf7306a0e78108f9" };
    { name = "g2 R#23=0"; r0 = 0x02; r1 = 0x42; r8 = 0x08;
      regs = [ (2, 0x06); (3, 0xff); (4, 0x03); (5, 0x36); (6, 0x07); (7, 0x04) ];
      fills = [ (0x0000, 16384, 29); (0x1800, 1024, 3); (0x0000, 6144, 7); (0x2000, 6144, 11); (0x3800, 2048, 13); (0x1b00, 128, 17) ];
      scroll = 0; md5 = "179b6ce901926ffbb51ddcb0ad59793b" };
    { name = "g2 R#23=37"; r0 = 0x02; r1 = 0x42; r8 = 0x08;
      regs = [ (2, 0x06); (3, 0xff); (4, 0x03); (5, 0x36); (6, 0x07); (7, 0x04) ];
      fills = [ (0x0000, 16384, 29); (0x1800, 1024, 3); (0x0000, 6144, 7); (0x2000, 6144, 11); (0x3800, 2048, 13); (0x1b00, 128, 17) ];
      scroll = 37; md5 = "21cd3644a1de2b2d04714c3d243bf854" };
    { name = "g3 R#23=0"; r0 = 0x04; r1 = 0x42; r8 = 0x08;
      regs = [ (2, 0x07); (3, 0xff); (4, 0x03); (5, 0x37); (6, 0x07); (7, 0x04) ];
      fills = [ (0x0000, 16384, 29); (0x1c00, 1024, 3); (0x0000, 6144, 7); (0x2000, 6144, 11); (0x3800, 2048, 13); (0x1800, 512, 19); (0x1a00, 128, 17) ];
      scroll = 0; md5 = "08e0330b4bf2b290ef13a7541ef502d5" };
    { name = "g3 R#23=37"; r0 = 0x04; r1 = 0x42; r8 = 0x08;
      regs = [ (2, 0x07); (3, 0xff); (4, 0x03); (5, 0x37); (6, 0x07); (7, 0x04) ];
      fills = [ (0x0000, 16384, 29); (0x1c00, 1024, 3); (0x0000, 6144, 7); (0x2000, 6144, 11); (0x3800, 2048, 13); (0x1800, 512, 19); (0x1a00, 128, 17) ];
      scroll = 37; md5 = "bf62fa8d8142fda903c402b250ca3c7c" };
    { name = "g3 R#23=200"; r0 = 0x04; r1 = 0x42; r8 = 0x08;
      regs = [ (2, 0x07); (3, 0xff); (4, 0x03); (5, 0x37); (6, 0x07); (7, 0x04) ];
      fills = [ (0x0000, 16384, 29); (0x1c00, 1024, 3); (0x0000, 6144, 7); (0x2000, 6144, 11); (0x3800, 2048, 13); (0x1800, 512, 19); (0x1a00, 128, 17) ];
      scroll = 200; md5 = "caa909c9e243f8b09646673bf7bf02d4" };
    { name = "g4 R#23=0"; r0 = 0x06; r1 = 0x42; r8 = 0x08;
      regs = [ (2, 0x1f); (5, 0xef); (6, 0x1e); (7, 0x04); (11, 0x01) ];
      fills = [ (0x0000, 32768, 3); (0xf000, 2048, 13); (0xf400, 512, 19); (0xf600, 128, 17) ];
      scroll = 0; md5 = "f5202fd42ddc159143c6e7a3bc121873" };
    { name = "g4 R#23=37"; r0 = 0x06; r1 = 0x42; r8 = 0x08;
      regs = [ (2, 0x1f); (5, 0xef); (6, 0x1e); (7, 0x04); (11, 0x01) ];
      fills = [ (0x0000, 32768, 3); (0xf000, 2048, 13); (0xf400, 512, 19); (0xf600, 128, 17) ];
      scroll = 37; md5 = "2d474c5fc8265e6f0ab141b1818edfb6" };
    { name = "g4 R#23=200"; r0 = 0x06; r1 = 0x42; r8 = 0x08;
      regs = [ (2, 0x1f); (5, 0xef); (6, 0x1e); (7, 0x04); (11, 0x01) ];
      fills = [ (0x0000, 32768, 3); (0xf000, 2048, 13); (0xf400, 512, 19); (0xf600, 128, 17) ];
      scroll = 200; md5 = "4515ce20d4d390b2e622d6333c5f73ad" };
  ]

let () =
  List.iter run scenarios;
  if !failures > 0 then begin
    Printf.eprintf "vdp_sprite_mode2_test: %d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "vdp_sprite_mode2_test: all pass"
