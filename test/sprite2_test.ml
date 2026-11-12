(* 스프라이트 모드 2 (GRAPHIC 3-7) 증명. 포트 경로로만 쓰고, frame_rgb
   픽셀과 advance 뒤의 S#0 으로 판정한다.

   기대값의 출처: V9938 application manual 의 sprite mode 2 절(줄마다
   컬러 바이트, CC/IC/EC, 라인당 8개, Y=216 끝), openMSX SpriteChecker.cc
   fillSprites2/findCollision2, SpriteConverter.hh drawMode2 (CC OR 합성,
   GRAPHIC 5 의 2비트 분할), Renderer.hh GRAPHIC7_SPRITE_PALETTE. *)

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
  set_reg t 14 (a lsr 14);
  Vdp.io_write t ~port:0x99 (a land 0xff);
  Vdp.io_write t ~port:0x99 (0x40 lor ((a lsr 8) land 0x3f));
  Vdp.io_write t ~port:0x98 v

let status t n =
  set_reg t 15 n;
  let v = Vdp.io_read t ~port:0x99 in
  set_reg t 15 0;
  v

(* BIOS 배치. SCREEN4: SAT 0x1E00, 컬러 0x1C00, PT 0x3800.
   SCREEN5/6: SAT 0x7600, 컬러 0x7400, PT 0x7800.
   SCREEN8: SAT 0xFA00, 컬러 0xF800, PT 0xF000 (planar 논리 주소). *)
type layout = { r0 : int; r5 : int; r11 : int; r6 : int; sat : int; ct : int; pt : int }

let screen4 = { r0 = 0x04; r5 = 0x3f; r11 = 0; r6 = 0x07; sat = 0x1e00; ct = 0x1c00; pt = 0x3800 }
let screen5 = { r0 = 0x06; r5 = 0xef; r11 = 0; r6 = 0x0f; sat = 0x7600; ct = 0x7400; pt = 0x7800 }
let screen6 = { screen5 with r0 = 0x08 }
let screen8 = { r0 = 0x0e; r5 = 0xf7; r11 = 1; r6 = 0x1e; sat = 0xfa00; ct = 0xf800; pt = 0xf000 }

let setup ?(r1 = 0x60) l =
  let t = Vdp.create () in
  set_reg t 0 l.r0;
  set_reg t 1 r1;
  set_reg t 5 l.r5;
  set_reg t 11 l.r11;
  set_reg t 6 l.r6;
  vram_w t l.sat 216;
  t

let sat_w t l i y x pat =
  vram_w t (l.sat + i * 4) y;
  vram_w t (l.sat + i * 4 + 1) x;
  vram_w t (l.sat + i * 4 + 2) pat

let colour_rows t l i rows = List.iteri (fun row c -> vram_w t (l.ct + i * 16 + row) c) rows
let colour_all t l i c = colour_rows t l i (List.init 16 (fun _ -> c))
let solid t l p = for row = 0 to 7 do vram_w t (l.pt + p * 8 + row) 0xff done

let px t x y =
  let w, _ = Vdp.frame_dims t in
  let rgb = Vdp.frame_rgb t in
  let i = (y * w + x) * 3 in
  (Char.code rgb.[i], Char.code rgb.[i + 1], Char.code rgb.[i + 2])

let is_col t x y c = px t x y = Vdp.palette_rgb t c
let run_to_line t line = ignore (Vdp.advance t ~cycles:(228 * line))

let () =
  (* 1. 줄마다 컬러 테이블 한 바이트 *)
  let l = screen5 in
  let t = setup l in
  solid t l 0;
  sat_w t l 0 100 50 0;
  colour_rows t l 0 [ 9; 10; 9; 10; 9; 10; 9; 10 ];
  vram_w t (l.sat + 4) 216;
  check "colour: row 0 takes the first byte" (is_col t 50 101 9);
  check "colour: row 1 takes the second byte" (is_col t 50 102 10);
  check "colour: Y+1 rule holds in mode 2" (is_col t 50 100 0)

let () =
  (* 2. 모드 2 의 끝은 Y=216. 208 은 그냥 좌표다 *)
  let l = screen5 in
  let t = setup l in
  solid t l 0;
  sat_w t l 0 208 50 0;
  sat_w t l 1 100 80 0;
  colour_all t l 1 9;
  sat_w t l 2 216 120 0;
  sat_w t l 3 100 160 0;
  colour_all t l 3 9;
  check "terminator: Y=208 does not end the table" (is_col t 80 101 9);
  check "terminator: Y=216 ends it" (is_col t 160 101 0)

let () =
  (* 3. 한 줄에 8개, 9번째는 숨고 S#0 에 9S 와 번호 *)
  let l = screen5 in
  let t = setup l in
  solid t l 0;
  for i = 0 to 8 do
    sat_w t l i 100 (i * 16) 0;
    colour_all t l i 9
  done;
  vram_w t (l.sat + 9 * 4) 216;
  check "limit: 8th sprite shows" (is_col t (7 * 16) 101 9);
  check "limit: 9th sprite hidden" (is_col t (8 * 16) 101 0);
  run_to_line t 101;
  check "limit: S#0 = 9S | sprite 8" (status t 0 land 0x5f = 0x48)

let () =
  (* 4. CC=1 은 앞선 CC=0 스프라이트 위에서 색을 OR 한다 *)
  let l = screen5 in
  let t = setup l in
  solid t l 0;
  sat_w t l 0 100 50 0;
  colour_all t l 0 4;
  sat_w t l 1 100 54 0;
  colour_all t l 1 (0x40 lor 2);
  vram_w t (l.sat + 8) 216;
  check "CC: owner alone keeps its colour" (is_col t 50 101 4);
  check "CC: overlap is the OR (4|2)" (is_col t 54 101 6);
  check "CC: CC=1 dots outside the owner keep their colour" (is_col t 60 101 2);
  run_to_line t 110;
  check "CC: CC=1 lines never collide" (Vdp.status0 t land 0x20 = 0)

let () =
  (* 5. CC=1 스프라이트 앞에 CC=0 이 없으면 보이지 않는다 *)
  let l = screen5 in
  let t = setup l in
  solid t l 0;
  sat_w t l 0 100 50 0;
  colour_all t l 0 (0x40 lor 9);
  vram_w t (l.sat + 4) 216;
  check "CC: orphan CC=1 sprite hidden" (is_col t 50 101 0)

let () =
  (* 6. IC=1 줄은 충돌하지 않고, 둘 다 꺼져 있으면 충돌한다 *)
  let l = screen5 in
  let t = setup l in
  solid t l 0;
  sat_w t l 0 100 50 0;
  colour_all t l 0 9;
  sat_w t l 1 100 52 0;
  colour_all t l 1 (0x20 lor 10);
  vram_w t (l.sat + 8) 216;
  run_to_line t 110;
  check "IC: no collision" (Vdp.status0 t land 0x20 = 0);
  colour_all t l 1 10;
  run_to_line t 262;
  check "IC clear: collision" (Vdp.status0 t land 0x20 <> 0);
  check "IC clear: S#3 = 52 + 12" (status t 3 = 64)

let () =
  (* 7. EC 는 줄마다: 컬러 바이트 bit7 이 그 줄만 32 도트 민다 *)
  let l = screen5 in
  let t = setup l in
  solid t l 0;
  sat_w t l 0 100 50 0;
  colour_rows t l 0 [ 0x80 lor 9; 9; 9; 9; 9; 9; 9; 9 ];
  vram_w t (l.sat + 4) 216;
  check "EC: row 0 shifted to x-32" (is_col t 18 101 9 && is_col t 50 101 0);
  check "EC: row 1 stays" (is_col t 50 102 9)

let () =
  (* 8. R#23 세로 스크롤은 스프라이트도 함께 올린다 *)
  let l = screen5 in
  let t = setup l in
  solid t l 0;
  sat_w t l 0 100 50 0;
  colour_all t l 0 9;
  vram_w t (l.sat + 4) 216;
  set_reg t 23 10;
  check "scroll: Y=100 shows at line 91" (is_col t 50 91 9 && is_col t 50 90 0)

let () =
  (* 9. GRAPHIC 3 (SCREEN4) 은 모드 2 스프라이트를 쓴다 *)
  let l = screen4 in
  let t = setup l in
  solid t l 0;
  sat_w t l 0 100 50 0;
  colour_rows t l 0 [ 9; 10; 9; 10; 9; 10; 9; 10 ];
  vram_w t (l.sat + 4) 216;
  check "SCREEN4: per-line colour" (is_col t 50 101 9 && is_col t 50 102 10)

let () =
  (* 10. GRAPHIC 5 (SCREEN6): 한 도트가 두 픽셀, 색 4비트가 2비트 둘로 *)
  let l = screen6 in
  let t = setup l in
  solid t l 0;
  sat_w t l 0 100 50 0;
  colour_all t l 0 0b1110;
  vram_w t (l.sat + 4) 216;
  check "SCREEN6: left pixel is colour >> 2" (is_col t 100 101 3);
  check "SCREEN6: right pixel is colour & 3" (is_col t 101 101 2)

let () =
  (* 11. GRAPHIC 7 (SCREEN8): planar 주소에 고정 팔레트 *)
  let l = screen8 in
  let t = setup l in
  solid t l 0;
  sat_w t l 0 100 50 0;
  colour_all t l 0 8;
  vram_w t (l.sat + 4) 216;
  (* 팔레트 레지스터 8 을 흰색으로 바꿔도 SCREEN8 스프라이트는 따르지 않는다. *)
  set_reg t 16 8;
  Vdp.io_write t ~port:0x9a 0x77;
  Vdp.io_write t ~port:0x9a 0x07;
  check "SCREEN8: fixed sprite colour 8 = GRB 4,7,2" (px t 50 101 = (255, 145, 72))

let () =
  if !failures > 0 then begin
    Printf.eprintf "sprite2_test: %d failure(s)\n%!" !failures;
    exit 1
  end;
  print_endline "sprite2_test: all pass"
