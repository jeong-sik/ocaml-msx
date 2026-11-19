(* 스프라이트 모드 1 렌더 증명. 레지스터와 VRAM 을 전부 실제 포트 경로
   (io_write 0x99 래치 / 0x98 데이터)로 써서, 래치 계약까지 같이 돌게
   한다. 판정은 frame_rgb 픽셀과 palette_rgb 기대색의 일치. *)

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

(* SCREEN1 관용 배치: SAT = 0x3B00 (R#5=0x76), PT = 0x3800 (R#6=0x07). *)
let sat = 0x3b00
let pt = 0x3800

let sat_w t i y x pat col =
  vram_w t (sat + i * 4) y;
  vram_w t (sat + i * 4 + 1) x;
  vram_w t (sat + i * 4 + 2) pat;
  vram_w t (sat + i * 4 + 3) col

let pat8_w t p bytes = List.iteri (fun row b -> vram_w t (pt + p * 8 + row) b) bytes

let screen1 t ?(r1 = 0xe0) () =
  set_reg t 0 0x00;
  set_reg t 1 r1;
  set_reg t 5 0x76;
  set_reg t 6 0x07

let px t x y =
  let rgb = Vdp.frame_rgb t in
  let i = (y * 256 + x) * 3 in
  (Char.code rgb.[i], Char.code rgb.[i + 1], Char.code rgb.[i + 2])

let is_col t x y c =
  let (r1, g1, b1) = px t x y in
  let (r2, g2, b2) = Vdp.palette_rgb t c in
  r1 = r2 && g1 = g2 && b1 = b2

let () =
  (* 1. 8×8 단색: 맨 윗줄은 Y+1 줄, X 는 그대로 *)
  let t = Vdp.create () in
  screen1 t ();
  pat8_w t 0 [ 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff ];
  sat_w t 0 100 50 0 9;
  check "solid: (50,101) is color 9" (is_col t 50 101 9);
  check "solid: (57,101) is color 9" (is_col t 57 101 9);
  check "solid: (58,101) stays background" (is_col t 58 101 0);
  check "solid: (50,100) stays background (Y+1 rule)" (is_col t 50 100 0)

let () =
  (* 2. SAT 0xD0 terminator: 그 엔트리부터 뒤는 없다 *)
  let t = Vdp.create () in
  screen1 t ();
  pat8_w t 0 [ 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff ];
  pat8_w t 2 [ 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff ];
  sat_w t 0 100 10 0 9;
  sat_w t 1 0xd0 0 0 0;
  sat_w t 2 100 100 2 10;
  check "term: entry 0 shows" (is_col t 10 101 9);
  check "term: entry 2 after 0xD0 does not" (not (is_col t 100 101 10))

let () =
  (* 3. Y=0xFF 는 맨 윗줄이 0번 줄이다 — 숨는 값이 아니고 뒤도 끊지 않는다.
     openMSX 와 같은 최소 ROM 으로 확인한 위치: Y=7 의 8×8 이 8번 줄을 덮고
     Y=0xFF 의 8×8 이 0번 줄에서 시작한다. *)
  let t = Vdp.create () in
  screen1 t ();
  pat8_w t 0 [ 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff ];
  sat_w t 0 0xff 10 0 9;
  sat_w t 1 100 100 0 10;
  check "Y=0xFF: line 0 is color 9" (is_col t 10 0 9);
  check "Y=0xFF: line 7 is color 9" (is_col t 10 7 9);
  check "Y=0xFF: line 8 is background" (is_col t 10 8 0);
  check "Y=0xFF: entry 1 after it still shows" (is_col t 100 101 10)

let () =
  (* 3b. 줄 번호는 256 으로 돌아 나간다: Y=0xF0 의 16×16 은 top=241 이라
     15행(마지막 행)이 0번 줄에 걸린다 *)
  let t = Vdp.create () in
  screen1 t ~r1:0xe2 ();
  for row = 0 to 15 do
    vram_w t (pt + row * 2) (if row = 15 then 0x80 else 0x00);
    vram_w t (pt + row * 2 + 1) 0x00
  done;
  sat_w t 0 0xf0 50 0 9;
  check "wrap: row 15 lands on line 0" (is_col t 50 0 9);
  check "wrap: line 1 is background" (is_col t 50 1 0)

let () =
  (* 4. 컬러 0 은 투명 *)
  let t = Vdp.create () in
  screen1 t ();
  pat8_w t 0 [ 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff ];
  sat_w t 0 100 50 0 0;
  check "transparent: color 0 draws nothing" (is_col t 50 101 0)

let () =
  (* 5. 16×16: 패턴번호 하위 2비트는 버리고 ×32 바이트 *)
  let t = Vdp.create () in
  screen1 t ~r1:0xe2 ();
  (* 패턴 4 의 첫 줄만 왼쪽 픽셀 2개 *)
  for row = 0 to 15 do
    vram_w t (pt + 4 * 32 + row * 2) (if row = 0 then 0xc0 else 0x00);
    vram_w t (pt + 4 * 32 + row * 2 + 1) 0x00
  done;
  sat_w t 0 100 50 5 9; (* pat 5 → 4 와 같은 패턴 *)
  check "16x16: (50,101) set" (is_col t 50 101 9);
  check "16x16: (51,101) set" (is_col t 51 101 9);
  check "16x16: (52,101) clear" (is_col t 52 101 0);
  (* 두 번째 줄(row 1)은 0 *)
  check "16x16: row 1 clear" (is_col t 50 102 0)

let () =
  (* 6. EC(컬러 bit7): X 를 32px 왼쪽으로 *)
  let t = Vdp.create () in
  screen1 t ();
  pat8_w t 0 [ 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff ];
  sat_w t 0 100 50 0 (0x80 lor 9);
  check "EC: (18,101) set (x-32)" (is_col t 18 101 9);
  check "EC: (25,101) set (x-32+7)" (is_col t 25 101 9);
  check "EC: (34,101) clear (not x-16)" (is_col t 34 101 0);
  check "EC: (50,101) clear" (is_col t 50 101 0)

let () =
  (* 7. MAG: 한 픽셀이 2×2 로 *)
  let t = Vdp.create () in
  screen1 t ~r1:0xe1 ();
  (* 패턴의 (0,0) 픽셀만 *)
  pat8_w t 0 [ 0x80; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00 ];
  sat_w t 0 100 50 0 9;
  check "MAG: (50,101) set" (is_col t 50 101 9);
  check "MAG: (51,101) set (2x wide)" (is_col t 51 101 9);
  check "MAG: (50,102) set (2x tall)" (is_col t 50 102 9);
  check "MAG: (52,101) clear" (is_col t 52 101 0)

let () =
  (* 8. 우선순위: 번호가 낮은 쪽이 위 *)
  let t = Vdp.create () in
  screen1 t ();
  pat8_w t 0 [ 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff ];
  pat8_w t 1 [ 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff ];
  sat_w t 1 100 50 1 10;
  sat_w t 0 100 50 0 9;
  check "priority: entry 0 wins the overlap" (is_col t 50 101 9)

let () =
  if !failures > 0 then begin
    Printf.eprintf "sprite_test: %d failure(s)\n%!" !failures;
    exit 1
  end;
  print_endline "sprite_test: all pass"
