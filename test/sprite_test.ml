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
  (* 1. 8×8 단색: Y 값은 하나 크게, X 는 그대로 *)
  let t = Vdp.create () in
  screen1 t ();
  pat8_w t 0 [ 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff ];
  sat_w t 0 100 50 0 9;
  check "solid: (50,99) is color 9" (is_col t 50 99 9);
  check "solid: (57,99) is color 9" (is_col t 57 99 9);
  check "solid: (58,99) stays background" (is_col t 58 99 0);
  check "solid: (50,98) stays background (Y-1 rule)" (is_col t 50 98 0)

let () =
  (* 2. SAT 0xD0 terminator: 그 엔트리부터 뒤는 없다 *)
  let t = Vdp.create () in
  screen1 t ();
  pat8_w t 0 [ 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff ];
  pat8_w t 2 [ 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff ];
  sat_w t 0 100 10 0 9;
  sat_w t 1 0xd0 0 0 0;
  sat_w t 2 100 100 2 10;
  check "term: entry 0 shows" (is_col t 10 99 9);
  check "term: entry 2 after 0xD0 does not" (not (is_col t 100 99 10))

let () =
  (* 3. Y=0xFF 는 그 엔트리만 화면 밖이고 뒤는 끊지 않는다 *)
  let t = Vdp.create () in
  screen1 t ();
  pat8_w t 0 [ 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff ];
  sat_w t 0 0xff 10 0 9;
  sat_w t 1 100 100 0 10;
  check "hidden: entry 1 after a 0xFF still shows" (is_col t 100 99 10)

let () =
  (* 4. 컬러 0 은 투명 *)
  let t = Vdp.create () in
  screen1 t ();
  pat8_w t 0 [ 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff ];
  sat_w t 0 100 50 0 0;
  check "transparent: color 0 draws nothing" (is_col t 50 99 0)

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
  check "16x16: (50,99) set" (is_col t 50 99 9);
  check "16x16: (51,99) set" (is_col t 51 99 9);
  check "16x16: (52,99) clear" (is_col t 52 99 0);
  (* 두 번째 줄(row 1)은 0 — Y+1 줄은 배경 *)
  check "16x16: row 1 clear" (is_col t 50 100 0)

let () =
  (* 6. EC(컬러 bit7): X 를 16px 왼쪽으로 *)
  let t = Vdp.create () in
  screen1 t ();
  pat8_w t 0 [ 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff ];
  sat_w t 0 100 50 0 (0x80 lor 9);
  check "EC: (34,99) set (x-16)" (is_col t 34 99 9);
  check "EC: (50,99) clear" (is_col t 50 99 0)

let () =
  (* 7. MAG: 한 픽셀이 2×2 로 *)
  let t = Vdp.create () in
  screen1 t ~r1:0xe1 ();
  (* 패턴의 (0,0) 픽셀만 *)
  pat8_w t 0 [ 0x80; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00 ];
  sat_w t 0 100 50 0 9;
  check "MAG: (50,99) set" (is_col t 50 99 9);
  check "MAG: (51,99) set (2x wide)" (is_col t 51 99 9);
  check "MAG: (50,100) set (2x tall)" (is_col t 50 100 9);
  check "MAG: (52,99) clear" (is_col t 52 99 0)

let () =
  (* 8. 우선순위: 번호가 낮은 쪽이 위 *)
  let t = Vdp.create () in
  screen1 t ();
  pat8_w t 0 [ 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff ];
  pat8_w t 1 [ 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff ];
  sat_w t 1 100 50 1 10;
  sat_w t 0 100 50 0 9;
  check "priority: entry 0 wins the overlap" (is_col t 50 99 9)

let () =
  if !failures > 0 then begin
    Printf.eprintf "sprite_test: %d failure(s)\n%!" !failures;
    exit 1
  end;
  print_endline "sprite_test: all pass"
