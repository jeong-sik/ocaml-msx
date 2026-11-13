(* 스프라이트 모드 1 렌더 증명. 레지스터와 VRAM 을 전부 실제 포트 경로
   (io_write 0x99 래치 / 0x98 데이터)로 써서, 래치 계약까지 같이 돌게
   한다. 판정은 frame_rgb 픽셀과 palette_rgb 기대색의 일치, 그리고
   advance 로 라인을 지난 뒤 읽은 S#0/S#3/S#5.

   기대값의 출처: TMS9918A data manual 2-3 절(Y=-1 이 맨 윗줄, EC 는
   32 도트 왼쪽, 16×16 은 8×8 네 블록), openMSX SpriteChecker.cc
   (라인당 4개, 5th 번호, 충돌 좌표 +12/+8). *)

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

let status t n =
  set_reg t 15 n;
  let v = Vdp.io_read t ~port:0x99 in
  set_reg t 15 0;
  v

(* SCREEN1 관용 배치: SAT = 0x3B00 (R#5=0x76), PT = 0x3800 (R#6=0x07). *)
let sat = 0x3b00
let pt = 0x3800

let sat_w t i y x pat col =
  vram_w t (sat + i * 4) y;
  vram_w t (sat + i * 4 + 1) x;
  vram_w t (sat + i * 4 + 2) pat;
  vram_w t (sat + i * 4 + 3) col

let solid = [ 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff; 0xff ]
let pat8_w t p bytes = List.iteri (fun row b -> vram_w t (pt + p * 8 + row) b) bytes

let screen1 t ?(r1 = 0xe0) () =
  set_reg t 0 0x00;
  set_reg t 1 r1;
  set_reg t 5 0x76;
  set_reg t 6 0x07;
  (* 나머지 엔트리는 terminator 로 끊는다. *)
  vram_w t sat 0xd0

(* Two frames: the second VBlank publishes lines all scanned after the
   first, so every line reflects the state written before. *)
let show t = ignore (Vdp.advance t ~cycles:(2 * 262 * 228))

let px t x y =
  show t;
  let rgb = Vdp.frame_rgb t in
  let i = (y * 256 + x) * 3 in
  (Char.code rgb.[i], Char.code rgb.[i + 1], Char.code rgb.[i + 2])

let is_col t x y c = px t x y = Vdp.palette_rgb t c

(* The sprite check of a line runs 14 cycles into it, at openMSX's line
   edge (84 ticks into this model's line). *)
let sprite_check_cycle = 14

(* Run the VDP [line] lines on, then past that line's sprite check. *)
let run_to_line t line =
  ignore (Vdp.advance t ~cycles:(228 * line));
  let _, cycle = Vdp.line_now t in
  if cycle < sprite_check_cycle then
    ignore (Vdp.advance t ~cycles:(sprite_check_cycle - cycle))

(* Vdp.create is a frame start, 42 lines above display line 0 (openMSX
   VDP::reset calls frameStart); these checks count lines from display
   line 0. *)
let to_display_start t =
  while Vdp.line_now t <> (0, 0) do ignore (Vdp.advance t ~cycles:1) done

let () =
  (* 1. 8×8 단색: Y 값 다음 줄부터 그린다 *)
  let t = Vdp.create () in
  to_display_start t;
  screen1 t ();
  pat8_w t 0 solid;
  sat_w t 0 100 50 0 9;
  sat_w t 1 0xd0 0 0 0;
  check "solid: (50,101) is color 9" (is_col t 50 101 9);
  check "solid: (57,108) is color 9" (is_col t 57 108 9);
  check "solid: (58,101) stays background" (is_col t 58 101 0);
  check "solid: (50,100) stays background (Y+1 rule)" (is_col t 50 100 0);
  check "solid: (50,109) stays background" (is_col t 50 109 0)

let () =
  (* 2. Y=255 는 -1: 맨 윗줄부터 보인다 *)
  let t = Vdp.create () in
  to_display_start t;
  screen1 t ();
  pat8_w t 0 solid;
  sat_w t 0 0xff 10 0 9;
  sat_w t 1 0xd0 0 0 0;
  check "Y=255: line 0 shows" (is_col t 10 0 9);
  check "Y=255: line 7 shows" (is_col t 10 7 9);
  check "Y=255: line 8 clear" (is_col t 10 8 0)

let () =
  (* 3. SAT 0xD0 terminator: 그 엔트리부터 뒤는 없다 *)
  let t = Vdp.create () in
  to_display_start t;
  screen1 t ();
  pat8_w t 0 solid;
  pat8_w t 2 solid;
  sat_w t 0 100 10 0 9;
  sat_w t 1 0xd0 0 0 0;
  sat_w t 2 100 100 2 10;
  check "term: entry 0 shows" (is_col t 10 101 9);
  check "term: entry 2 after 0xD0 does not" (not (is_col t 100 101 10))

let () =
  (* 4. 컬러 0 은 TP 가 꺼져 있으면 투명, 켜면 팔레트 0 으로 그린다 *)
  let t = Vdp.create () in
  to_display_start t;
  screen1 t ();
  set_reg t 3 0x80; (* G1 컬러 테이블 = 0x2000 *)
  vram_w t 0x2000 0xf4; (* 첫 칸: 배경 4 *)
  pat8_w t 0 solid;
  sat_w t 0 100 50 0 0;
  sat_w t 1 0xd0 0 0 0;
  check "transparent: color 0 draws nothing" (is_col t 50 101 4);
  set_reg t 8 0x20;
  check "TP: color 0 draws palette 0" (is_col t 50 101 0)

let () =
  (* 5. 16×16: 패턴번호 하위 2비트는 버리고 번호×8 바이트에서 시작한다.
     오른쪽 반은 +16, 아래 반은 +8 *)
  let t = Vdp.create () in
  to_display_start t;
  screen1 t ~r1:0xe2 ();
  let base = pt + 4 * 8 in
  for i = 0 to 31 do vram_w t (base + i) 0x00 done;
  vram_w t base 0xc0; (* top-left row 0: dots 0,1 *)
  vram_w t (base + 16) 0x01; (* top-right row 0: dot 15 *)
  vram_w t (base + 8) 0x80; (* bottom-left row 8: dot 0 *)
  sat_w t 0 100 50 5 9; (* pat 5 → 4 와 같은 패턴 *)
  sat_w t 1 0xd0 0 0 0;
  check "16x16: (50,101) set" (is_col t 50 101 9);
  check "16x16: (51,101) set" (is_col t 51 101 9);
  check "16x16: (52,101) clear" (is_col t 52 101 0);
  check "16x16: right half from byte +16" (is_col t 65 101 9);
  check "16x16: (58,101) clear (no interleave)" (is_col t 58 101 0);
  check "16x16: row 1 clear" (is_col t 50 102 0);
  check "16x16: bottom half from byte +8" (is_col t 50 109 9)

let () =
  (* 6. EC(컬러 bit7): X 를 32 도트 왼쪽으로 *)
  let t = Vdp.create () in
  to_display_start t;
  screen1 t ();
  pat8_w t 0 solid;
  sat_w t 0 100 50 0 (0x80 lor 9);
  sat_w t 1 0xd0 0 0 0;
  check "EC: (18,101) set (x-32)" (is_col t 18 101 9);
  check "EC: (34,101) clear" (is_col t 34 101 0);
  check "EC: (50,101) clear" (is_col t 50 101 0)

let () =
  (* 7. MAG: 한 픽셀이 2×2 로 *)
  let t = Vdp.create () in
  to_display_start t;
  screen1 t ~r1:0xe1 ();
  pat8_w t 0 [ 0x80; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00 ];
  sat_w t 0 100 50 0 9;
  sat_w t 1 0xd0 0 0 0;
  check "MAG: (50,101) set" (is_col t 50 101 9);
  check "MAG: (51,101) set (2x wide)" (is_col t 51 101 9);
  check "MAG: (50,102) set (2x tall)" (is_col t 50 102 9);
  check "MAG: (52,101) clear" (is_col t 52 101 0);
  check "MAG: (50,103) clear" (is_col t 50 103 0)

let () =
  (* 8. 우선순위: 번호가 낮은 쪽이 위 *)
  let t = Vdp.create () in
  to_display_start t;
  screen1 t ();
  pat8_w t 0 solid;
  pat8_w t 1 solid;
  sat_w t 1 100 50 1 10;
  sat_w t 0 100 50 0 9;
  sat_w t 2 0xd0 0 0 0;
  check "priority: entry 0 wins the overlap" (is_col t 50 101 9)

let () =
  (* 9. 한 줄에 4개까지: 5번째는 그리지 않고 S#0 에 5S 와 번호 *)
  let t = Vdp.create () in
  to_display_start t;
  screen1 t ();
  pat8_w t 0 solid;
  for i = 0 to 4 do sat_w t i 100 (i * 20) 0 (9 + i) done;
  sat_w t 5 0xd0 0 0 0;
  check "limit: 4th sprite shows" (is_col t 60 101 12);
  check "limit: 5th sprite hidden" (is_col t 80 101 0);
  (* Drawing the frames above left F and 5S set; reading S#0 re-arms them. *)
  ignore (status t 0);
  run_to_line t 100;
  check "limit: no 5S above the sprites" (Vdp.status0 t land 0x40 = 0);
  run_to_line t 1;
  let s0 = status t 0 in
  check "limit: S#0 = 5S | sprite 4" (s0 land 0x5f = 0x44);
  check "limit: reading S#0 keeps the number, clears 5S"
    (Vdp.status0 t land 0x5f = 0x04)

let () =
  (* 10. 5S 가 없으면 S#0 하위 5비트는 terminator 까지 훑은 수 *)
  let t = Vdp.create () in
  to_display_start t;
  screen1 t ();
  sat_w t 0 100 50 0 9;
  sat_w t 1 100 50 0 9;
  sat_w t 2 100 50 0 9;
  sat_w t 3 0xd0 0 0 0;
  run_to_line t 10;
  check "scan count: 3 sprites before 0xD0" (Vdp.status0 t land 0x1f = 3)

let () =
  (* 11. 충돌: 두 스프라이트가 겹치는 줄에서 C 와 좌표(+12, +8) *)
  let t = Vdp.create () in
  to_display_start t;
  screen1 t ();
  pat8_w t 0 solid;
  sat_w t 0 100 50 0 9;
  sat_w t 1 103 54 0 10;
  sat_w t 2 0xd0 0 0 0;
  run_to_line t 103;
  check "collision: not before the overlap line" (Vdp.status0 t land 0x20 = 0);
  run_to_line t 1;
  check "collision: C set" (Vdp.status0 t land 0x20 <> 0);
  check "collision: S#3 = x + 12" (status t 3 = 54 + 12);
  check "collision: S#4 high bits" (status t 4 = 0xfe);
  check "collision: S#5 = checked line + 8" (status t 5 = 104 - 1 + 8);
  check "collision: S#5 read clears coordinates" (status t 3 = 0);
  ignore (status t 0);
  check "collision: S#0 read clears C" (Vdp.status0 t land 0x20 = 0)

let () =
  (* 12. 컬러 0 스프라이트는 TP 가 꺼져 있으면 충돌하지 않는다 *)
  let t = Vdp.create () in
  to_display_start t;
  screen1 t ();
  pat8_w t 0 solid;
  sat_w t 0 100 50 0 0;
  sat_w t 1 100 52 0 10;
  sat_w t 2 0xd0 0 0 0;
  run_to_line t 110;
  check "collision: transparent color 0 does not collide" (Vdp.status0 t land 0x20 = 0);
  set_reg t 8 0x20;
  run_to_line t 262;
  check "collision: TP makes color 0 collide" (Vdp.status0 t land 0x20 <> 0)

let () =
  (* 13. R#8 SPD 는 표시와 판정을 모두 끈다 *)
  let t = Vdp.create () in
  to_display_start t;
  screen1 t ();
  pat8_w t 0 solid;
  sat_w t 0 100 50 0 9;
  sat_w t 1 100 52 0 10;
  sat_w t 2 0xd0 0 0 0;
  set_reg t 8 0x02;
  check "SPD: sprite hidden" (is_col t 50 101 0);
  run_to_line t 110;
  check "SPD: no collision" (Vdp.status0 t land 0x20 = 0)

let () =
  (* 14. 한 줄의 스프라이트 판정은 그 줄 14사이클째(openMSX 줄 경계)에 한다.
     1942(실 BIOS)는 SAT 를 한 바이트씩 고치다 그 사이에 판정이 끼면 옛
     자리와 새 자리가 겹친다 — 판정 전에 고친 바이트는 판정에 든다. *)
  let overlap_then_move ~move_at =
    let t = Vdp.create () in
    to_display_start t;
    screen1 t ();
    pat8_w t 0 solid;
    sat_w t 0 100 50 0 9;
    sat_w t 1 103 54 0 10;
    sat_w t 2 0xd0 0 0 0;
    (* Display line 104 is the first where the two overlap. *)
    ignore (Vdp.advance t ~cycles:((228 * 104) + move_at));
    vram_w t sat 150;
    ignore (Vdp.advance t ~cycles:(sprite_check_cycle + 6));
    Vdp.status0 t land 0x20 <> 0
  in
  check "check time: a SAT write before cycle 14 counts" (not (overlap_then_move ~move_at:13));
  check "check time: a SAT write after cycle 14 is too late" (overlap_then_move ~move_at:14)

let () =
  if !failures > 0 then begin
    Printf.eprintf "sprite_test: %d failure(s)\n%!" !failures;
    exit 1
  end;
  print_endline "sprite_test: all pass"
