(* SCREEN 4 (GRAPHIC3) 타일과 스프라이트 모드 2 렌더 증명. 레지스터와 VRAM 을
   전부 실제 포트 경로(io_write 0x99 래치 / 0x98 데이터)로 쓴다. 판정은
   frame_rgb 픽셀과 palette_rgb 기대색의 일치. 스프라이트 규칙은 openMSX 와
   같은 최소 ROM 으로 확인한 모드 1 규칙(맨 윗줄 Y+1, 16×16 배치, EC -32)과
   V9938 모드 2 표(컬러 테이블, CC, TP, 줄당 8개, Y=0xD8 종료)를 따른다. *)

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

let color_table = 0x1c00 (* R#5 = 0x3f *)
let attr = 0x1e00
let pat_table = 0x3800 (* R#6 = 0x07 *)

let sprite_regs t =
  set_reg t 5 0x3f;
  set_reg t 6 0x07;
  set_reg t 11 0x00

let screen4 t =
  set_reg t 0 0x04;
  set_reg t 1 0xe0;
  set_reg t 2 0x06;
  set_reg t 3 0xff;
  set_reg t 4 0x03;
  sprite_regs t

let sat_w t i y x pat =
  vram_w t (attr + (i * 4)) y;
  vram_w t (attr + (i * 4) + 1) x;
  vram_w t (attr + (i * 4) + 2) pat

(* 스프라이트 i 의 행 r 컬러 바이트 *)
let col_w t i r v = vram_w t (color_table + (i * 16) + r) v

let solid8 t p = for r = 0 to 7 do vram_w t (pat_table + (p * 8) + r) 0xff done
let all_rows t i v = for r = 0 to 15 do col_w t i r v done

let px_w t w x y =
  let rgb = Vdp.frame_rgb t in
  let i = ((y * w) + x) * 3 in
  (Char.code rgb.[i], Char.code rgb.[i + 1], Char.code rgb.[i + 2])

let is_col_w t w x y c = px_w t w x y = Vdp.palette_rgb t c
let is_col t x y c = is_col_w t 256 x y c

let () =
  (* 1. 타일: G2 와 같은 레이아웃 — 세 덩어리가 서로 다른 패턴·컬러를 쓰고
     컬러가 행마다 한 바이트다. G1 경로로 빠지면 컬러 테이블을 name>>3 으로
     읽어 아래 값이 하나도 맞지 않는다. *)
  let t = Vdp.create () in
  screen4 t;
  List.iter (fun row -> vram_w t (0x1800 + (row * 32)) 1) [ 0; 8; 16 ];
  List.iter
    (fun (third, fg, bg) ->
      vram_w t ((third * 0x800) + 8) 0x80;
      vram_w t (0x2000 + (third * 0x800) + 8) ((fg lsl 4) lor bg))
    [ (0, 9, 10); (1, 11, 12); (2, 13, 14) ];
  (* 첫 덩어리 0행 말고 1행도: 컬러가 행마다 다르다 *)
  vram_w t 9 0x80;
  vram_w t (0x2000 + 9) 0x34;
  check "tile third 0 fg" (is_col t 0 0 9);
  check "tile third 0 bg" (is_col t 1 0 10);
  check "tile third 1 fg (line 64)" (is_col t 0 64 11);
  check "tile third 1 bg" (is_col t 1 64 12);
  check "tile third 2 fg (line 128)" (is_col t 0 128 13);
  check "tile third 2 bg" (is_col t 1 128 14);
  check "tile row 1 has its own color byte" (is_col t 0 1 3 && is_col t 1 1 4)

let () =
  (* 2. 모드 2 스프라이트: 패턴 행마다 컬러가 다르다 *)
  let t = Vdp.create () in
  screen4 t;
  solid8 t 0;
  sat_w t 0 100 50 0;
  for r = 0 to 7 do col_w t 0 r (r + 2) done;
  for r = 0 to 7 do
    check (Printf.sprintf "per-row color: row %d" r) (is_col t 50 (101 + r) (r + 2))
  done;
  check "per-row color: line above the top stays background" (is_col t 50 100 0);
  check "per-row color: right of 8 px stays background" (is_col t 58 101 0)

let () =
  (* 3. Y=0xD8 이 목록을 끝낸다. 모드 1 의 0xD0 은 여기서 종료가 아니다. *)
  let t = Vdp.create () in
  screen4 t;
  solid8 t 0;
  sat_w t 0 0xd0 10 0;
  sat_w t 1 100 50 0;
  sat_w t 2 0xd8 0 0;
  sat_w t 3 100 100 0;
  all_rows t 0 9;
  all_rows t 1 9;
  all_rows t 3 9;
  check "0xD0 does not end the list: entry 1 shows" (is_col t 50 101 9);
  check "0xD8 ends the list: entry 3 hidden" (is_col t 100 101 0)

let () =
  (* 4. 한 줄에는 번호순 처음 8개만 보인다 *)
  let t = Vdp.create () in
  screen4 t;
  solid8 t 0;
  for i = 0 to 8 do
    sat_w t i 50 (i * 20) 0;
    all_rows t i 9
  done;
  check "8th sprite (index 7) shows" (is_col t 140 51 9);
  check "9th sprite (index 8) is dropped" (is_col t 160 51 0)

let () =
  (* 5. CC: 번호가 낮은 스프라이트 위에서 컬러를 OR 한다. CC 없는 겹침은
     번호가 낮은 쪽이 이긴다. *)
  let t = Vdp.create () in
  screen4 t;
  solid8 t 0;
  sat_w t 0 50 50 0;
  sat_w t 1 50 50 0;
  sat_w t 2 50 100 0;
  sat_w t 3 50 100 0;
  all_rows t 0 1;
  all_rows t 1 (0x40 lor 2);
  all_rows t 2 1;
  all_rows t 3 2;
  check "CC: colors OR (1 lor 2 = 3)" (is_col t 50 51 3);
  check "no CC: lower index wins" (is_col t 100 51 1)

let () =
  (* 6. EC: X 를 32px 왼쪽으로 *)
  let t = Vdp.create () in
  screen4 t;
  solid8 t 0;
  sat_w t 0 50 100 0;
  all_rows t 0 (0x80 lor 9);
  check "EC: (68,51) set (x-32)" (is_col t 68 51 9);
  check "EC: (75,51) set" (is_col t 75 51 9);
  check "EC: (84,51) clear (not x-16)" (is_col t 84 51 0);
  check "EC: (100,51) clear" (is_col t 100 51 0)

let () =
  (* 7. TP(R#8 bit5): 0 이면 스프라이트 컬러 0 은 투명, 1 이면 팔레트 0 으로
     칠한다. openMSX 최소 ROM 으로 확인한 방향이다(SpriteConverter:
     `colIndex == 0 && transparency` 면 그리지 않고 transparency = TP 가 0). *)
  let mk tp =
    let t = Vdp.create () in
    screen4 t;
    (* 첫 덩어리 타일 0 의 배경을 5 로 *)
    for r = 0 to 7 do vram_w t (0x2000 + r) 0x05 done;
    solid8 t 0;
    sat_w t 0 10 50 0;
    all_rows t 0 0;
    set_reg t 8 (if tp then 0x20 else 0x00);
    t
  in
  check "TP=0: sprite color 0 is transparent (tile bg 5 shows)" (is_col (mk false) 50 11 5);
  check "TP=1: sprite color 0 is opaque palette 0" (is_col (mk true) 50 11 0)

let () =
  (* 7b. 모드 1 스프라이트도 같은 TP 규칙(openMSX: "TP bit also has effect in
     sprite mode 1", 실제 V9958 확인) *)
  let mk tp =
    let t = Vdp.create () in
    set_reg t 0 0x00;
    set_reg t 1 0xe0;
    set_reg t 2 0x06;
    set_reg t 3 0x80;
    set_reg t 4 0x00;
    set_reg t 5 0x76;
    set_reg t 6 0x07;
    vram_w t 0x2000 0x05;
    for r = 0 to 7 do vram_w t (0x3800 + r) 0xff done;
    List.iteri (fun i v -> vram_w t (0x3b00 + i) v) [ 100; 50; 0; 0; 0xd0 ];
    set_reg t 8 (if tp then 0x20 else 0x00);
    t
  in
  check "mode 1, TP=0: color 0 sprite is transparent" (is_col (mk false) 50 101 5);
  check "mode 1, TP=1: color 0 sprite is opaque palette 0" (is_col (mk true) 50 101 0)

let () =
  (* 7c. 타일·비트맵 픽셀의 컬러 0 은 TP=0 이면 R#7 하위 4비트의 백드롭 색,
     TP=1 이면 팔레트 0. 같은 ROM 을 openMSX 에서 돌려 G1·G3·SCREEN5 가
     이 방향임을 확인했다. *)
  let tile tp =
    let t = Vdp.create () in
    screen4 t;
    set_reg t 7 0x0c;
    set_reg t 8 (if tp then 0x20 else 0x00);
    t
  in
  check "tile color 0, TP=0: backdrop color 12" (is_col (tile false) 10 10 12);
  check "tile color 0, TP=1: palette 0" (is_col (tile true) 10 10 0);
  let bitmap tp =
    let t = Vdp.create () in
    set_reg t 0 0x06;
    set_reg t 1 0x60;
    set_reg t 2 0x1f;
    set_reg t 7 0x0c;
    set_reg t 8 (if tp then 0x20 else 0x00);
    t
  in
  check "SCREEN5 color 0, TP=0: backdrop color 12" (is_col (bitmap false) 10 10 12);
  check "SCREEN5 color 0, TP=1: palette 0" (is_col (bitmap true) 10 10 0);
  (* SCREEN6: R#7 은 두 팔레트 번호(상위 2비트, 하위 2비트)로 쪼개져 두 픽셀에
     하나씩 쓰인다. 0x0A 면 둘 다 팔레트 2 라 짝수·홀수가 같다. *)
  let screen6 tp =
    let t = Vdp.create () in
    set_reg t 0 0x08;
    set_reg t 1 0x60;
    set_reg t 2 0x1f;
    set_reg t 7 0x0a;
    set_reg t 8 (if tp then 0x20 else 0x00);
    t
  in
  let t6 = screen6 false in
  check "SCREEN6 color 0, TP=0: even pixel uses palette 2" (is_col_w t6 512 20 10 2);
  check "SCREEN6 color 0, TP=0: odd pixel uses palette 2" (is_col_w t6 512 21 10 2);
  check "SCREEN6 color 0, TP=1: palette 0" (is_col_w (screen6 true) 512 20 10 0);
  (* SCREEN8 은 TP 를 쓰지 않는다: 바이트 0 은 검정 *)
  let t8 = Vdp.create () in
  set_reg t8 0 0x0e;
  set_reg t8 1 0x60;
  set_reg t8 2 0x1f;
  set_reg t8 7 0x0c;
  check "SCREEN8 color 0 ignores TP and the backdrop" (px_w t8 256 10 10 = (0, 0, 0))

let () =
  (* 7e. 텍스트는 TP 와 무관하게 컬러 0 이 늘 백드롭이다(openMSX: "Text1, Text2
     and Multicolor ignore the TP bit", 실제 V9958 확인). fg=0, bg=3 인 글자
     픽셀은 TP=1 이어도 팔레트 0 이 아니라 백드롭 3 이다. *)
  let t = Vdp.create () in
  set_reg t 0 0x00;
  set_reg t 1 0x50;
  set_reg t 2 0x04;
  set_reg t 4 0x00;
  set_reg t 7 0x03;
  set_reg t 8 0x20;
  vram_w t 0 0x80;
  check "TEXT1 fg color 0 is the backdrop even with TP=1" (is_col t 8 0 3)

let () =
  (* 7d. 속성·컬러 테이블 주소는 (R11<<15 | R5<<7 | 0x7F) & (오프셋 | ~0x3FF).
     R#5 하위 3비트가 1 이면 컬러 테이블 = R5 상위 5비트 << 10, 속성 테이블 =
     그 +0x200. 하위 비트가 0 이면 오프셋 비트가 지워져 속성·컬러 테이블이
     같은 128바이트로 접힌다(openMSX 에서 R5=0xE8 로 확인: 스프라이트 1 이
     (0,0) 에 컬러 10 으로 뜬다). *)
  let mk r5 =
    let t = Vdp.create () in
    screen4 t;
    set_reg t 5 r5;
    set_reg t 11 0;
    solid8 t 0;
    (* 0x1c00 에 컬러 테이블, 0x1e00 에 속성 테이블 을 두 번 쓴다. *)
    for i = 0 to 1 do
      for r = 0 to 15 do vram_w t (0x1c00 + (i * 16) + r) (if i = 0 then 0 else 10) done
    done;
    List.iteri (fun k v -> vram_w t (0x1e00 + k) v) [ 40; 80; 0; 0; 100; 200; 0; 0; 0xd8 ];
    t
  in
  (* R5 = 0x3f: 컬러 0x1c00, 속성 0x1e00 → 스프라이트 1 은 (200,101) 컬러 10 *)
  check "R5 low bits 111: entry 1 shows at (200,101)" (is_col (mk 0x3f) 200 101 10);
  (* R5 = 0x38: 오프셋의 bit9..7 이 지워져 속성 테이블이 0x1c00, 컬러 테이블도
     0x1c00 — 속성 1 은 0x1c04.. = 0 (Y=0,X=0), 컬러는 0x1c10 = 10 *)
  let t = mk 0x38 in
  check "R5 low bits 000: attribute table folds onto the colour table" (is_col t 200 101 0);
  check "R5 low bits 000: sprite 1 sits at (0,1) with colour 10" (is_col t 0 1 10)

let () =
  (* 8. 16×16 + MAG: 패턴 배치는 모드 1 과 같고 컬러는 행마다 *)
  let t = Vdp.create () in
  screen4 t;
  set_reg t 1 0xe3;
  (* 패턴 4: 왼쪽 반 0행 맨 왼쪽, 오른쪽 반 0행 맨 오른쪽 *)
  for b = 0 to 31 do
    vram_w t (pat_table + 32 + b) (match b with 0 -> 0x80 | 16 -> 0x01 | _ -> 0)
  done;
  sat_w t 0 50 20 5;
  all_rows t 0 9;
  check "16x16 MAG: left pixel (20,51) set" (is_col t 20 51 9);
  check "16x16 MAG: left pixel is 2 wide (21,51)" (is_col t 21 51 9);
  check "16x16 MAG: left pixel is 2 tall (20,52)" (is_col t 20 52 9);
  check "16x16 MAG: right half last pixel (50,51)" (is_col t 50 51 9);
  check "16x16 MAG: gap between halves (22,51) clear" (is_col t 22 51 0)

let () =
  (* 9. SCREEN5 (256폭 비트맵) 에도 모드 2 스프라이트가 올라간다 *)
  let t = Vdp.create () in
  set_reg t 0 0x06;
  set_reg t 1 0x60;
  set_reg t 2 0x1f;
  sprite_regs t;
  solid8 t 0;
  sat_w t 0 100 50 0;
  all_rows t 0 9;
  check "SCREEN5: sprite (50,101)" (is_col t 50 101 9);
  check "SCREEN5: sprite right edge (57,101)" (is_col t 57 101 9);
  check "SCREEN5: outside (58,101)" (is_col t 58 101 0)

let () =
  (* 10. SCREEN6 (512폭) 은 스프라이트 픽셀 하나가 가로 두 픽셀이다 *)
  let t = Vdp.create () in
  set_reg t 0 0x08;
  set_reg t 1 0x60;
  set_reg t 2 0x1f;
  sprite_regs t;
  solid8 t 0;
  sat_w t 0 100 50 0;
  all_rows t 0 9;
  let c = is_col_w t 512 in
  check "SCREEN6: first sprite pixel is 2 wide (100,101)" (c 100 101 9);
  check "SCREEN6: first sprite pixel is 2 wide (101,101)" (c 101 101 9);
  check "SCREEN6: last pixel (115,101)" (c 115 101 9);
  check "SCREEN6: left of it (99,101) clear" (c 99 101 0);
  check "SCREEN6: right of it (116,101) clear" (c 116 101 0)

let () =
  (* 11. SCREEN7 은 CPU 주소가 물리 뱅크에 섞여 있어도 테이블을 제 주소로 읽는다 *)
  let t = Vdp.create () in
  set_reg t 0 0x0a;
  set_reg t 1 0x60;
  set_reg t 2 0x1f;
  sprite_regs t;
  solid8 t 0;
  sat_w t 0 100 50 0;
  all_rows t 0 9;
  let c = is_col_w t 512 in
  check "SCREEN7: sprite (100,101)" (c 100 101 9);
  check "SCREEN7: sprite (115,101)" (c 115 101 9);
  check "SCREEN7: (116,101) clear" (c 116 101 0)

let () =
  if !failures > 0 then begin
    Printf.eprintf "screen4_test: %d failure(s)\n%!" !failures;
    exit 1
  end;
  print_endline "screen4_test: all pass"
