(* 화면 모드 판독과 VRAM 읽기 — 전부 포트 경로. 기대 코드는 V9938 모드표
   (openMSX DisplayMode base 코드와 같은 배치). *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let machine () =
  Msx.create ~machine:{ Msx.ram_kb = 64; vram_kb = 128; roms = [ ""; ""; "" ] }

let set_reg t r v =
  Msx.port_out t 0x99 v;
  Msx.port_out t 0x99 (0x80 lor r)

(* (R#0, R#1) → 모드. R#1 은 화면 켬(bit6)을 함께 둔다 — 모드 비트만 본다. *)
let cases =
  [ (0x00, 0x40, Msx.Graphic1, "GRAPHIC1")
  ; (0x02, 0x40, Msx.Graphic2, "GRAPHIC2")
  ; (0x04, 0x40, Msx.Graphic3, "GRAPHIC3")
  ; (0x06, 0x40, Msx.Graphic4, "GRAPHIC4")
  ; (0x08, 0x40, Msx.Graphic5, "GRAPHIC5")
  ; (0x0a, 0x40, Msx.Graphic6, "GRAPHIC6")
  ; (0x0e, 0x40, Msx.Graphic7, "GRAPHIC7")
  ; (0x00, 0x48, Msx.Multicolor, "MULTICOLOR")
  ; (0x00, 0x50, Msx.Text1, "TEXT1")
  ; (0x04, 0x50, Msx.Text2, "TEXT2")
  ]

let () =
  let t = machine () in
  List.iter
    (fun (r0, r1, expected, name) ->
      set_reg t 0 r0;
      set_reg t 1 r1;
      check (name ^ " decoded") (Msx.display_mode t = expected);
      check (name ^ " named") (Msx.display_mode_to_string (Msx.display_mode t) = name))
    cases;
  (* M1+M3 는 표에 없다: 코드 0x05 그대로. *)
  set_reg t 0 0x02;
  set_reg t 1 0x50;
  check "M1+M3 is Undefined 0x05" (Msx.display_mode t = Msx.Undefined 0x05);
  check "Undefined names its code"
    (Msx.display_mode_to_string (Msx.display_mode t) = "UNDEFINED(0x05)");
  (* 다른 R#0/R#1 비트(IE·MAG·SIZE·BL)는 모드에 안 섞인다. *)
  set_reg t 0 0x12;
  set_reg t 1 0xe3;
  check "non-mode bits ignored" (Msx.display_mode t = Msx.Graphic2);

  (* VRAM: 0x99 래치(write) 후 0x98 데이터 → vram_read 로 그 자리에서 읽힌다. *)
  Msx.port_out t 0x99 0x34;
  Msx.port_out t 0x99 (0x40 lor 0x12);
  Msx.port_out t 0x98 0xab;
  Msx.port_out t 0x98 0xcd;
  check "vram_read 0x1234" (Msx.vram_read t 0x1234 = 0xab);
  check "vram_read autoincrement" (Msx.vram_read t 0x1235 = 0xcd);
  check "vram_read wraps at 17 bits" (Msx.vram_read t (0x1234 + 0x20000) = 0xab);

  (* 512폭 비트맵(G5/G6/G7)의 2:1 축소 — 캔버스 한 점은 원본 인접
     2픽셀(2x, 2x+1)의 RGB 평그이어야 한다. 한쪽만 고르면 50% 디더링이
     moiré 로 남는다 (삼국지2 SCREEN7 타이틀 실측). *)
  let frame_px rgb x y =
    let i = (y * 256 + x) * 3 in
    (Char.code rgb.[i], Char.code rgb.[i + 1], Char.code rgb.[i + 2])
  in
  let vram_seek a =
    Msx.port_out t 0x99 (a land 0xff);
    Msx.port_out t 0x99 (0x40 lor ((a lsr 8) land 0x3f))
  in
  (* 팔레트 색 1 을 흰색(7,7,7) 으로 — 기본 팔레트의 1 번은 검정이라
     평균 검사가 구분을 못 한다. *)
  set_reg t 16 1;
  Msx.port_out t 0x9a 0x77;
  Msx.port_out t 0x9a 0x07;
  (* GRAPHIC6: VRAM 0 = 0x01 → 좌 색0(검정) / 우 색1(흰색).
     캔버스 (0,0) 은 그 둘의 평균 (127,127,127), (1,0) 은 다음 바이트
     0x00 의 평균 (0,0,0). *)
  set_reg t 0 0x0a; set_reg t 1 0x40;
  vram_seek 0x0000;
  Msx.port_out t 0x98 0x01;
  let rgb = Msx.frame_rgb t in
  check "G6 pair average" (frame_px rgb 0 0 = (127, 127, 127));
  check "G6 next byte black" (frame_px rgb 1 0 = (0, 0, 0));
  (* GRAPHIC5: VRAM 0 = 0x40 → 2bpp 픽셀0 = 색1(흰), 픽셀1 = 색0(검). *)
  set_reg t 0 0x08;
  vram_seek 0x0000;
  Msx.port_out t 0x98 0x40;
  let rgb = Msx.frame_rgb t in
  check "G5 pair average" (frame_px rgb 0 0 = (127, 127, 127));
  (* GRAPHIC7: VRAM 0 = 0xE0(R7), 1 = 0x00 → R 평균 127. *)
  set_reg t 0 0x0e;
  vram_seek 0x0000;
  Msx.port_out t 0x98 0xe0;
  Msx.port_out t 0x98 0x00;
  let rgb = Msx.frame_rgb t in
  check "G7 pair average" (frame_px rgb 0 0 = (127, 0, 0));

  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "mode_test ok"
