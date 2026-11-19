(* S#0 의 스프라이트 플래그(충돌 bit5, 5번째 스프라이트 bit6 + 하위 5비트
   번호)를 openMSX 와 맞춘다. 아래 표의 기대값은 같은 장면을 MSX2 카트리지 ROM
   으로 만들어(VDP 레지스터·VRAM 을 직접 쓰고 H.KEYI 훅에서 프레임 끝 상태를
   읽는다) openMSX(my8250cbios)에서 돌려 읽은 바이트다. 각 항목은
   (이름, 스프라이트 모드, 16×16, 추가 레지스터, [(Y, X, 패턴, 컬러)], 기대 S#0).
   모드 2 의 네 번째 값은 모든 패턴 행에 쓰는 컬러 바이트다(CC=0x40, IC=0x20,
   EC=0x80). 읽기는 플래그를 지우므로 프레임을 두 번 돌려 두 번째 읽기를 본다. *)

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

let cycles_per_frame = 228 * 262

let scenarios =
  [
  ("m1_four_apart", 1, false, [], [ (50, 0, 0, 9); (50, 20, 0, 9); (50, 40, 0, 9); (50, 60, 0, 9) ], 0x84);
  ("m1_five_apart", 1, false, [], [ (50, 0, 0, 9); (50, 20, 0, 9); (50, 40, 0, 9); (50, 60, 0, 9); (50, 80, 0, 9) ], 0xc4);
  ("m1_overlap", 1, false, [], [ (50, 10, 0, 9); (50, 14, 0, 10) ], 0xa2);
  ("m1_overlap_c0", 1, false, [], [ (50, 10, 0, 9); (50, 14, 0, 0) ], 0x82);
  ("m1_adjacent", 1, false, [], [ (50, 10, 0, 9); (50, 18, 0, 10) ], 0x82);
  ("m1_disjoint_bits", 1, false, [], [ (50, 10, 1, 9); (50, 10, 2, 10) ], 0x82);
  ("m1_fifth_overlaps_first", 1, false, [], [ (50, 0, 0, 9); (50, 20, 0, 9); (50, 40, 0, 9); (50, 60, 0, 9); (50, 0, 0, 10) ], 0xc4);
  ("m1_vertical_partial", 1, false, [], [ (50, 10, 0, 9); (56, 14, 0, 10) ], 0xa2);
  ("m1_five_first_two_lines", 1, false, [], [ (50, 0, 0, 9); (50, 20, 0, 9); (50, 40, 0, 9); (50, 60, 0, 9); (50, 80, 0, 9); (100, 0, 0, 9); (100, 20, 0, 9); (100, 40, 0, 9); (100, 60, 0, 9); (100, 80, 0, 9); (100, 100, 0, 9) ], 0xc4);
  ("m1_none", 1, false, [], [ (50, 10, 0, 9) ], 0x81);
  ("m1_ec_overlap", 1, false, [], [ (50, 10, 0, 9); (50, 42, 0, 138) ], 0xa2);
  ("m2_overlap", 2, false, [], [ (50, 10, 0, 9); (50, 14, 0, 10) ], 0xa2);
  ("m2_overlap_ic", 2, false, [], [ (50, 10, 0, 9); (50, 14, 0, 42) ], 0x82);
  ("m2_overlap_c0", 2, false, [], [ (50, 10, 0, 9); (50, 14, 0, 0) ], 0x82);
  ("m2_eight", 2, false, [], [ (50, 0, 0, 9); (50, 20, 0, 9); (50, 40, 0, 9); (50, 60, 0, 9); (50, 80, 0, 9); (50, 100, 0, 9); (50, 120, 0, 9); (50, 140, 0, 9) ], 0x88);
  ("m2_nine", 2, false, [], [ (50, 0, 0, 9); (50, 20, 0, 9); (50, 40, 0, 9); (50, 60, 0, 9); (50, 80, 0, 9); (50, 100, 0, 9); (50, 120, 0, 9); (50, 140, 0, 9); (50, 160, 0, 9) ], 0xc8);
  ("m2_nine_overlap9", 2, false, [], [ (50, 0, 0, 9); (50, 20, 0, 9); (50, 40, 0, 9); (50, 60, 0, 9); (50, 80, 0, 9); (50, 100, 0, 9); (50, 120, 0, 9); (50, 140, 0, 9); (50, 0, 0, 10) ], 0xc8);
  ("m2_cc_overlap", 2, false, [], [ (50, 10, 0, 1); (50, 14, 0, 66) ], 0x82);
  ("m2_ic_first", 2, false, [], [ (50, 10, 0, 41); (50, 14, 0, 10) ], 0x82);
  ("m2_cc_first", 2, false, [], [ (50, 10, 0, 65); (50, 14, 0, 2) ], 0x82);
  ("m2_cc_middle", 2, false, [], [ (50, 10, 0, 1); (50, 14, 0, 66); (50, 18, 0, 4) ], 0x83);
  ("m2_c0_tp1", 2, false, [ (8, 0x20) ], [ (50, 10, 0, 9); (50, 14, 0, 0) ], 0xa2);
  ("m2_spd", 2, false, [ (8, 0x02) ], [ (50, 10, 0, 9); (50, 14, 0, 10) ], 0x9f);
  ("m1_spd", 1, false, [ (8, 0x02) ], [ (50, 10, 0, 9); (50, 14, 0, 10) ], 0x9f);
  ("m1_16_overlap", 1, true, [], [ (50, 10, 0, 9); (60, 20, 0, 10) ], 0xa2);
  ("m1_16_five", 1, true, [], [ (50, 0, 0, 9); (50, 20, 0, 9); (50, 40, 0, 9); (50, 60, 0, 9); (50, 80, 0, 9) ], 0xc4);
  ("m1_mag_overlap", 1, false, [ (1, 0xe1) ], [ (50, 10, 0, 9); (50, 24, 0, 10) ], 0xa2);
  ("m2_nine_first_hidden_collide", 2, false, [], [ (50, 0, 0, 9); (50, 20, 0, 9); (50, 40, 0, 9); (50, 60, 0, 9); (50, 80, 0, 9); (50, 100, 0, 9); (50, 120, 0, 9); (50, 140, 0, 9); (50, 0, 0, 10) ], 0xc8);
  ("m1_c_then_five", 1, false, [], [ (50, 10, 0, 9); (50, 14, 0, 10); (50, 40, 0, 9); (50, 60, 0, 9); (50, 80, 0, 9) ], 0xe4);
  ("m2_sprite_after_line", 2, false, [], [ (50, 10, 0, 9); (60, 0, 0, 9); (60, 8, 0, 9); (60, 16, 0, 9); (60, 24, 0, 9); (60, 32, 0, 9); (60, 40, 0, 9); (60, 48, 0, 9); (60, 56, 0, 9); (60, 64, 0, 9) ], 0xc9);
  ("m1_32_noterm", 1, false, [], [ (10, 0, 0, 9); (16, 30, 0, 9); (22, 60, 0, 9); (28, 90, 0, 9); (34, 120, 0, 9); (40, 150, 0, 9); (46, 180, 0, 9); (52, 210, 0, 9); (58, 0, 0, 9); (64, 30, 0, 9); (70, 60, 0, 9); (76, 90, 0, 9); (82, 120, 0, 9); (88, 150, 0, 9); (94, 180, 0, 9); (100, 210, 0, 9); (106, 0, 0, 9); (112, 30, 0, 9); (118, 60, 0, 9); (124, 90, 0, 9); (130, 120, 0, 9); (136, 150, 0, 9); (142, 180, 0, 9); (148, 210, 0, 9); (154, 0, 0, 9); (160, 30, 0, 9); (166, 60, 0, 9); (172, 90, 0, 9); (178, 120, 0, 9); (184, 150, 0, 9); (190, 180, 0, 9); (196, 210, 0, 9) ], 0x9f);
  ("m1_term0", 1, false, [], [], 0x80);
  ("m1_term7_offscreen", 1, false, [], [ (200, 10, 0, 9); (201, 10, 0, 9); (202, 10, 0, 9); (203, 10, 0, 9); (204, 10, 0, 9); (205, 10, 0, 9); (206, 10, 0, 9) ], 0x87);
  ("m1_five_below_192", 1, false, [], [ (195, 0, 0, 9); (195, 20, 0, 9); (195, 40, 0, 9); (195, 60, 0, 9); (195, 80, 0, 9) ], 0x85);
  ("m1_five_below_192_r9_212", 1, false, [ (9, 0x80) ], [ (195, 0, 0, 9); (195, 20, 0, 9); (195, 40, 0, 9); (195, 60, 0, 9); (195, 80, 0, 9) ], 0xc4);
  ("m1_overlap_blank", 1, false, [ (1, 0xa0) ], [ (50, 10, 0, 9); (50, 14, 0, 10) ], 0x9f);
  ("m1_overlap_text1", 1, false, [ (1, 0xf0) ], [ (50, 10, 0, 9); (50, 14, 0, 10) ], 0x9f);
  ("m1_c0_tp1", 1, false, [ (8, 0x20) ], [ (50, 10, 0, 9); (50, 14, 0, 0) ], 0xa2);
  ("m2_cc_between", 2, false, [], [ (50, 10, 0, 1); (80, 100, 0, 66); (50, 14, 0, 3) ], 0xa3);
  ("m2_32_noterm", 2, false, [], [ (10, 0, 0, 9); (16, 30, 0, 9); (22, 60, 0, 9); (28, 90, 0, 9); (34, 120, 0, 9); (40, 150, 0, 9); (46, 180, 0, 9); (52, 210, 0, 9); (58, 0, 0, 9); (64, 30, 0, 9); (70, 60, 0, 9); (76, 90, 0, 9); (82, 120, 0, 9); (88, 150, 0, 9); (94, 180, 0, 9); (100, 210, 0, 9); (106, 0, 0, 9); (112, 30, 0, 9); (118, 60, 0, 9); (124, 90, 0, 9); (130, 120, 0, 9); (136, 150, 0, 9); (142, 180, 0, 9); (148, 210, 0, 9); (154, 0, 0, 9); (160, 30, 0, 9); (166, 60, 0, 9); (172, 90, 0, 9); (178, 120, 0, 9); (184, 150, 0, 9); (190, 180, 0, 9); (196, 210, 0, 9) ], 0x9f);
  ("m2_overlap_spd0_tp1_c0both", 2, false, [ (8, 0x20) ], [ (50, 10, 0, 0); (50, 14, 0, 0) ], 0xa2);
  ("m2_overlap_x_over_255", 2, false, [], [ (50, 250, 0, 9); (50, 254, 0, 10) ], 0xa2);
  ("m1_overlap_x_over_255", 1, false, [], [ (50, 250, 0, 9); (50, 254, 0, 10) ], 0xa2);
  ("m1_four_then_overlap5th_on_second", 1, false, [], [ (50, 0, 0, 9); (50, 20, 0, 9); (50, 40, 0, 9); (50, 60, 0, 9); (52, 60, 0, 10) ], 0xc4);
  ("m1_sprite_y_d1", 1, false, [], [ (209, 10, 0, 9); (209, 14, 0, 10) ], 0x82);
  ("m2_16_overlap_mag", 2, true, [ (1, 0xe3) ], [ (50, 10, 0, 9); (50, 26, 0, 10) ], 0xa2);
  ("m1_ec_offscreen_overlap", 1, false, [], [ (50, 10, 0, 137); (50, 12, 0, 138) ], 0x82);
  ("m2_ec_offscreen_overlap", 2, false, [], [ (50, 10, 0, 137); (50, 12, 0, 138) ], 0x82);
  ("m1_col0_first", 1, false, [ (8, 0x00) ], [ (50, 10, 0, 0); (50, 10, 0, 10) ], 0x82);

  ]

let status_after_two_frames (mode, size16, regs, sprites) =
  let t = Vdp.create () in
  let r1 = 0xe0 lor if size16 then 2 else 0 in
  let base =
    if mode = 1 then
      [ (0, 0); (1, r1); (2, 6); (3, 0x80); (4, 0); (5, 0x76); (6, 7); (7, 4) ]
    else
      [ (0, 4); (1, r1); (2, 6); (3, 0xff); (4, 3); (5, 0x3f); (6, 7); (7, 4); (8, 0) ]
  in
  List.iter (fun (r, v) -> set_reg t r v) (base @ regs);
  let n = if size16 then 32 else 8 in
  List.iteri
    (fun p b -> for k = 0 to n - 1 do vram_w t (0x3800 + (p * n) + k) b done)
    [ 0xff; 0xf0; 0x0f; 0xff ];
  if mode = 1 then begin
    List.iteri
      (fun i (y, x, p, c) ->
        List.iteri (fun k v -> vram_w t (0x3b00 + (i * 4) + k) v) [ y; x; p; c ])
      sprites;
    vram_w t (0x3b00 + (List.length sprites * 4)) 0xd0
  end
  else begin
    List.iteri
      (fun i (y, x, p, cb) ->
        for r = 0 to 15 do vram_w t (0x1c00 + (i * 16) + r) cb done;
        List.iteri (fun k v -> vram_w t (0x1e00 + (i * 4) + k) v) [ y; x; p; 0 ])
      sprites;
    vram_w t (0x1e00 + (List.length sprites * 4)) 0xd8
  end;
  ignore (Vdp.advance t ~cycles:cycles_per_frame);
  ignore (Vdp.io_read t ~port:0x99);
  ignore (Vdp.advance t ~cycles:cycles_per_frame);
  Vdp.io_read t ~port:0x99

let () =
  (* CPU 가 인터럽트를 받기 전에는 R#15 를 건드리지 않는다. DI 구간에서 S#2 를
     고른 채 VBlank 가 지나가도 선택이 남는다(MSXgl 의 CE 대기 루프는 S#2 의
     bit0 을 RRA 로 본다 — S#0 을 읽게 되면 S#0 의 하위 비트가 CE 로 읽힌다). *)
  let t = Vdp.create () in
  set_reg t 15 2;
  ignore (Vdp.advance t ~cycles:cycles_per_frame);
  check "VBlank without an accepted interrupt keeps the S#2 selection"
    (Vdp.io_read t ~port:0x99 land 0x7e = 0x7e);
  set_reg t 15 2;
  ignore (Vdp.advance t ~cycles:cycles_per_frame);
  Vdp.interrupt_accepted t;
  check "an accepted VBlank interrupt selects S#0 again"
    (Vdp.io_read t ~port:0x99 land 0x80 = 0x80)

let () =
  List.iter
    (fun (name, mode, size16, regs, sprites, expect) ->
      let got = status_after_two_frames (mode, size16, regs, sprites) in
      check (Printf.sprintf "%s: S#0 = 0x%02x, openMSX 0x%02x" name got expect) (got = expect))
    scenarios;
  if !failures > 0 then begin
    Printf.eprintf "sprite_flags_test: %d failure(s)\n%!" !failures;
    exit 1
  end;
  Printf.printf "sprite_flags_test: all pass (%d scenarios)\n" (List.length scenarios)
