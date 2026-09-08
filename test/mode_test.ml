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

  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "mode_test ok"
