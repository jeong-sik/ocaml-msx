(* 키보드 매트릭스와 조이스틱 포트 계약 증명. 전부 Z80 이 보는 I/O 포트
   경로(0xAA 행 선택 → 0xA9 읽기, 0xA0 래치 → 0xA2 읽기)로 판정한다.
   기대값의 정본: openMSX unicodemap.int (행/비트), MSXPSG::readA +
   DummyJoystick (R#14 idle = 0x3F). ROM 없이 돈다. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let machine () =
  Msx.create ~machine:{ Msx.ram_kb = 64; vram_kb = 128; roms = [ ""; ""; "" ] }

(* 상위 니블은 PPI C 의 다른 신호(카세트·클릭·LED) — 행 선택에 섞이면 안 된다. *)
let row_read t row =
  Msx.port_out t 0xAA (0x50 lor row);
  Msx.port_in t 0xA9

let all_rows_idle t name =
  for r = 0 to 10 do
    check (Printf.sprintf "%s: row %d idle" name r) (row_read t r = 0xff)
  done

let () =
  let t = machine () in
  all_rows_idle t "fresh";
  check "row 11+ reads 0xff" (row_read t 11 = 0xff && row_read t 15 = 0xff);

  (* 행 8: SPACE bit0 · LEFT 4 · UP 5 · DOWN 6 · RIGHT 7 — C-BIOS gtstck 이
     행 8 상위 니블을 RDUL 로 읽고 gttrig 이 bit0 을 스페이스로 읽는다. *)
  check "Space maps" (Msx.set_key t Space ~pressed:true);
  check "Space = row 8 bit 0" (row_read t 8 = 0xfe);
  check "Space leaves row 7 alone" (row_read t 7 = 0xff);
  check "Up maps" (Msx.set_key t Up ~pressed:true);
  check "Space+Up" (row_read t 8 = 0xde);
  ignore (Msx.set_key t Space ~pressed:false);
  ignore (Msx.set_key t Up ~pressed:false);
  all_rows_idle t "after release";
  ignore (Msx.set_key t Left ~pressed:true);
  ignore (Msx.set_key t Down ~pressed:true);
  ignore (Msx.set_key t Right ~pressed:true);
  check "Left+Down+Right = bits 4,6,7" (row_read t 8 = 0x2f);
  ignore (Msx.set_key t Left ~pressed:false);
  ignore (Msx.set_key t Down ~pressed:false);
  ignore (Msx.set_key t Right ~pressed:false);

  (* 글자: A/B 는 행 2 bit 6/7, C-J 행 3, K-R 행 4, S-Z 행 5. 대소문자 동일. *)
  ignore (Msx.set_key t (Char 'a') ~pressed:true);
  check "a = row 2 bit 6" (row_read t 2 = 0xbf);
  ignore (Msx.set_key t (Char 'a') ~pressed:false);
  ignore (Msx.set_key t (Char 'A') ~pressed:true);
  check "A = same key as a" (row_read t 2 = 0xbf);
  ignore (Msx.set_key t (Char 'A') ~pressed:false);
  ignore (Msx.set_key t (Char 'm') ~pressed:true);
  check "m = row 4 bit 2" (row_read t 4 = 0xfb);
  ignore (Msx.set_key t (Char 'm') ~pressed:false);
  ignore (Msx.set_key t (Char 'z') ~pressed:true);
  check "z = row 5 bit 7" (row_read t 5 = 0x7f);
  ignore (Msx.set_key t (Char 'z') ~pressed:false);
  ignore (Msx.set_key t (Char 'c') ~pressed:true);
  check "c = row 3 bit 0" (row_read t 3 = 0xfe);
  ignore (Msx.set_key t (Char 'c') ~pressed:false);

  (* 숫자·기호: 0-7 행 0, 8·9 행 1 bit 0/1, '-' 행 1 bit 2, ';' 행 1 bit 7. *)
  ignore (Msx.set_key t (Char '0') ~pressed:true);
  ignore (Msx.set_key t (Char '7') ~pressed:true);
  check "0 and 7 = row 0 bits 0,7" (row_read t 0 = 0x7e);
  ignore (Msx.set_key t (Char '0') ~pressed:false);
  ignore (Msx.set_key t (Char '7') ~pressed:false);
  ignore (Msx.set_key t (Char '8') ~pressed:true);
  ignore (Msx.set_key t (Char ';') ~pressed:true);
  check "8 and ; = row 1 bits 0,7" (row_read t 1 = 0x7e);
  ignore (Msx.set_key t (Char '8') ~pressed:false);
  ignore (Msx.set_key t (Char ';') ~pressed:false);

  (* 행 6/7: F1-F3 행 6 bit 5-7, F4·F5 행 7 bit 0·1, ESC 행 7 bit 2, RETURN 행 7 bit 7. *)
  ignore (Msx.set_key t (Function 1) ~pressed:true);
  check "F1 = row 6 bit 5" (row_read t 6 = 0xdf);
  ignore (Msx.set_key t (Function 1) ~pressed:false);
  ignore (Msx.set_key t (Function 4) ~pressed:true);
  ignore (Msx.set_key t Esc ~pressed:true);
  ignore (Msx.set_key t Return ~pressed:true);
  check "F4+ESC+RETURN = row 7 bits 0,2,7" (row_read t 7 = 0x7a);
  ignore (Msx.set_key t (Function 4) ~pressed:false);
  ignore (Msx.set_key t Esc ~pressed:false);
  ignore (Msx.set_key t Return ~pressed:false);

  (* 모디파이어: SHIFT 행 6 비트 0 · CTRL 비트 1 · GRAPH 비트 2. 단독으로는
     매트릭스만 친다. SHIFT+글자 는 chord(동시 누름)로 만든다 — BIOS·게임이
     행 6 과 글자 행을 함께 스캔한다. *)
  ignore (Msx.set_key t Shift ~pressed:true);
  check "Shift = row 6 bit 0" (row_read t 6 = 0xfe);
  ignore (Msx.set_key t Ctrl ~pressed:true);
  ignore (Msx.set_key t Graph ~pressed:true);
  check "Shift+Ctrl+Graph = row 6 bits 0,1,2" (row_read t 6 = 0xf8);
  ignore (Msx.set_key t Ctrl ~pressed:false);
  ignore (Msx.set_key t Graph ~pressed:false);
  ignore (Msx.set_key t (Char 'a') ~pressed:true);
  check "Shift held + a = row 6 bit 0 and row 2 bit 6"
    (row_read t 6 = 0xfe && row_read t 2 = 0xbf);
  ignore (Msx.set_key t Shift ~pressed:false);
  ignore (Msx.set_key t (Char 'a') ~pressed:false);
  all_rows_idle t "end of keyboard";
  check "Backspace maps" (Msx.set_key t Backspace ~pressed:true);
  check "Backspace = row 7 bit 5" (row_read t 7 = 0xdf);
  check "Backspace leaves cursor row alone" (row_read t 8 = 0xff);
  ignore (Msx.set_key t Return ~pressed:true);
  check "Backspace and Return independent" (row_read t 7 = 0x5f);
  ignore (Msx.set_key t Backspace ~pressed:false);
  check "Backspace release leaves Return" (row_read t 7 = 0x7f);
  ignore (Msx.set_key t Return ~pressed:false);
  all_rows_idle t "after editing keys";

  (* 자리 없는 키는 false 이고 매트릭스를 건드리지 않는다. *)
  check "'!' unmapped" (not (Msx.set_key t (Char '!') ~pressed:true));
  check "F9 unmapped" (not (Msx.set_key t (Function 9) ~pressed:true));
  all_rows_idle t "after unmapped";

  (* PSG R#14: 빈 조이스틱 = 0x3F. Trigger_a/b = bit 4/5. R#15 bit6 = 포트 2 선택. *)
  let psg_read t r =
    Msx.port_out t 0xA0 r;
    Msx.port_in t 0xA2
  in
  let psg_write t r v =
    Msx.port_out t 0xA0 r;
    Msx.port_out t 0xA1 v
  in
  check "R#14 idle = 0x3F (not the written value)" (psg_read t 14 = 0x3f);
  check "Trigger_a maps" (Msx.set_key t Trigger_a ~pressed:true);
  check "Trigger_a clears bit 4" (psg_read t 14 = 0x2f);
  ignore (Msx.set_key t Trigger_b ~pressed:true);
  check "Trigger_a+b clear bits 4,5" (psg_read t 14 = 0x0f);
  check "triggers do not touch row 8" (row_read t 8 = 0xff);
  psg_write t 15 0x4f;
  check "port 2 selected reads empty" (psg_read t 14 = 0x3f);
  psg_write t 15 0x0f;
  check "port 1 again" (psg_read t 14 = 0x0f);
  ignore (Msx.set_key t Trigger_a ~pressed:false);
  ignore (Msx.set_key t Trigger_b ~pressed:false);
  check "released = 0x3F" (psg_read t 14 = 0x3f);

  (* 방향키는 커서(매트릭스 행 8)와 조이스틱 방향 비트를 함께 구동한다.
     openMSX JoystickDevice: UP=0x01 DOWN=0x02 LEFT=0x04 RIGHT=0x08 (active low).
     GTSTCK(0) 게임은 행 8 을, GTSTCK(1)/PSG 직독 게임은 R#14 를 읽는다 —
     한 키가 두 경로를 모두 채운다. *)
  ignore (Msx.set_key t Up ~pressed:true);
  check "Up clears joy1 bit 0" (psg_read t 14 = 0x3e);
  check "Up still drives cursor row 8 bit 5" (row_read t 8 = 0xdf);
  ignore (Msx.set_key t Up ~pressed:false);
  check "Up release restores joy1 and row 8"
    (psg_read t 14 = 0x3f && row_read t 8 = 0xff);
  ignore (Msx.set_key t Left ~pressed:true);
  ignore (Msx.set_key t Down ~pressed:true);
  check "Left+Down clear joy1 bits 2,1" (psg_read t 14 = 0x39);
  check "Left+Down still drive row 8 bits 4,6" (row_read t 8 = 0xaf);
  ignore (Msx.set_key t Left ~pressed:false);
  ignore (Msx.set_key t Down ~pressed:false);
  check "directions released = idle" (psg_read t 14 = 0x3f && row_read t 8 = 0xff);

  psg_write t 7 0xb8;
  check "other registers read back" (psg_read t 7 = 0xb8);

  (* 두 머신은 PSG·키 상태를 공유하지 않는다. *)
  let u = machine () in
  ignore (Msx.set_key t Space ~pressed:true);
  check "second machine sees no keys" (row_read u 8 = 0xff);
  check "second machine PSG idle" (psg_read u 14 = 0x3f);

  (* i8255 제어 포트 0xAB. 기대값의 정본: openMSX 21.0 I8255.cc
     (writeControlPort·readPortA/B/C) 와 MSXPPI.cc (peekA = 0, 입력 C 니블 =
     0xF). 머신은 BIOS 가 쓰는 모드 0x82 로 시작한다. *)
  let p = machine () in
  ignore (Msx.set_key p Space ~pressed:true);
  check "starts in mode 0x82" (Msx.port_in p 0xAB = 0x82);
  check "mode 0x82: port B is the keyboard" (row_read p 8 = 0xfe);
  (* 모드 0x80: 포트 B 도 출력 — 키보드 대신 래치(전원 투입 0)를 읽는다.
     Hole In One Special 이 이 모드를 쓰고 행 8 을 0x00 으로 읽는다. *)
  Msx.port_out p 0xAB 0x80;
  check "mode 0x80: port B reads its latch, not the keys" (row_read p 8 = 0x00);
  Msx.port_out p 0xA9 0x5a;
  check "mode 0x80: a port B write is read back" (Msx.port_in p 0xA9 = 0x5a);
  Msx.port_out p 0xAB 0x82;
  check "back to 0x82: the keyboard again" (row_read p 8 = 0xfe);
  (* 비트 set/reset: bit 0 이 값, bit 3-1 이 포트 C 비트 번호. *)
  Msx.port_out p 0xAA 0x00;
  Msx.port_out p 0xAB 0x0f;
  check "set C bit 7" (Msx.port_in p 0xAA = 0x80);
  Msx.port_out p 0xAB 0x07;
  check "set C bit 3 selects row 8" (Msx.port_in p 0xAA = 0x88 && Msx.port_in p 0xA9 = 0xfe);
  Msx.port_out p 0xAB 0x0e;
  check "reset C bit 7" (Msx.port_in p 0xAA = 0x08);
  (* 포트 C 하위가 입력(모드 0x83): 0xF 로 읽히고, 키보드 행은 마지막으로
     내보낸 값(8)에 머문다. 그 사이 C 쓰기는 래치만 바꾼다. *)
  Msx.port_out p 0xAB 0x83;
  check "C low input reads 0xF" (Msx.port_in p 0xAA = 0x0f);
  Msx.port_out p 0xAA 0x02;
  check "C low input: the row stays 8" (Msx.port_in p 0xA9 = 0xfe);
  Msx.port_out p 0xAB 0x82;
  check "C low output again: the latch picks row 2" (Msx.port_in p 0xA9 = 0xff
                                                    && Msx.port_in p 0xAA = 0x02);
  (* 포트 A 가 입력(모드 0x92): 0 으로 읽히고, 쓰기는 래치에만 남아 슬롯이
     그대로다. 다시 출력이 되면 래치가 슬롯을 고른다. *)
  Msx.port_out p 0xA8 0xf0;
  Msx.port_out p 0xAB 0x92;
  check "A input reads 0" (Msx.port_in p 0xA8 = 0x00);
  Msx.port_out p 0xA8 0x0f;
  check "A input: the slots keep the last output" (Msx.ppi_a p = 0xf0);
  Msx.port_out p 0xAB 0x82;
  check "A output again: the latch drives the slots"
    (Msx.ppi_a p = 0x0f && Msx.port_in p 0xA8 = 0x0f);
  (* 모드 1·2 는 MSX 에 없다: openMSX 는 그 포트를 0xFF 로 읽는다. *)
  Msx.port_out p 0xAB 0xa2;
  check "group A mode 1 reads 0xFF" (Msx.port_in p 0xA8 = 0xff);
  Msx.port_out p 0xAB 0x86;
  check "group B mode 1 reads 0xFF" (Msx.port_in p 0xA9 = 0xff);
  (* 세이브: 제어 워드와 B 래치가 살아남는다. *)
  Msx.port_out p 0xAB 0x80;
  Msx.port_out p 0xA9 0x3c;
  (match Msx.restore ~state:(Msx.serialize p) with
   | Error e -> check ("PPI state restores: " ^ e) false
   | Ok q ->
     check "restored control word" (Msx.port_in q 0xAB = 0x80);
     check "restored port B latch" (Msx.port_in q 0xA9 = 0x3c));

  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "keyboard_test ok"
