(* S#2 VR·HR·EO 와 R#9 PAL 프레임 길이의 계약 증명. 포트 경로(0x99 레지스터
   쓰기, R#15 로 고른 상태 읽기)와 advance 로만 움직이고, 빔 위치는
   Vdp.line_now 로 맞춘다.

   기대값의 출처는 openMSX VDP.cc/VDP.hh 다. 이 모델의 한 줄은 오른쪽
   테두리에서 다음 오른쪽 테두리까지이고(라인 인터럽트 = scheduleHScan 의
   getRightBorder), Z80 한 사이클은 VDP 6틱이다.
   - getHR: 오른쪽 테두리부터 312틱(텍스트 모드 404틱) 동안 1
     → 52 사이클, 68 사이클(404/6 = 67.3 을 올림).
   - readStatusReg case 2: VR 은 displayStart - TICKS_PER_LINE 에서 0 이 된다.
     displayStart 는 표시 줄 0 의 틱 202 이므로 그 윗줄의 틱 202, 곧 우리
     마지막 줄의 1368 - 1282 + 202 = 288틱 = 48 사이클째다.
   - frameStart: EO 를 뒤집고 palTiming 을 고정한다. 프레임 시작은 표시
     줄 0 보다 8 + 17/27 줄(60Hz), 8 + 44/54 줄(50Hz) 위다
     (scheduleLineCountReset, execLineCountReset).
   - PAL_LINES 313, NTSC_LINES 262. statusReg2 의 초깃값 0x0C. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let reg t r v =
  Vdp.io_write t ~port:0x99 v;
  Vdp.io_write t ~port:0x99 (0x80 lor r)

let s2 t =
  reg t 15 2;
  let v = Vdp.io_read t ~port:0x99 in
  reg t 15 0;
  v

let vr t = s2 t land 0x40 <> 0
let hr t = s2 t land 0x20 <> 0
let eo t = s2 t land 0x02 <> 0

let line_cycles = 228

(* Move the beam forward to (line, cycle) of the current frame or, if that
   is behind it, of the next one. *)
let seek ?(frame = 262) t line cycle =
  let l, c = Vdp.line_now t in
  let now = (l * line_cycles) + c and target = (line * line_cycles) + cycle in
  let delta = if target >= now then target - now else (frame * line_cycles) - now + target in
  ignore (Vdp.advance t ~cycles:delta)

let at ?frame t line cycle = seek ?frame t line cycle; t

(* SCREEN5 (G4), display on; 192 lines unless R#9 says otherwise. *)
let fresh ?(r0 = 0x06) ?(r1 = 0x40) ?(r9 = 0) () =
  let t = Vdp.create () in
  reg t 0 r0;
  reg t 1 r1;
  reg t 9 r9;
  t

let () =
  (* HR: graphic modes, 52 cycles from the line boundary *)
  let t = fresh () in
  check "HR at the line boundary" (hr (at t 10 0));
  check "HR one cycle before the active part" (hr (at t 10 51));
  check "HR clears at cycle 52" (not (hr (at t 10 52)));
  check "HR stays clear to the end of the line" (not (hr (at t 10 227)));
  check "HR sets again on the next line" (hr (at t 11 0));
  check "HR flips in the bottom border too" (hr (at t 230 10) && not (hr (at t 230 60)));

  (* HR: text modes, 68 cycles (TEXT1, TEXT2, TEXT1Q) *)
  List.iter
    (fun (name, r0, r1) ->
      let t = fresh ~r0 ~r1 () in
      check (name ^ " HR at cycle 67") (hr (at t 20 67));
      check (name ^ " HR clears at cycle 68") (not (hr (at t 20 68))))
    [ ("TEXT1", 0x00, 0x50); ("TEXT2", 0x04, 0x50); ("TEXT1Q", 0x02, 0x50) ];
  check "GRAPHIC1 uses the graphic HR" (not (hr (at (fresh ~r0:0 ~r1:0x40 ()) 20 52)));

  (* VR: 0 over the display, 1 from VBlank until 48 cycles into line 261 *)
  let t = fresh () in
  check "VR clear on display line 0" (not (vr (at t 0 100)));
  check "VR clear on the last display line" (not (vr (at t 191 227)));
  check "VR set with VBlank (192 lines)" (vr (at t 192 0));
  check "VR set in the bottom border" (vr (at t 230 0));
  check "VR set until cycle 47 of the last line" (vr (at t 261 47));
  check "VR clears 48 cycles into the last line" (not (vr (at t 261 48)));
  check "VR clear into display line 0" (not (vr (at t 0 0)));
  let t = fresh ~r9:0x80 () in
  check "VR clear on line 211 with 212 lines" (not (vr (at t 211 100)));
  check "VR set at line 212 with 212 lines" (vr (at t 212 0));

  (* Bits 3-2 read 1; nothing else is set on an idle VDP in the display *)
  check "idle S#2 in the display is 0x0C" (s2 (at (fresh ()) 50 100) = 0x0c);

  (* EO: 0 at power-on, flips at the frame start line, once a frame *)
  let t = fresh () in
  check "EO starts at 0" (not (eo t));
  check "EO unchanged just before the 60 Hz/192 frame start (line 227)" (not (eo (at t 226 227)));
  check "EO flips at line 227" (eo (at t 227 0));
  check "EO holds over display line 0" (eo (at t 100 0));
  check "EO flips back one frame later" (not (eo (at t 227 0)));
  let t = fresh ~r9:0x80 () in
  check "EO waits for line 237 with 212 lines" (not (eo (at t 236 0)) && eo (at t 237 0));

  (* PAL: R#9 bit 1 takes effect when the line counter wraps *)
  let t = fresh ~r9:0x02 () in
  ignore (at t 100 0);
  check "a 50 Hz request leaves the running frame at 262 lines"
    (fst (Vdp.line_now (at t 261 227)) = 261);
  ignore (Vdp.advance t ~cycles:1);
  check "the counter wraps after line 261" (Vdp.line_now t = (0, 0));
  ignore (at ~frame:313 t 300 0);
  check "PAL frame reaches line 300" (fst (Vdp.line_now t) = 300);
  check "VR set at line 300 (PAL)" (vr t);
  check "VR set until cycle 47 of line 312" (vr (at ~frame:313 t 312 47));
  check "VR clears 48 cycles into line 312" (not (vr (at ~frame:313 t 312 48)));
  ignore (at ~frame:313 t 312 227);
  ignore (Vdp.advance t ~cycles:1);
  check "PAL counter wraps after line 312" (Vdp.line_now t = (0, 0));
  let before = eo t in
  check "PAL/192 frame start is line 251"
    (eo (at ~frame:313 t 250 0) = before && eo (at ~frame:313 t 251 0) <> before);
  reg t 9 0x00;
  ignore (at ~frame:313 t 312 227);
  ignore (Vdp.advance t ~cycles:1);
  ignore (at t 261 227);
  ignore (Vdp.advance t ~cycles:1);
  check "back to 60 Hz: 262 lines" (Vdp.line_now t = (0, 0));

  (* Save and restore keep EO and the 50 Hz latch (format v7) *)
  let t = fresh ~r9:0x02 () in
  ignore (at t 261 227);
  ignore (Vdp.advance t ~cycles:1);
  ignore (at ~frame:313 t 100 0);
  check "EO is 1 at the save point (flipped once, at line 227)" (eo t);
  let w = State_codec.writer () in
  Vdp.write_state w t;
  let r = State_codec.reader (State_codec.finish w) in
  let u = Vdp.create () in
  Vdp.read_state r u;
  State_codec.end_of_input r;
  check "restored EO" (eo u = eo t);
  check "restored line" (Vdp.line_now u = Vdp.line_now t);
  check "restored PAL keeps VR on line 300" (vr (at ~frame:313 u 300 0));
  ignore (at ~frame:313 u 312 227);
  ignore (Vdp.advance u ~cycles:1);
  check "restored PAL wraps after line 312" (Vdp.line_now u = (0, 0));

  (* Line interrupt beyond the display: the counter runs from display line 0
     until it resets 8 lines below the frame start (227 + 8 = 235 at 60 Hz
     with 192 lines, 245 with 212, 251 + 8 / 261 + 8 at 50 Hz). *)
  let fh t =
    reg t 15 1;
    let v = Vdp.io_read t ~port:0x99 in
    reg t 15 0;
    v land 1 = 1
  in
  let border ?(r9 = 0) ?(r23 = 0) r19 =
    let t = fresh ~r0:0x16 ~r9 () in
    reg t 19 r19;
    reg t 23 r23;
    t
  in
  let t = border 200 in
  check "no FH before line 200 ends" (not (fh (at t 200 227)));
  check "FH at the end of line 200 (bottom border)" (fh (at t 201 0));
  let t = border 234 in
  check "FH on line 234, the last before the reset" (fh (at t 235 0));
  let t = border 235 in
  check "no FH on line 235: the counter has reset" (not (fh (at t 261 227)));
  let t = border ~r9:0x80 244 in
  check "212 lines: FH on line 244" (fh (at t 245 0));
  let t = border ~r9:0x80 245 in
  check "212 lines: no FH on line 245" (not (fh (at t 261 227)));
  let t = border ~r23:56 0 in
  check "R#19 - R#23 wraps to line 200" (not (fh (at t 200 0)) && fh (at t 201 0));
  let t = border ~r9:0x82 255 in
  ignore (at t 261 227);
  ignore (Vdp.advance t ~cycles:1);
  check "PAL/212: FH on line 255 (reset at 269)" (fh (at ~frame:313 t 256 0));

  if !failures > 0 then exit 1;
  print_endline "status2_test: all pass"
