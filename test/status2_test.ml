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
     마지막 줄의 1368 - 1284 + 202 = 286틱이다. 읽기는 다음 사이클 경계인
     48 사이클째(288틱)부터 그것을 본다.
   - frameStart: EO 를 뒤집고 palTiming 을 고정한다. 전원을 켤 때 한 번
     (VDP::reset), 그 뒤로 lines_per_frame 줄마다 온다.
   - 줄 카운터는 프레임 시작에서 8 + ((R#18 >> 4) ^ 7) 줄 뒤(중립 15)에
     리셋되고, 표시 줄 0 은 그때의 R#9 에 따라 17/27 줄(60Hz), 44/54
     줄(50Hz) 뒤다 (master scheduleLineCountReset, execLineCountReset).
     그래서 프레임 시작은 표시 줄 0 보다 32/42 줄(60Hz), 59/69 줄(50Hz)
     위다.
   - PAL_LINES 313, NTSC_LINES 262. statusReg2 의 초깃값 0x0C 를 전원
     투입의 frameStart 가 한 번 뒤집는다. *)

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

(* Move the beam forward, a cycle at a time, to the next (line, cycle):
   frames change length when R#9 or R#18 move display line 0. *)
let seek t line cycle =
  let budget = ref (2 * 313 * line_cycles) in
  while Vdp.line_now t <> (line, cycle) do
    if !budget = 0 then failwith (Printf.sprintf "the beam never reached %d:%d" line cycle);
    decr budget;
    ignore (Vdp.advance t ~cycles:1)
  done

let at t line cycle = seek t line cycle; t

(* SCREEN5 (G4), display on; 192 lines unless R#9 says otherwise. The
   power-on reset reads R#9, so the beam stops at display line 0 of a
   frame whose borders already follow it. *)
let fresh ?(r0 = 0x06) ?(r1 = 0x40) ?(r9 = 0) () =
  let t = Vdp.create () in
  reg t 0 r0;
  reg t 1 r1;
  reg t 9 r9;
  seek t 0 0;
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

  (* VR: 0 over the display, 1 from VBlank (48 cycles into line 192) until
     48 cycles into line 261 *)
  let t = fresh () in
  check "VR clear on display line 0" (not (vr (at t 0 100)));
  check "VR clear on the last display line" (not (vr (at t 191 227)));
  check "VR clear until VBlank (192 lines)" (not (vr (at t 192 47)));
  check "VR set with VBlank (192 lines)" (vr (at t 192 48));
  check "VR set in the bottom border" (vr (at t 230 0));
  check "VR set until cycle 47 of the last line" (vr (at t 261 47));
  check "VR clears 48 cycles into the last line" (not (vr (at t 261 48)));
  check "VR clear into display line 0" (not (vr (at t 0 0)));
  let t = fresh ~r9:0x80 () in
  check "VR clear on line 211 with 212 lines" (not (vr (at t 211 100)));
  check "VR clear at line 212 cycle 47 with 212 lines" (not (vr (at t 212 47)));
  check "VR set at line 212 with 212 lines" (vr (at t 212 48));

  (* Bits 3-2 read 1; EO is the power-on flip; nothing else is set on an
     idle VDP in the first display *)
  check "idle S#2 in the first display is 0x0E" (s2 (at (fresh ()) 50 100) = 0x0e);

  (* EO: power-on is a frame start, so EO reads 1 (openMSX 21.0 S#2 = 0x6E
     10 cycles after power-on). It flips at the frame start line, once a
     frame: 262 - 42 = 220 at 60 Hz with 192 lines, 262 - 32 = 230 with
     212. The beam powers on 86 ticks into line 220, where openMSX's frame
     tick 0 falls. *)
  let t = Vdp.create () in
  check "EO is 1 at power-on" (eo t);
  check "power-on beam at line 220, cycle 14" (Vdp.line_now t = (220, 14));
  let t = fresh () in
  check "EO unchanged just before the 60 Hz/192 frame start (line 220)" (eo (at t 219 227));
  check "EO flips at line 220" (not (eo (at t 220 0)));
  check "EO holds over display line 0" (not (eo (at t 100 0)));
  check "EO flips back one frame later" (eo (at t 220 0));
  let t = fresh ~r9:0x80 () in
  check "EO waits for line 230 with 212 lines" (eo (at t 229 0) && not (eo (at t 230 0)));

  (* PAL: the frame start latches R#9 bit 1 for its frame's length; the
     reset reads R#9 as it is. A 50 Hz request in the display reaches both
     in the same frame: the frame start at 220 latches 313 lines and the
     reset at 235 puts display line 0 54 lines later, at 289. The next frame
     start is then 220 + 313 - 289 = 244. *)
  let t = fresh () in
  reg t 9 0x02;
  ignore (at t 288 227);
  ignore (Vdp.advance t ~cycles:1);
  check "a 50 Hz request: display line 0 returns after line 288" (Vdp.line_now t = (0, 0));
  ignore (at t 300 0);
  check "PAL frame reaches line 300" (fst (Vdp.line_now t) = 300);
  check "VR set at line 300 (PAL)" (vr t);
  check "VR set until cycle 47 of line 312" (vr (at t 312 47));
  check "VR clears 48 cycles into line 312" (not (vr (at t 312 48)));
  ignore (at t 312 227);
  ignore (Vdp.advance t ~cycles:1);
  check "PAL counter wraps after line 312" (Vdp.line_now t = (0, 0));
  let before = eo t in
  check "PAL/192 frame start is line 244"
    (eo (at t 243 0) = before && eo (at t 244 0) <> before);
  (* Back to 60 Hz after the frame start: this frame keeps 313 lines, but
     its reset at 259 reads 27 lines, so display line 0 returns at 286 and
     the next frame start, 313 lines on, falls on line 244 + 313 - 286 =
     271. That one latches 262 lines and its reset at 286 puts display line
     0 at 313; the frame start after it is 271 + 262 - 313 = 220 again. *)
  reg t 9 0x00;
  ignore (at t 285 227);
  ignore (Vdp.advance t ~cycles:1);
  check "back to 60 Hz: display line 0 returns after line 285" (Vdp.line_now t = (0, 0));
  let before = eo t in
  check "back to 60 Hz: the next frame start is line 271"
    (eo (at t 270 0) = before && eo (at t 271 0) <> before);
  ignore (at t 312 227);
  ignore (Vdp.advance t ~cycles:1);
  check "its display line 0 returns after line 312" (Vdp.line_now t = (0, 0));
  let before = eo t in
  check "then the frame start is line 220 again"
    (eo (at t 219 0) = before && eo (at t 220 0) <> before);
  ignore (at t 261 227);
  ignore (Vdp.advance t ~cycles:1);
  check "and the frame is 262 lines" (Vdp.line_now t = (0, 0));

  (* Save and restore keep EO, the 50 Hz latch (format v7) and the frame
     start with the event ahead of it (format v14) *)
  let t = fresh ~r9:0x02 () in
  ignore (at t 100 0);
  ignore (at t 0 0);
  ignore (at t 250 0);
  let w = State_codec.writer () in
  Vdp.write_state w t;
  Vdp.write_appended w t;
  let r = State_codec.reader (State_codec.finish w) in
  let u = Vdp.create () in
  Vdp.read_state r u;
  Vdp.read_appended r u;
  State_codec.end_of_input r;
  check "restored EO" (eo u = eo t);
  check "restored line" (Vdp.line_now u = Vdp.line_now t);
  check "restored PAL keeps VR on line 300" (vr (at u 300 0));
  ignore (at u 312 227);
  ignore (Vdp.advance u ~cycles:1);
  check "restored PAL wraps after line 312" (Vdp.line_now u = (0, 0));
  let before = eo u in
  check "restored PAL frame start stays at 244"
    (eo (at u 243 0) = before && eo (at u 244 0) <> before);

  (* Line interrupt beyond the display: the counter runs from display line 0
     until it resets 15 lines below the frame start (220 + 15 = 235 at 60 Hz
     with 192 lines, 230 + 15 = 245 with 212, 244 + 15 / 254 + 15 at
     50 Hz). *)
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
  (* The power-on frame latched 60 Hz, so the first 50 Hz frame start is at
     203 and its reset at 218; the next frame has them at 254 and 269. *)
  let t = border ~r9:0x82 255 in
  ignore (at t 100 0);
  ignore (at t 0 0);
  check "PAL/212: FH on line 255 (reset at 269)" (fh (at t 256 0));

  if !failures > 0 then exit 1;
  print_endline "status2_test: all pass"
