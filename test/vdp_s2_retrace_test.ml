(* S#2 HR (bit 5) and VR (bit 6) follow the beam instead of reading 1 for
   ever (issue #60). A loop that waits for HR or VR to drop never ended, and
   Europa Sensen's intro, which polls HR before it writes the VDP, never drew
   a frame.

   Expected values come from openMSX VDP.cc peekStatusReg case 2 and
   VDP.hh getHR: HR is 1 for HBLANK_LEN_GFX = 312 ticks after the right
   border (404 = HBLANK_LEN_TXT in the text modes), a Z80 cycle being 6 VDP
   ticks. Everything goes through the 0x99 port the Z80 sees. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let cycles_per_line = 228
let visible_lines = 192
let lines_per_frame = 262

let set_reg v r n =
  Vdp.io_write v ~port:0x99 n;
  Vdp.io_write v ~port:0x99 (0x80 lor r)

(* S#2 at the current beam position. The VBlank interrupt selects S#0 again,
   so each read selects S#2 first. *)
let s2 v =
  set_reg v 15 2;
  Vdp.io_read v ~port:0x99

let hr v = s2 v land 0x20 <> 0
let vr v = s2 v land 0x40 <> 0

(* Move the beam to [line], [cycle] from the frame start. *)
let beam v ~line ~cycle =
  let cur_line, cur_cycle = Vdp.line_now v in
  let delta =
    ((line - cur_line + lines_per_frame) mod lines_per_frame) * cycles_per_line
    + cycle - cur_cycle
  in
  let delta = if delta < 0 then delta + (lines_per_frame * cycles_per_line) else delta in
  ignore (Vdp.advance v ~cycles:delta)

let () =
  (* Graphic modes: HR is the first 52 cycles of a line. *)
  let v = Vdp.create () in
  beam v ~line:10 ~cycle:0;
  check "graphic: HR at cycle 0" (hr v);
  beam v ~line:10 ~cycle:51;
  check "graphic: HR at cycle 51" (hr v);
  beam v ~line:10 ~cycle:52;
  check "graphic: HR drops at cycle 52" (not (hr v));
  beam v ~line:10 ~cycle:227;
  check "graphic: HR stays low to the line end" (not (hr v));
  beam v ~line:11 ~cycle:0;
  check "graphic: HR is back on the next line" (hr v);
  (* Text modes: 404 ticks is 67.3 cycles, so the 68th cycle is the first low one. *)
  let v = Vdp.create () in
  set_reg v 1 0x10;
  beam v ~line:10 ~cycle:67;
  check "text: HR at cycle 67" (hr v);
  beam v ~line:10 ~cycle:68;
  check "text: HR drops at cycle 68" (not (hr v));
  (* VR: low in the display, high from the display end, low again 48 cycles
     into the last line of the frame. *)
  let v = Vdp.create () in
  beam v ~line:0 ~cycle:100;
  check "VR is low on the first display line" (not (vr v));
  beam v ~line:(visible_lines - 1) ~cycle:200;
  check "VR is low on the last display line" (not (vr v));
  beam v ~line:visible_lines ~cycle:0;
  check "VR is high at the display end" (vr v);
  beam v ~line:(lines_per_frame - 1) ~cycle:47;
  check "VR is high before the last line's cycle 48" (vr v);
  beam v ~line:(lines_per_frame - 1) ~cycle:48;
  check "VR drops at cycle 48 of the last line" (not (vr v));
  (* The other bits keep their old values. *)
  check "bits 4-1 still read 1" (s2 v land 0x1e = 0x1e);
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "vdp_s2_retrace_test: all pass"
