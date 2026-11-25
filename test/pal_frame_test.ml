(* R#9 bit 1 (NT) makes the frame 313 lines (PAL, 50 Hz) instead of 262.

   openMSX latches it at the start of each frame (VDP::frameStart), so a write
   in the middle of a frame changes the next frame, not this one. Msx.step
   ends a frame where the VDP's frame ends, so N PAL frames are N * 313 * 228
   cycles. Measured on the real NMS8250 BIOS under openMSX: R#9 = 2 after
   boot, and the MSXgl sample xracing, whose animation steps once per frame,
   ends 25 s on the same picture as this emulator only when the frames last
   313 lines. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let line = 228
let ntsc = 262 * line
let pal = 313 * line

let set_reg t r v =
  Vdp.io_write t ~port:0x99 v;
  Vdp.io_write t ~port:0x99 (0x80 lor r)

let advance t n = ignore (Vdp.advance t ~cycles:n)

let () =
  (* A VDP alone: NTSC first, PAL from the frame after the write. *)
  let t = Vdp.create () in
  check "a new VDP is NTSC" (not (Vdp.is_pal t));
  check "an NTSC frame has 262 lines" (Vdp.cycles_to_frame_end t = ntsc);
  set_reg t 9 0x02;
  check "the write does not change the running frame" (not (Vdp.is_pal t));
  advance t (ntsc - 1);
  check "the NTSC frame ends at line 262" (Vdp.line_now t = (261, 227));
  advance t 1;
  check "the next frame is PAL" (Vdp.is_pal t && Vdp.line_now t = (0, 0));
  check "a PAL frame has 313 lines" (Vdp.cycles_to_frame_end t = pal);
  advance t (pal - 1);
  check "the PAL frame ends at line 313" (Vdp.line_now t = (312, 227));
  set_reg t 9 0x00;
  check "clearing NT mid-frame keeps this frame PAL" (Vdp.is_pal t);
  advance t 1;
  check "the frame after the clear is NTSC" (not (Vdp.is_pal t) && Vdp.line_now t = (0, 0));
  (* The remaining time counts from the position inside the frame. *)
  advance t ((100 * line) + 5);
  check "cycles to the end of an NTSC frame from line 100 cycle 5"
    (Vdp.cycles_to_frame_end t = ((262 - 100) * line) - 5);
  (* Restore the current frame length and scanline. *)
  let t = Vdp.create () in
  set_reg t 9 0x02;
  advance t ntsc;
  advance t (50 * line);
  let w = State_codec.writer () in
  Vdp.write_state w t;
  let r = Vdp.create () in
  Vdp.read_state (State_codec.reader (State_codec.finish w)) r;
  check "a restored PAL VDP is PAL" (Vdp.is_pal r);
  check "and keeps its position" (Vdp.line_now r = (50, 0) && Vdp.cycles_to_frame_end r = (263 * line));
  (* Lines 262..312 exist only in a PAL frame: a save taken there restores. *)
  let saved_at_line n =
    let t = Vdp.create () in
    set_reg t 9 0x02;
    advance t ntsc;
    advance t ((n * line) + 7);
    let w = State_codec.writer () in
    Vdp.write_state w t;
    State_codec.finish w
  in
  let restore state =
    let r = Vdp.create () in
    Vdp.read_state (State_codec.reader state) r;
    r
  in
  let late = restore (saved_at_line 300) in
  check "a PAL save at line 300 keeps its position" (Vdp.line_now late = (300, 7));
  let last = restore (saved_at_line 312) in
  check "a PAL save at line 312 keeps its position" (Vdp.line_now last = (312, 7));
  let round_trip t =
    let w = State_codec.writer () in
    Vdp.write_state w t;
    restore (State_codec.finish w)
  in
  let t = Vdp.create () in
  advance t ((261 * line) + 7);
  set_reg t 9 0x02;
  let r = round_trip t in
  check "pending PAL change keeps the restored current frame NTSC"
    (not (Vdp.is_pal r) && Vdp.line_now r = (261, 7));
  advance r (line - 7);
  check "the pending PAL setting applies at the next boundary" (Vdp.is_pal r);
  set_reg last 9 0;
  let r = round_trip last in
  check "pending NTSC change restores the late PAL scanline"
    (Vdp.is_pal r && Vdp.line_now r = (312, 7));
  advance r (line - 7);
  check "the pending NTSC setting applies at the next boundary" (not (Vdp.is_pal r));
  (* A machine: Msx.step ends a frame at the VDP's frame end. *)
  let m = Msx.create ~machine:{ Msx.ram_kb = 64; vram_kb = 128; roms = [ ""; ""; "" ] } in
  Msx.port_out m 0xa8 0xff;
  Msx.mem_write m 0xffff 0xaa;
  List.iteri (fun i seg -> Msx.port_out m (0xfc + i) seg) [ 3; 2; 1; 0 ];
  List.iteri (fun i b -> Msx.mem_write m i b) [ 0x31; 0x00; 0xf0; 0xe3; 0x23; 0x23; 0xc3; 0x03; 0x00 ];
  Msx.port_out m 0x99 0x02;
  Msx.port_out m 0x99 0x89;
  let reference = Vdp.create () in
  set_reg reference 9 0x02;
  let worst = ref 0 in
  advance reference ntsc;
  Msx.step m ~frames:1;
  for k = 1 to 40 do
    Msx.step m ~frames:1;
    advance reference pal;
    let l1, c1 = Msx.vdp_line m and l0, c0 = Vdp.line_now reference in
    let d = ((l1 * line) + c1) - ((l0 * line) + c0) in
    worst := max !worst d;
    if d < 0 || d >= 20 then
      check (Printf.sprintf "PAL frame %d ends within one instruction of its boundary (%d past)" k d) false
  done;
  check "some PAL frame ends past the boundary" (!worst > 0);
  check "frame_number counts PAL frames too" (Msx.frame_number m = 41);
  if !failures > 0 then begin
    Printf.eprintf "pal_frame_test: %d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "pal_frame_test: all pass"
