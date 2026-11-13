(* R#18 set-adjust contracts, judged on the published frame. The frame is
   the display area at its neutral place; adjust moves the display inside
   it and the uncovered edge shows the border (R#7). Expected values come
   from openMSX VDP.cc: execHorAdjust and getVerticalAdjust ((nibble xor
   7), neutral 7), getLeftSprites (horizontal move in 4-tick dots), and
   scheduleLineCountReset (vertical adjust taken at the counter reset).
   Palette: 0 red, 1 green, 2 yellow, 3 cyan, 4 blue. R#7 = 3 makes the
   border cyan; R#8 TP makes colour 0 red, so content never looks like
   border. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let reg t r v =
  Vdp.io_write t ~port:0x99 v;
  Vdp.io_write t ~port:0x99 (0x80 lor r)

let seek t a =
  reg t 14 (a lsr 14);
  Vdp.io_write t ~port:0x99 (a land 0xff);
  Vdp.io_write t ~port:0x99 (0x40 lor ((a lsr 8) land 0x3f))

let poke t a v = seek t a; Vdp.io_write t ~port:0x98 v

let palette t i (r, g, b) =
  reg t 16 i;
  Vdp.io_write t ~port:0x9a ((r lsl 4) lor b);
  Vdp.io_write t ~port:0x9a g

let rgb (r, g, b) = (r * 255 / 7, g * 255 / 7, b * 255 / 7)
let red = rgb (7, 0, 0)
let green = rgb (0, 7, 0)
let yellow = rgb (7, 7, 0)
let cyan = rgb (0, 7, 7)
let blue = rgb (0, 0, 7)

let lines t n = ignore (Vdp.advance t ~cycles:(228 * n))
let first_frame t = lines t 212
let next_frame t = lines t 262

let px t x y =
  let w, _ = Vdp.frame_dims t in
  let s = Vdp.frame_rgb t in
  let i = ((y * w) + x) * 3 in
  (Char.code s.[i], Char.code s.[i + 1], Char.code s.[i + 2])

(* SCREEN5 (or SCREEN7 for [r0] = 0x0A), page 0, border cyan, TP on.
   Line 0: dot 0 green, dot 1 yellow, dot 254 blue, dot 255 green. Line 1:
   dot 0 blue. Column 0 of lines 2-191 green. *)
(* Vdp.create is a frame start, 42 lines above display line 0 (openMSX
   VDP::reset calls frameStart); these checks count lines from display
   line 0. *)
let to_display_start t =
  while Vdp.line_now t <> (0, 0) do ignore (Vdp.advance t ~cycles:1) done

let screen ?(r0 = 0x06) r18 =
  let t = Vdp.create () in
  to_display_start t;
  List.iteri (fun i c -> palette t i c)
    [ (7, 0, 0); (0, 7, 0); (7, 7, 0); (0, 7, 7); (0, 0, 7) ];
  List.iter (fun (r, v) -> reg t r v)
    [ (0, r0); (1, 0x40); (2, 0x1f); (7, 0x03); (8, 0x20); (18, r18) ];
  if r0 = 0x06 then begin
    poke t 0 0x12;
    poke t 127 0x41;
    poke t 128 0x40;
    for y = 2 to 191 do poke t (y * 128) 0x10 done
  end
  else poke t 0 0x12;
  t

let () =
  let t = screen 0x00 in
  first_frame t;
  check "R#18 = 0: no move" (px t 0 0 = green && px t 1 0 = yellow);
  let t = screen 0x0f in
  first_frame t;
  check "R#18 low 0xF: one dot right, border on the left"
    (px t 0 0 = cyan && px t 1 0 = green && px t 2 0 = yellow);
  check "R#18 low 0xF: the last dot shows dot 254" (px t 255 0 = blue);
  let t = screen 0x08 in
  first_frame t;
  check "R#18 low 8: eight dots right"
    (px t 7 0 = cyan && px t 8 0 = green && px t 9 0 = yellow);
  let t = screen 0x01 in
  first_frame t;
  check "R#18 low 1: one dot left, border on the right"
    (px t 0 0 = yellow && px t 254 0 = green && px t 255 0 = cyan);
  let t = screen 0x07 in
  first_frame t;
  check "R#18 low 7: seven dots left"
    (px t 248 0 = green && px t 249 0 = cyan && px t 0 0 = red);
  (* SCREEN7 is 512 wide: one adjust step is two native dots. *)
  let t = screen ~r0:0x0a 0x0f in
  first_frame t;
  check "SCREEN7: one step moves two dots"
    (px t 1 0 = cyan && px t 2 0 = green && px t 3 0 = yellow);
  (* A blanked line is border throughout, adjust or not. *)
  let t = screen 0x0f in
  reg t 1 0x00;
  first_frame t;
  check "blanked line stays border" (px t 0 0 = cyan && px t 1 0 = cyan);

  (* Vertical: taken at the line counter reset, so it shows from the
     frame after the write. *)
  let t = screen 0xf0 in
  first_frame t;
  check "vertical adjust waits for the counter reset" (px t 0 0 = green);
  next_frame t;
  check "R#18 high 0xF: one line down, border on top"
    (px t 0 0 = cyan && px t 0 1 = green && px t 0 2 = blue);
  let t = screen 0x10 in
  first_frame t;
  next_frame t;
  check "R#18 high 1: one line up, border at the bottom"
    (px t 0 0 = blue && px t 0 190 = green && px t 0 191 = cyan);
  let t = screen 0x80 in
  first_frame t;
  next_frame t;
  check "R#18 high 8: eight lines down" (px t 0 7 = cyan && px t 0 8 = green);
  (* A write during the display waits for the next reset. *)
  let t = screen 0x00 in
  first_frame t;
  next_frame t;
  lines t 120;
  reg t 18 0xf0;
  lines t 142;
  check "vertical write during the display waits a frame" (px t 0 0 = green);
  next_frame t;
  check "and then moves the display" (px t 0 0 = cyan);

  (* The reset sits 27 lines above display line 0 in a 60 Hz, 192-line
     frame (openMSX execLineCountReset), 8 + 7 lines after the frame start
     at line 220: a write at line 230 is between the two and still counts.
     It moves the reset, and with it display line 0, one line later
     (openMSX master scheduleLineCountReset), so VBlank comes a line
     later too. *)
  let t = screen 0x00 in
  first_frame t;
  lines t 18;
  reg t 18 0xf0;
  lines t (262 - 230 + 1 + 192);
  check "a write between frame start and the reset is taken" (px t 0 0 = cyan);

  (* Horizontal changes take effect per line: a write before line 100 is
     scanned moves lines 100 on. *)
  let t = screen 0x00 in
  first_frame t;
  lines t 50;
  lines t 100;
  reg t 18 0x0f;
  lines t 112;
  check "horizontal write mid-frame: lines above keep their place" (px t 0 99 = green);
  check "horizontal write mid-frame: lines below move" (px t 0 100 = cyan && px t 1 100 = green);

  (* The latched vertical adjust survives a save: R#18 changed after the
     reset must not move the restored frame. *)
  let t = screen 0xf0 in
  first_frame t;
  next_frame t;
  lines t 120;
  reg t 18 0x00;
  let w = State_codec.writer () in
  Vdp.write_state w t;
  Vdp.write_appended w t;
  let r = State_codec.reader (State_codec.finish w) in
  let q = Vdp.create () in
  Vdp.read_state r q;
  Vdp.read_appended r q;
  State_codec.end_of_input r;
  lines t 142;
  lines q 142;
  check "restored VDP keeps the latched vertical adjust"
    (px q 0 0 = cyan && Vdp.frame_rgb q = Vdp.frame_rgb t);

  if !failures > 0 then begin
    Printf.eprintf "adjust_test: %d failure(s)\n" !failures;
    exit 1
  end;
  print_endline "adjust_test: all passed"
