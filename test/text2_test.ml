(* TEXT2 (SCREEN 0 WIDTH 80). The frame is 512 wide: 80 columns of 6x8 cells
   between 16px borders of the R#7 background colour. The name table is 80 bytes
   a row at (R#2 bits 6-2) x 0x400, the pattern table at R#4 x 0x800. With LN
   (R#9 bit 7) the frame has 212 lines and a 27th row, cut at line 211.
   The mode was drawn as TEXT1 before: a 256x192 frame of 40 columns. *)

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

(* Glyph 0x41: first row 0xFC (the six cells' pixels all lit), second row 0x80
   (only the leftmost). Every other glyph is empty. *)
let text2 ?(r2 = 0x03) ?(r4 = 2) ?(ln = false) () =
  let t = Vdp.create () in
  set_reg t 0 0x04;
  set_reg t 1 0x70;
  set_reg t 2 r2;
  set_reg t 4 r4;
  set_reg t 7 0xf4;
  set_reg t 9 (if ln then 0x80 else 0x00);
  vram_w t ((r4 * 0x800) + (0x41 * 8)) 0xfc;
  vram_w t ((r4 * 0x800) + (0x41 * 8) + 1) 0x80;
  t

let px t x y =
  let w, _ = Vdp.frame_dims t in
  let rgb = Vdp.frame_rgb t in
  let i = ((y * w) + x) * 3 in
  (Char.code rgb.[i], Char.code rgb.[i + 1], Char.code rgb.[i + 2])

let is_col t x y c = px t x y = Vdp.palette_rgb t c

let () =
  let t = text2 () in
  check "TEXT2 is the mode" (Vdp.display_mode t = Vdp.Text2);
  check "frame is 512x192" (Vdp.frame_dims t = (512, 192));
  check "frame holds 512x192 pixels" (String.length (Vdp.frame_rgb t) = 512 * 192 * 3);
  check "left border is bg 4" (is_col t 0 0 4 && is_col t 15 0 4);
  check "right border is bg 4" (is_col t 496 0 4 && is_col t 511 0 4);
  (* name table all zero: every cell shows glyph 0 (empty) *)
  check "an empty cell is bg 4" (is_col t 16 0 4 && is_col t 100 100 4);
  vram_w t 0 0x41;
  check "column 0 starts at x=16 with fg 15" (is_col t 16 0 15);
  check "column 0 ends at x=21" (is_col t 21 0 15);
  check "column 1 is empty" (is_col t 22 0 4);
  check "second glyph row lights only the leftmost pixel" (is_col t 16 1 15 && is_col t 17 1 4);
  vram_w t 79 0x41;
  check "column 79 starts at x=16+79*6=490" (is_col t 490 0 15 && is_col t 495 0 15);
  check "column 79 ends before the right border" (is_col t 496 0 4);
  vram_w t 80 0x41;
  check "the second row starts at name table byte 80, line 8" (is_col t 16 8 15);
  (* TEXT1 keeps its own frame *)
  let t1 = Vdp.create () in
  set_reg t1 0 0x00;
  set_reg t1 1 0x50;
  check "TEXT1 frame stays 256x192" (Vdp.frame_dims t1 = (256, 192));
  (* pattern table base follows R#4 *)
  let t = text2 ~r4:1 () in
  vram_w t 0 0x41;
  check "R#4=1 reads the glyph from 0x0800" (is_col t 16 0 15);
  (* name table base: R#2 bits 6-2 are 4KB steps; bits 1-0 do not move it *)
  let t = text2 ~r2:0x07 () in
  vram_w t 0x1000 0x41;
  check "R#2=7 puts the name table at 0x1000" (is_col t 16 0 15);
  let t = text2 ~r2:0x03 () in
  vram_w t 0x400 0x41;
  check "R#2=3: bits 1-0 do not move the name table" (is_col t 16 0 4);
  (* LN: 212 lines, the 27th row is cut *)
  let t = text2 ~ln:true () in
  check "LN gives a 512x212 frame" (Vdp.frame_dims t = (512, 212));
  vram_w t (26 * 80) 0x41;
  check "row 26 starts at line 208" (is_col t 16 208 15);
  check "the frame ends at line 211, the cut of the 27th row"
    (is_col t 16 211 4 && String.length (Vdp.frame_rgb t) = 512 * 212 * 3);
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "text2_test: all pass"
