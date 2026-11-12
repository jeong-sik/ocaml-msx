(* Character-mode and bitmap-page contracts, written through the VDP ports
   and judged on the frame the VBlank publishes. Expected values come from
   openMSX: CharacterConverter.cc (TEXT1, TEXT2, MULTICOLOR, GRAPHIC 1-3),
   PixelRenderer::draw (R#23 in character modes, the text row counter),
   VDP.hh getLeftSprites (text starts 9 dots in on a V99x8),
   SDLRasterizer::drawDisplay and VDP::getEvenOddMask (bitmap pages), and
   VDP::frameStart / changeRegister (R#13 blinking).
   Palette: 0 red, 1 green, 2 yellow, 3 cyan, 4 blue. *)

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

let vdp ~r0 ~r1 regs =
  let t = Vdp.create () in
  List.iteri (fun i (r, g, b) -> palette t i (r, g, b))
    [ (7, 0, 0); (0, 7, 0); (7, 7, 0); (0, 7, 7); (0, 0, 7) ];
  reg t 0 r0;
  reg t 1 r1;
  List.iter (fun (r, v) -> reg t r v) regs;
  t

let cycles_per_frame = 262 * 228

(* From line 0, scan to the first VBlank (212 lines covers both heights:
   the VBlank of a 192-line frame publishes and the rest is border). *)
let first_frame t = ignore (Vdp.advance t ~cycles:(228 * 212))

(* From a VBlank, one whole frame to the next VBlank. *)
let next_frame t = ignore (Vdp.advance t ~cycles:cycles_per_frame)

let px t x y =
  let w, _ = Vdp.frame_dims t in
  let s = Vdp.frame_rgb t in
  let i = ((y * w) + x) * 3 in
  (Char.code s.[i], Char.code s.[i + 1], Char.code s.[i + 2])

(* Lengths of the runs of equal values, in order. *)
let runs xs =
  let rec go acc n prev = function
    | [] -> List.rev (n :: acc)
    | x :: rest -> if x = prev then go acc (n + 1) prev rest else go (n :: acc) 1 x rest
  in
  match xs with [] -> [] | x :: rest -> go [] 1 x rest

let sample t frames probe = List.init frames (fun _ -> next_frame t; probe t)

let () =
  (* GRAPHIC 2: R#23 picks the VRAM row, rows 24-31 included. Name table
     0x1800 (R#2 = 6), patterns 0 (R#4 = 3), colours 0x2000 (R#3 = 0xFF).
     Row 25 is the last third; its pattern index 3 << 11 | 5 << 3 lands at
     0x1828. *)
  let g2 ?(r9 = 0) r23 =
    let t = vdp ~r0:0x02 ~r1:0x40 [ (2, 0x06); (3, 0xff); (4, 0x03); (9, r9); (23, r23) ] in
    poke t (0x1800 + (25 * 32)) 5;
    poke t 0x1828 0x80;
    poke t 0x3828 0x41;
    first_frame t;
    t
  in
  let t = g2 200 in
  check "G2: R#23 = 200 shows row 25 on line 0 (pattern dot)" (px t 0 0 = blue);
  check "G2: R#23 = 200 shows row 25 on line 0 (clear dot)" (px t 1 0 = green);
  check "G2: no scroll shows row 0 on line 0" (px (g2 0) 0 0 <> blue);
  let t = g2 ~r9:0x80 0 in
  check "G2 with LN: frame is 256x212" (Vdp.frame_dims t = (256, 212));
  check "G2 with LN: line 200 shows row 25" (px t 0 200 = blue && px t 1 200 = green);

  (* GRAPHIC 1: R#23 = 9 puts row 1, dot row 1 on line 0. Colour byte
     index is name >> 3 at 0x2000 (R#3 = 0x80). *)
  let t = vdp ~r0:0x00 ~r1:0x40 [ (2, 0x06); (3, 0x80); (4, 0x00); (23, 9) ] in
  poke t (0x1800 + 32) 8;
  poke t ((8 * 8) + 1) 0x80;
  poke t (0x2000 + 1) 0x41;
  first_frame t;
  check "G1: R#23 = 9 shows row 1, dot row 1 on line 0" (px t 0 0 = blue && px t 1 0 = green);

  (* MULTICOLOR: name 3 at row 0 and row 1 col 0. The byte of a line is
     3 * 8 + (line / 4) mod 8; its high nibble colours the left 4 dots. *)
  let multi r23 =
    let t = vdp ~r0:0x00 ~r1:0x48 [ (2, 0x06); (4, 0x00); (23, r23) ] in
    poke t 0x1800 3;
    poke t 0x1820 3;
    poke t 24 0x41;
    poke t 25 0x23;
    poke t 26 0x44;
    first_frame t;
    t
  in
  let t = multi 0 in
  check "MULTICOLOR: lines 0-3 left block" (px t 0 0 = blue && px t 3 3 = blue);
  check "MULTICOLOR: lines 0-3 right block" (px t 4 0 = green && px t 7 3 = green);
  check "MULTICOLOR: lines 4-7 take the next byte" (px t 0 4 = yellow && px t 4 7 = cyan);
  check "MULTICOLOR: row 1 starts at byte 2" (px t 0 8 = blue && px t 4 8 = blue);
  check "MULTICOLOR: R#23 = 4 shows lines 4-7 on line 0" (px (multi 4) 0 0 = yellow);

  (* TEXT1: R#23 moves the dot row inside the character, not the
     character row. Code 1 has only dot row 3 set; code 2, on row 1, has
     dot rows 0 and 3 set. *)
  let text1 r23 =
    let t = vdp ~r0:0x00 ~r1:0x50 [ (2, 0x00); (4, 0x01); (7, 0x43); (23, r23) ] in
    poke t 0 1;
    poke t 40 2;
    poke t (0x800 + 8 + 3) 0x80;
    poke t (0x800 + 16) 0x80;
    poke t (0x800 + 16 + 3) 0x80;
    first_frame t;
    t
  in
  let t = text1 3 in
  check "TEXT1: text starts at dot 9" (px t 9 0 = blue && px t 8 0 = cyan);
  check "TEXT1: R#23 = 3 shows dot row 3 on line 0" (px (text1 0) 9 3 = blue && px t 9 0 = blue);
  check "TEXT1: the character row ignores R#23" (px t 9 8 = blue && px t 9 5 = cyan);

  (* TEXT2: 80 columns on the 512-dot grid. Names at 0 (R#2 = 3),
     patterns at 0x1000 (R#4 = 2), blink marks at 0x800 (R#3 = 0x27).
     Code 1 sets five dots of its first row. Column 0 is marked. *)
  let text2 ?(r12 = 0x21) r13 =
    let t = vdp ~r0:0x04 ~r1:0x50 [ (2, 0x03); (3, 0x27); (4, 0x02); (7, 0x43); (12, r12) ] in
    poke t 0 1;
    poke t 79 1;
    poke t (0x1000 + 8) 0xf8;
    poke t 0x800 0x80;
    reg t 13 r13;
    first_frame t;
    t
  in
  let t = text2 0x00 in
  check "TEXT2: frame is 512 wide" (Vdp.frame_dims t = (512, 192));
  check "TEXT2: text starts at dot 18" (px t 17 0 = cyan && px t 18 0 = blue);
  check "TEXT2: column 79 ends at dot 497"
    (px t (18 + (79 * 6)) 0 = blue && px t (18 + (79 * 6) + 4) 0 = blue
     && px t 498 0 = cyan);
  check "TEXT2: blinking off shows plain colours on a marked column" (px t 18 0 = blue);
  let t = text2 0xf0 in
  check "TEXT2: blink ON shows R#12 colours on a marked column"
    (px t 18 0 = yellow && px t 23 0 = green);
  check "TEXT2: unmarked columns keep the plain colours" (px t (18 + (79 * 6)) 0 = blue);
  check "TEXT2: blink foreground 0 shows the blink background"
    (px (text2 ~r12:0x01 0xf0) 18 0 = green);
  (* R#13 = 0x11: ten frames ON, ten OFF, from the write. *)
  let t = text2 0x11 in
  let probe t = px t 18 0 = yellow in
  let first = probe t in
  let seen = first :: sample t 49 probe in
  check "TEXT2: R#13 = 0x11 starts ON" (List.hd seen);
  (* The write frame is the first of the ten ON frames: the tenth frame
     start after the write flips the state. *)
  check "TEXT2: R#13 = 0x11 alternates every 10 frames" (runs seen = [ 10; 10; 10; 10; 10 ]);
  let t = text2 0x10 in
  check "TEXT2: an OFF period of 0 stays ON" (List.for_all Fun.id (sample t 30 probe));
  (* R#13 high nibble is the ON period, low the OFF period. *)
  let t = text2 0x21 in
  let first = probe t in
  let seen = first :: sample t 59 probe in
  check "TEXT2: R#13 = 0x21 is 20 frames ON, 10 OFF" (runs seen = [ 20; 10; 20; 10 ]);
  (* Blink colours are palette entries: blink background 0 is palette 0,
     not the R#7 backdrop. *)
  check "TEXT2: blink background 0 is palette 0" (px (text2 ~r12:0x20 0xf0) 23 0 = red);

  (* Bitmap pages. GRAPHIC 7 is planar: page 1 starts at CPU address
     0x10000 and R#2 bit 5 selects it; bit 6 is dropped. *)
  let g7 r2 =
    let t = vdp ~r0:0x0e ~r1:0x40 [ (2, r2) ] in
    poke t 0x10000 0x1c;
    first_frame t;
    t
  in
  check "G7: R#2 = 0x3F shows page 1" (px (g7 0x3f) 0 0 = (255, 0, 0));
  check "G7: R#2 = 0x5F shows page 0" (px (g7 0x5f) 0 0 = (0, 0, 0));
  (* GRAPHIC 4: R#2 bits 4-0 mask display line bits 7-3. *)
  let g4 r2 =
    let t = vdp ~r0:0x06 ~r1:0x40 [ (2, r2) ] in
    poke t 0 0x11;
    first_frame t;
    t
  in
  check "G4: R#2 = 0x1E shows line 0 again on line 8" (px (g4 0x1e) 0 8 = green);
  check "G4: R#2 = 0x1F shows line 8 on line 8" (px (g4 0x1f) 0 8 = red);
  (* Page 0 line 0 is colour 1, page 1 line 0 colour 2. *)
  let pages regs =
    let t = vdp ~r0:0x06 ~r1:0x40 ((2, 0x3f) :: regs) in
    poke t 0 0x11;
    poke t 0x8000 0x22;
    first_frame t;
    t
  in
  let odd_page t = px t 0 0 = yellow in
  check "G4: R#2 = 0x3F shows page 1" (odd_page (pages []));
  let seen = sample (pages [ (9, 0x04) ]) 6 odd_page in
  check "G4: R#9 bit 2 alternates the pages every field"
    (runs seen = [ 1; 1; 1; 1; 1; 1 ]);
  let t = pages [ (13, 0x11) ] in
  let first = odd_page t in
  let seen = first :: sample t 39 odd_page in
  check "G4: blink ON shows the even page" (not (List.hd seen));
  check "G4: R#13 = 0x11 alternates the pages every 10 frames"
    (runs seen = [ 10; 10; 10; 10 ]);

  (* Blinking survives a save: the restored VDP continues the same
     sequence. *)
  let t = text2 0x23 in
  for _ = 1 to 13 do next_frame t done;
  let w = State_codec.writer () in
  Vdp.write_state w t;
  Vdp.write_appended w t;
  let r = State_codec.reader (State_codec.finish w) in
  let q = Vdp.create () in
  Vdp.read_state r q;
  Vdp.read_appended r q;
  State_codec.end_of_input r;
  check "restored blinking continues the same sequence"
    (sample t 60 probe = sample q 60 probe);

  if !failures > 0 then begin
    Printf.eprintf "charmode_test: %d failure(s)\n" !failures;
    exit 1
  end;
  print_endline "charmode_test: all passed"
