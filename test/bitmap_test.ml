(* Port-level fixtures derived from the Yamaha V9938 application manual.
   Raw addresses below are physical VRAM, independently fixed expectations. *)
let check name cond = if not cond then failwith name
let reg t r v = Vdp.io_write t ~port:0x99 v; Vdp.io_write t ~port:0x99 (0x80 lor r)
let seek t ~write a =
  reg t 14 (a lsr 14);
  Vdp.io_write t ~port:0x99 (a land 255);
  Vdp.io_write t ~port:0x99 (((a lsr 8) land 63) lor if write then 64 else 0)
let data t xs = List.iter (Vdp.io_write t ~port:0x98) xs
let raw t a = Vdp.vram_read t a
(* Page 0 with R#2 bits 4-0 set: those bits mask the display line
   (openMSX SDLRasterizer::drawDisplay), so the BIOS writes them as 1. *)
(* The display mode, blank and sprite-disable bits take effect where the
   next line starts (openMSX syncAtNextLine); the CPU's planar addressing
   and the command engine follow the mode from then. One line always
   passes one line start; [fresh] starts at cycle 0 and only needs the 39
   cycles to the first one, so its line count stays. *)
let line_start = 39
let settle t = ignore (Vdp.advance t ~cycles:228)
(* Vdp.create is a frame start, 42 lines above display line 0 (openMSX
   VDP::reset calls frameStart); these checks count lines from display
   line 0. *)
let to_display_start t =
  while Vdp.line_now t <> (0, 0) do ignore (Vdp.advance t ~cycles:1) done

let fresh mode =
  let t = Vdp.create () in
  to_display_start t;
  reg t 0 mode; reg t 1 0x40; reg t 2 0x1f;
  ignore (Vdp.advance t ~cycles:line_start);
  t
let xy t r x = reg t r (x land 255); reg t (r + 1) (x lsr 8)
(* Every command but STOP and the CPU transfers runs in VRAM access slots
   (openMSX VDPCmdEngine timing): run the clock until S#2 CE drops. *)
let ce t =
  reg t 15 2;
  let v = Vdp.io_read t ~port:0x99 in
  reg t 15 0;
  v land 1 <> 0
let timed cmd =
  match cmd lsr 4 with
  | 4 | 5 | 6 | 7 | 8 | 9 | 0xc | 0xd | 0xe -> true
  | _ -> false
let command t ~cmd ~dx ~dy ~nx ~ny ~arg ~color =
  xy t 36 dx; xy t 38 dy; xy t 40 nx; xy t 42 ny;
  reg t 45 arg; reg t 44 color; reg t 46 cmd;
  if timed cmd then while ce t do ignore (Vdp.advance t ~cycles:1) done
(* Two frames: the second VBlank publishes lines all scanned after the
   first, so every line reflects the state written before. *)
let show t = ignore (Vdp.advance t ~cycles:(2 * 262 * 228))
let dims t = show t; Vdp.frame_dims t
let pixel t x y =
  show t;
  let width, _ = Vdp.frame_dims t in
  let rgb = Vdp.frame_rgb t in let i = (y * width + x) * 3 in
  (Char.code rgb.[i], Char.code rgb.[i + 1], Char.code rgb.[i + 2])
let white t = reg t 16 1; Vdp.io_write t ~port:0x9a 0x77; Vdp.io_write t ~port:0x9a 7

let () =
  List.iter (fun (mode, width) ->
    let t = fresh mode in
    check "native width and default height" (dims t = (width, 192));
    reg t 9 0x80;
    check "212 lines selected" (dims t = (width, 212));
    check "RGB length agrees with dimensions" (String.length (Vdp.frame_rgb t) = width * 212 * 3)
  ) [0x06,256; 0x08,512; 0x0a,512; 0x0e,256];
  let t = fresh 0x0a in
  white t; seek t ~write:true 0; data t [0x01;0x10;0x23];
  check "SCREEN7 physical banks interleave" (raw t 0 = 0x01 && raw t 0x10000 = 0x10 && raw t 1 = 0x23);
  check "left pixel preserved" (pixel t 0 0 = (0,0,0));
  check "right pixel preserved" (pixel t 1 0 = (255,255,255));
  check "next bank rendered" (pixel t 2 0 = (255,255,255));
  seek t ~write:false 0;
  let first = Vdp.io_read t ~port:0x98 in
  let second = Vdp.io_read t ~port:0x98 in
  let third = Vdp.io_read t ~port:0x98 in
  check "read prefetch does not duplicate first byte" (first = 1 && second = 0x10 && third = 0x23);
  (* Mode switches preserve physical VRAM, not a mode-specific shadow. *)
  reg t 0 0x06; settle t; seek t ~write:false 1;
  check "SCREEN5 sees SCREEN7 even-bank byte" (Vdp.io_read t ~port:0x98 = 0x23);
  let t = fresh 0x08 in white t;
  seek t ~write:true 0; data t [0x40];
  check "SCREEN6 four pixels per byte" (pixel t 0 0 = (255,255,255) && pixel t 1 0 = (0,0,0));
  let t = fresh 0x0e in
  seek t ~write:true 0; data t [0x1c;0xe0;0x03];
  check "SCREEN8 red is bits 2-4" (pixel t 0 0 = (255,0,0));
  check "SCREEN8 green is bits 5-7" (pixel t 1 0 = (0,255,0));
  check "SCREEN8 pixels are not averaged or doubled" (pixel t 2 0 = (0,0,255));
  let t = fresh 0x0a in white t; reg t 9 0x80;
  seek t ~write:true (211 * 256 + 255); data t [0x01];
  check "last native pixel in 212-line mode" (pixel t 511 211 = (255,255,255));
  reg t 23 211;
  check "vertical scroll uses selected display page" (pixel t 511 0 = (255,255,255));
  (* R#2 bits 6-5 select page 3 in SCREEN5/6 (0x7F); the planar
     SCREEN7/8 have two pages and take bit 5 (0x3F is page 1). Scroll
     wraps from line 255 to line 0 within that page, not into page 0. *)
  List.iter (fun (name, mode, r2, page_base, stride, last_physical, first_physical, color, rgb) ->
    let t = fresh mode in white t;
    reg t 2 r2;
    seek t ~write:true (page_base + 255 * stride); data t [color];
    seek t ~write:true page_base; data t [color];
    check (name ^ " selected page physical addresses")
      (raw t last_physical = color && raw t first_physical = color);
    reg t 23 255;
    check (name ^ " scroll selects last line of nonzero page") (pixel t 0 0 = rgb);
    check (name ^ " scroll wraps within nonzero page") (pixel t 0 1 = rgb);
    reg t 2 0x1f;
    check (name ^ " page zero remains distinct")
      (pixel t 0 0 = (0,0,0) && pixel t 0 1 = (0,0,0))
  ) [ "SCREEN5", 0x06, 0x7f, 0x18000, 128, 0x1ff80, 0x18000, 0x10, (255,255,255);
      "SCREEN6", 0x08, 0x7f, 0x18000, 128, 0x1ff80, 0x18000, 0x40, (255,255,255);
      "SCREEN7", 0x0a, 0x3f, 0x10000, 256, 0x0ff80, 0x08000, 0x10, (255,255,255);
      "SCREEN8", 0x0e, 0x3f, 0x10000, 256, 0x0ff80, 0x08000, 0x1c, (255,0,0) ];
  (* NX is pixels even for byte transfer commands. Two bytes, not four. *)
  let t = fresh 0x0a in
  command t ~cmd:0xf0 ~dx:0 ~dy:0 ~nx:4 ~ny:1 ~arg:0 ~color:0x12;
  reg t 44 0x34;
  let active,_,_,_,_ = Vdp.tx_state t in
  check "HMMC completes after NX/2 bytes" (not active);
  reg t 44 0x56;
  check "HMMC contiguous logical bytes" (raw t 0 = 0x12 && raw t 0x10000 = 0x34 && raw t 1 = 0);
  let t = fresh 0x08 in
  command t ~cmd:0xb0 ~dx:1 ~dy:0 ~nx:2 ~ny:1 ~arg:0 ~color:2;
  reg t 44 3;
  check "LMMC SCREEN6 preserves neighboring 2-bit pixels" (raw t 0 = 0x2c);
  let t = fresh 0x0a in
  seek t ~write:true 0; data t [0xab];
  command t ~cmd:0x80 ~dx:1 ~dy:0 ~nx:1 ~ny:1 ~arg:0 ~color:3;
  check "LMMV odd pixel preserves adjacent nibble" (raw t 0 = 0xa3);
  command t ~cmd:0x8b ~dx:0 ~dy:0 ~nx:2 ~ny:1 ~arg:0 ~color:0;
  check "transparent XOR skips zero" (raw t 0 = 0xa3);
  command t ~cmd:0x84 ~dx:1 ~dy:0 ~nx:1 ~ny:1 ~arg:0 ~color:1;
  check "NOT is masked to one pixel" (raw t 0 = 0xae);
  let t = fresh 0x0a in
  command t ~cmd:0xc0 ~dx:510 ~dy:257 ~nx:20 ~ny:1 ~arg:0 ~color:0x5a;
  check "HMMV clips at right edge and honors high Y" (raw t 0x180ff = 0x5a && raw t 0x8080 = 0);
  let t = fresh 0x06 in
  seek t ~write:true 0; data t [0x12;0x34;0x56];
  xy t 32 0; xy t 34 0;
  command t ~cmd:0xd0 ~dx:2 ~dy:0 ~nx:4 ~ny:1 ~arg:0 ~color:0;
  check "overlapping HMMM uses byte steps" (raw t 0 = 0x12 && raw t 1 = 0x12 && raw t 2 = 0x12);
  let t = fresh 0x06 in
  seek t ~write:true 0; data t [0x12;0x34;0x56];
  xy t 32 1; xy t 34 0;
  command t ~cmd:0x90 ~dx:0 ~dy:1 ~nx:3 ~ny:1 ~arg:0 ~color:0;
  check "LMMM copies pixels across nibble alignment" (raw t 128 = 0x23 && raw t 129 = 0x40);
  let t = fresh 0x06 in
  seek t ~write:true 126; data t [0xab;0xcd];
  xy t 34 0;
  command t ~cmd:0xe0 ~dx:252 ~dy:1 ~nx:0 ~ny:1 ~arg:0 ~color:0;
  check "YMMM starts at DX, not beginning of row" (raw t 128 = 0 && raw t 254 = 0xab && raw t 255 = 0xcd);
  let t = fresh 0x0a in
  command t ~cmd:0xf0 ~dx:2 ~dy:0 ~nx:8 ~ny:1 ~arg:4 ~color:0x12;
  reg t 44 0x34;
  let active,_,_,_,_ = Vdp.tx_state t in
  check "reverse HMMC clips at left edge" (not active && raw t 0x10000 = 0x12 && raw t 0 = 0x34);
  let t = fresh 0x06 in
  command t ~cmd:0xc0 ~dx:0 ~dy:0 ~nx:2 ~ny:1 ~arg:0x20 ~color:0xff;
  check "absent expansion VRAM does not overwrite display" (raw t 0 = 0 && raw t 0x10000 = 0);
  let t = fresh 0x06 in reg t 9 0x80;
  ignore (Vdp.advance t ~cycles:(192*228));
  check "212-line vblank does not begin at 192" (Vdp.status0 t land 0x80 = 0);
  ignore (Vdp.advance t ~cycles:(20*228));
  check "212-line vblank begins at 212" (Vdp.status0 t land 0x80 <> 0);
  let t = fresh 0x06 in reg t 0 0x16; reg t 19 10;
  ignore (Vdp.advance t ~cycles:(10*228));
  check "line interrupt waits for line 10 to finish" (not (Vdp.int_active t));
  ignore (Vdp.advance t ~cycles:228);
  check "line interrupt enable is R0 bit4" (Vdp.int_active t);
  print_endline "bitmap: native pixels, VRAM ports, commands and interrupts passed"
