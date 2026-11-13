(* Where the display mode, blank and sprite-disable bits take effect. openMSX
   VDP::changeRegister defers them to the next line start (syncAtNextLine),
   144 ticks after the line's sync; every other register bit applies at
   once. In this model a line starts at the right border of the line above,
   so that point is 230 ticks (cycle 39) into the line in the graphic modes
   and 258 ticks (cycle 43) in the text modes. A write before it changes the
   line being scanned, a later one the next line. The same timing was
   measured against openMSX 21.0 with a probe ROM (sprite collision on one
   line versus the cycle of an R#1 BL or R#8 SPD write). *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let cycles_per_line = 228

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

let run t cycles = ignore (Vdp.advance t ~cycles)

(* From line 0 cycle 0 to [cycle] of [line]. *)
let goto t ~line ~cycle = run t ((line * cycles_per_line) + cycle)

(* Advance to the next VBlank publish and read a pixel. *)
let px t x y =
  let rec to_vblank () =
    let line, _ = Vdp.line_now t in
    if line <> 212 then begin run t 1; to_vblank () end
  in
  to_vblank ();
  let w, _ = Vdp.frame_dims t in
  let s = Vdp.frame_rgb t in
  let i = ((y * w) + x) * 3 in
  (Char.code s.[i], Char.code s.[i + 1], Char.code s.[i + 2])

let white = (255, 255, 255)
let black = (0, 0, 0)

(* SCREEN5, every byte 0x11: colour 1 white on every dot. Border R#7 = 0,
   black. All 128KB is filled so the planar SCREEN8 sees 0x11 too. *)
let screen5 () =
  let t = Vdp.create () in
  palette t 1 (7, 7, 7);
  reg t 0 0x06;
  reg t 1 0x40;
  reg t 2 0x1f;
  reg t 8 0x08;
  for bank = 0 to 7 do
    seek t (bank * 0x4000);
    for _ = 0 to 0x3fff do Vdp.io_write t ~port:0x98 0x11 done
  done;
  t

let () =
  (* Blank: cycle 38 is before the line start, cycle 39 is not. *)
  let t = screen5 () in
  goto t ~line:100 ~cycle:38;
  reg t 1 0x00;
  check "BL at cycle 38: line 99 drawn" (px t 0 99 = white);
  check "BL at cycle 38: line 100 blank" (px t 0 100 = black);
  let t = screen5 () in
  goto t ~line:100 ~cycle:39;
  reg t 1 0x00;
  check "BL at cycle 39: line 100 still drawn" (px t 0 100 = white);
  check "BL at cycle 39: line 101 blank" (px t 0 101 = black);

  (* Mode: SCREEN5 to SCREEN8 after the line start keeps line 100 in
     SCREEN5 (colour 1, white); line 101 reads 0x11 as GRB 0,4,1. *)
  let t = screen5 () in
  goto t ~line:100 ~cycle:39;
  reg t 0 0x0e;
  check "mode after the line start: line 100 keeps SCREEN5" (px t 0 100 = white);
  check "mode after the line start: line 101 is SCREEN8" (px t 0 101 <> white);

  (* SPD: sprites for display line 100 are checked as line 99 ends, with
     the value taken at line 99's start. Two 8x8 sprites overlap on line
     100 only (sprite 0's last row, sprite 1's first). *)
  let sprites () =
    let t = Vdp.create () in
    List.iter (fun (r, v) -> reg t r v)
      [ (0, 0x06); (1, 0x40); (2, 0x1f); (5, 0xef); (11, 0x00); (6, 0x0f); (8, 0x08) ];
    poke t (0x7800 + 7) 0xff;
    poke t (0x7800 + 8) 0xff;
    for i = 0 to 31 do poke t (0x7400 + i) 0x0f done;
    List.iteri (fun i v -> poke t (0x7600 + i) v) [ 92; 100; 0; 0; 99; 100; 1; 0; 0xd8 ];
    t
  in
  let collided t = Vdp.status0 t land 0x20 <> 0 in
  let t = sprites () in
  goto t ~line:99 ~cycle:38;
  reg t 8 0x0a;
  run t (2 * cycles_per_line);
  check "SPD at cycle 38 of line 99: no collision on line 100" (not (collided t));
  let t = sprites () in
  goto t ~line:99 ~cycle:39;
  reg t 8 0x0a;
  run t (2 * cycles_per_line);
  check "SPD at cycle 39 of line 99: line 100 still collides" (collided t);

  (* Bits outside the deferred set apply at once: IE0 raises the interrupt
     with F already set, in the same cycle. *)
  let t = screen5 () in
  goto t ~line:200 ~cycle:100;
  check "F set, IE0 off: no interrupt" (not (Vdp.int_active t));
  reg t 1 0x60;
  check "IE0 applies at once" (Vdp.int_active t);

  (* The CPU's planar addressing follows the mode once it takes effect.
     SCREEN8 address 3 is physical 0x10001. *)
  let t = Vdp.create () in
  reg t 0 0x0e;
  reg t 1 0x40;
  poke t 3 0xaa;
  check "before the line start, address 3 is still linear" (Vdp.vram_read t 3 = 0xaa);
  run t 39;
  poke t 3 0xbb;
  check "after it, address 3 is planar" (Vdp.vram_read t 0x10001 = 0xbb);

  (* A bit waiting for the line start survives a save. *)
  let t = screen5 () in
  goto t ~line:50 ~cycle:100;
  reg t 1 0x00;
  let w = State_codec.writer () in
  Vdp.write_state w t;
  Vdp.write_appended w t;
  let r = State_codec.reader (State_codec.finish w) in
  let q = Vdp.create () in
  Vdp.read_state r q;
  Vdp.read_appended r q;
  State_codec.end_of_input r;
  check "restored: line 50 drawn" (px q 0 50 = white);
  check "restored: line 51 blank" (px q 0 51 = black);
  check "restored frame equals the original" (px t 0 51 = black && Vdp.frame_rgb q = Vdp.frame_rgb t);

  (* TEXT1: the line starts 258 ticks in, cycle 43. Character 0 has its
     top row set, so display line 96 (row 12, dot row 0) shows FG dots from
     x = 9 on. R#7 = 0xF0: FG white, BG and border black. *)
  let text1 () =
    let t = Vdp.create () in
    palette t 15 (7, 7, 7);
    reg t 0 0x00;
    reg t 1 0x50;
    reg t 2 0x00;
    reg t 4 0x01;
    reg t 7 0xf0;
    poke t 0x800 0xfc;
    t
  in
  let t = text1 () in
  goto t ~line:96 ~cycle:42;
  reg t 1 0x10;
  check "TEXT1 BL at cycle 42: line 96 blank" (px t 9 96 = black);
  let t = text1 () in
  goto t ~line:96 ~cycle:43;
  reg t 1 0x10;
  check "TEXT1 BL at cycle 43: line 96 drawn" (px t 9 96 = white);

  if !failures > 0 then begin
    Printf.eprintf "line_start_test: %d failure(s)\n" !failures;
    exit 1
  end;
  print_endline "line_start_test: all passed"
