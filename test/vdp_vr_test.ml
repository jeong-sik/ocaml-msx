(* R#8 VR (bit 3), after openMSX VDPVRAM::setSizeMask and updateVRMode.
   With VR = 0 (16K DRAM wiring) every VRAM access -- CPU port, command
   engine, display -- lands in 0x0000-0x7FFF. R#8 is 0 at power-on. When VR
   changes, the contents follow the wiring: a byte written at VR = 0
   address x sits in cell 1 | (x & 0x7F) << 1 | (x & 0x7FC0) << 2 once VR
   is 1 again, the run of swaps openMSX performs. Cells are read with
   Vdp.vram_read, which sees the physical 128K. *)

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
  reg t 14 ((a lsr 14) land 7);
  Vdp.io_write t ~port:0x99 (a land 255);
  Vdp.io_write t ~port:0x99 (((a lsr 8) land 63) lor 64)

let poke t a v =
  seek t a;
  Vdp.io_write t ~port:0x98 v

let cell t a = Vdp.vram_read t a
let vr1 t = reg t 8 0x08

let xy t r x =
  reg t r (x land 255);
  reg t (r + 1) (x lsr 8)

let pixel t x y =
  let width, _ = Vdp.frame_dims t in
  let rgb = Vdp.frame_rgb t in
  let i = ((y * width) + x) * 3 in
  (Char.code rgb.[i], Char.code rgb.[i + 1], Char.code rgb.[i + 2])

let () =
  (* Power-on: VR = 0, so 0x8000 and 0x10005 reach cells 0 and 5. *)
  let t = Vdp.create () in
  poke t 0x8000 0x22;
  poke t 0x10005 0x33;
  check "VR=0: 0x8000 is cell 0" (cell t 0 = 0x22 && cell t 0x8000 = 0);
  check "VR=0: 0x10005 is cell 5" (cell t 5 = 0x33 && cell t 0x10005 = 0);
  (* With VR = 1 the whole 128K is reachable. *)
  let t = Vdp.create () in
  vr1 t;
  poke t 0x18005 0x44;
  check "VR=1: 0x18005 is its own cell" (cell t 0x18005 = 0x44 && cell t 0x0005 = 0);

  (* VR 0 -> 1 carries VR = 0 address x to cell vr0(x). *)
  let t = Vdp.create () in
  poke t 0x0000 0x44;
  poke t 0x0080 0x55;
  poke t 0x7fff 0x66;
  vr1 t;
  check "VR 0->1: address 0 is cell 1" (cell t 1 = 0x44 && cell t 0 = 0);
  check "VR 0->1: address 0x80 is cell 0x201" (cell t 0x201 = 0x55 && cell t 0x80 = 0);
  check "VR 0->1: address 0x7FFF is cell 0x1FFFF" (cell t 0x1ffff = 0x66);

  (* VR 1 -> 0 brings cell vr0(x) to VR = 0 address x. *)
  let t = Vdp.create () in
  vr1 t;
  poke t 0x201 0x77;
  reg t 8 0x00;
  check "VR 1->0: cell 0x201 shows at address 0x80" (cell t 0x80 = 0x77);

  (* 1 -> 0 -> 1 leaves every cell where it was; an R#8 write that keeps VR
     moves nothing. *)
  let t = Vdp.create () in
  vr1 t;
  for block = 0 to 7 do
    seek t (block * 0x4000);
    for i = 0 to 0x3fff do
      let a = (block * 0x4000) + i in
      Vdp.io_write t ~port:0x98 (((a * 7) + (a lsr 8)) land 0xff)
    done
  done;
  let before = Bytes.copy (Vdp.vram t) in
  reg t 8 0x0a;
  check "R#8 write keeping VR moves nothing" (Bytes.equal before (Vdp.vram t));
  reg t 8 0x00;
  check "VR 1->0 moves cells" (not (Bytes.equal before (Vdp.vram t)));
  reg t 8 0x08;
  check "VR 1->0->1 restores every cell" (Bytes.equal before (Vdp.vram t));

  (* A saved VR = 0 machine comes back with the same cells and the same
     wiring: restoring sets R#8 without the swap run. *)
  let t = Vdp.create () in
  poke t 0x0080 0x55;
  let w = State_codec.writer () in
  Vdp.write_state w t;
  let copy = Vdp.create () in
  Vdp.read_state (State_codec.reader (State_codec.finish w)) copy;
  check "restore keeps VR=0 cells" (Bytes.equal (Vdp.vram t) (Vdp.vram copy));
  vr1 t;
  vr1 copy;
  check "restored VR=0 machine rewires the same" (Bytes.equal (Vdp.vram t) (Vdp.vram copy));

  (* The command engine: HMMV at Y 256 in GRAPHIC 4 (address 0x8000) fills
     cell 0 while VR = 0. *)
  let t = Vdp.create () in
  reg t 0 0x06;
  reg t 1 0x40;
  xy t 36 0;
  xy t 38 256;
  xy t 40 2;
  xy t 42 1;
  reg t 45 0;
  reg t 44 0x9a;
  reg t 46 0xc0;
  check "VR=0: HMMV at 0x8000 fills cell 0" (cell t 0 = 0x9a && cell t 0x8000 = 0);

  (* GRAPHIC 7 interleaves first and masks after (openMSX applies sizeMask
     to the planar address): address 1 is planar 0x10000, which VR = 0
     folds onto cell 0. *)
  let t = Vdp.create () in
  reg t 0 0x0e;
  poke t 0x0001 0x5c;
  check "VR=0 G7: address 1 is cell 0" (cell t 0 = 0x5c && cell t 0x10000 = 0);

  (* The display: GRAPHIC 1 with the name table at 0x8000 (R#2 = 0x20)
     reads cell 0 while VR = 0. Name 1, pattern 0xFF on its first row,
     colour 0xF0: the top-left pixel is colour 15. Read at cell 0x8000 the
     name would be 0, whose first pattern row (cell 0 = 0x01) leaves the
     pixel at the backdrop. *)
  let t = Vdp.create () in
  reg t 1 0x40;
  reg t 2 0x20;
  reg t 3 0x80;
  reg t 4 0x00;
  reg t 7 0x00;
  reg t 16 15;
  Vdp.io_write t ~port:0x9a 0x77;
  Vdp.io_write t ~port:0x9a 0x07;
  poke t 0x8000 0x01;
  poke t 0x0008 0xff;
  poke t 0x2000 0xf0;
  let white = pixel t 0 0 in
  check "VR=0: G1 name table at 0x8000 reads cell 0"
    (let r, g, b = white in r > 200 && g > 200 && b > 200);

  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "vdp_vr_test: all pass"
