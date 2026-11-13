(* How long a VDP command runs. The engine reaches VRAM only in the access
   slots the display fetch leaves free, one access per slot, with a fixed
   least gap between its accesses (openMSX VDPAccessSlots and
   VDPCmdEngine, measured on a real V9938). Before this the block commands
   ended inside the CMR write: CE was never seen set and a copy took the
   source as it was at that instant (WebMSX issue 81: a scroller that
   writes a column while HMMM copies the screen breaks that way). The
   absolute times are checked against openMSX with a probe ROM; these
   cases pin the behaviour around them. *)

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

let ce t =
  reg t 15 2;
  let v = Vdp.io_read t ~port:0x99 in
  reg t 15 0;
  v land 1 <> 0

let run t cycles = ignore (Vdp.advance t ~cycles)

(* R#32-R#46 through R#17, CMR last. *)
let command t ?(sx = 0) ?(sy = 0) ?(dx = 0) ?(dy = 0) ?(nx = 0) ?(ny = 0)
    ?(col = 0) ?(arg = 0) cmr =
  reg t 17 32;
  List.iter (Vdp.io_write t ~port:0x9b)
    [ sx land 0xff; sx lsr 8; sy land 0xff; sy lsr 8;
      dx land 0xff; dx lsr 8; dy land 0xff; dy lsr 8;
      nx land 0xff; nx lsr 8; ny land 0xff; ny lsr 8;
      col; arg; cmr ]

(* Cycles until CE drops. *)
let duration t =
  let n = ref 0 in
  while ce t do
    run t 1;
    incr n
  done;
  !n

let hmmv = 0xc0
let hmmm = 0xd0
let lmmm = 0x90
let line = 0x70

type screen = Off | Sprites_off | Sprites_on

(* SCREEN5 at [line] cycle [cycle], past the line start that takes the
   mode, blank and sprite bits. *)
let screen5 ?(line = 10) ?(cycle = 60) screen =
  let t = Vdp.create () in
  reg t 0 0x06;
  reg t 1 (match screen with Off -> 0x00 | Sprites_off | Sprites_on -> 0x40);
  reg t 8 (match screen with Sprites_off -> 0x0a | Off | Sprites_on -> 0x08);
  run t ((line * cycles_per_line) + cycle);
  t

let hmmm_time screen =
  let t = screen5 screen in
  command t ~sy:0 ~dy:100 ~nx:64 ~ny:8 hmmm;
  duration t

(* Exact times, worked out by hand from the slot tables. Every case starts
   at line 10 cycle 60 unless it says otherwise: tick 360 of this model's
   line, which is tick (360 + 1282) mod 1368 = 274 of openMSX's line. A
   result is the cycles until S#2 reads CE clear, so an access at tick T
   (openMSX T - 86 here) shows from the first cycle boundary after T.
   Screen-off slots near there: 268 276 292 300 ... 404 420 ... 532 548 ...;
   sprites-off: 252 278 310 316 342; sprites-on: 252 316 348 380. *)
let timed ?line ?cycle screen ?sx ?sy ?dx ?dy ?nx ?ny ?col ?arg cmr =
  let t = screen5 ?line ?cycle screen in
  command t ?sx ?sy ?dx ?dy ?nx ?ny ?col ?arg cmr;
  duration t

let () =
  (* POINT: one read at 276 (362 here, cycle 60.3). *)
  check "POINT reads at the first slot" (timed Off 0x40 = 1);
  (* PSET: read 276, write at the first slot 24 later, 300 (cycle 64.3). *)
  check "PSET reads, then writes 24 ticks later" (timed Off 0x50 = 5);
  (* From cycle 58 (tick 262): read 268, write 292 = 378 here, exactly
     cycle 63, so CE shows clear from cycle 64. *)
  check "an access on a cycle boundary shows from the next cycle"
    (timed ~cycle:58 Off 0x50 = 6);
  (* HMMV, two bytes in a row: 276, then 48 later 324 (cycle 68.3). *)
  check "HMMV bytes are 48 ticks apart" (timed Off ~dy:50 ~nx:4 ~ny:1 ~col:0x11 hmmv = 9);
  (* HMMV, one byte in each of two rows: 276, then 104 later 380. *)
  check "HMMV turns a row in 104 ticks" (timed Off ~dy:50 ~nx:2 ~ny:2 ~col:0x11 hmmv = 18);
  (* The display fetch leaves fewer slots. Sprites off: 278, then 342;
     sprites on: 316, then 380. *)
  check "HMMV with sprites off" (timed Sprites_off ~dy:50 ~nx:4 ~ny:1 hmmv = 12);
  check "HMMV with sprites on" (timed Sprites_on ~dy:50 ~nx:4 ~ny:1 hmmv = 18);
  (* Line 192 is past the last display line (R#9 LN = 0), but the display
     area ends only at cycle 48: from cycle 10 (openMSX 1342) the
     sprites-on slots 1368+28 and 1368+92 give 178 here, cycle 29.7. *)
  check "the display area ends at cycle 48 of the line below it"
    (timed ~line:192 ~cycle:10 Sprites_on ~dy:50 ~nx:4 ~ny:1 hmmv = 20);
  check "the bottom border has the screen-off slots"
    (timed ~line:215 Sprites_on ~dy:50 ~nx:4 ~ny:1 hmmv = 9);
  (* HMMM, one byte: read 276, write 300. *)
  check "HMMM writes 24 ticks after its read" (timed Off ~sy:300 ~dy:50 ~nx:2 ~ny:1 hmmm = 5);
  (* Two bytes: 276 300, then 64 later 364 388 = 474 here, cycle 79. *)
  check "HMMM steps 64 ticks a byte" (timed Off ~sy:300 ~dy:50 ~nx:4 ~ny:1 hmmm = 20);
  (* Two rows of one byte: 276 300, then 128 later 428 452. *)
  check "HMMM turns a row in 128 ticks" (timed Off ~sy:300 ~dy:50 ~nx:2 ~ny:2 hmmm = 30);
  (* YMMM from DX 252, two bytes: 276 300, then 40 later 340 364. *)
  check "YMMM steps 40 ticks a byte"
    (timed Off ~sy:300 ~dx:252 ~dy:50 ~ny:1 0xe0 = 16);
  (* LMMV, two dots: 276 300, then 72 later 372 396. *)
  check "LMMV steps 72 ticks a dot" (timed Off ~dy:50 ~nx:2 ~ny:1 ~col:5 0x80 = 21);
  (* Two rows of one dot: 276 300, then 136 later 436 460 = 546, cycle 91. *)
  check "LMMV turns a row in 136 ticks" (timed Off ~dy:50 ~nx:1 ~ny:2 ~col:5 0x80 = 32);
  (* LMMM, two dots: source 276, destination 308 and 332, then 64 later
     396, 428, 452. *)
  check "LMMM reads source, reads and writes destination"
    (timed Off ~sy:300 ~dx:1 ~dy:50 ~nx:2 ~ny:1 lmmm = 30);
  (* Two rows of one dot: 276 308 332, then 128 later 460 492 516. *)
  check "LMMM turns a row in 128 ticks" (timed Off ~sy:300 ~dx:1 ~dy:50 ~nx:1 ~ny:2 lmmm = 41);
  (* SRCH from 254 to the edge: 276, 364. *)
  check "SRCH steps 88 ticks a dot" (timed Off ~sx:254 ~sy:300 ~col:9 0x60 = 16);
  (* LINE NX 2 NY 1: dot one 276 300 and a minor step (120 more), dot two
     420 444, dot three 532 556 = 642 here, exactly cycle 107. *)
  check "LINE: 88 ticks a dot, 32 more on a minor step"
    (timed Off ~dx:9 ~dy:50 ~nx:2 ~ny:1 ~col:7 line = 48);

  (* CE is set from the CMR write until the last access. *)
  let t = screen5 Off in
  command t ~dy:50 ~nx:128 ~ny:1 ~col:0x11 hmmv;
  check "HMMV raises CE" (ce t);
  ignore (duration t);
  check "HMMV filled its row" (Vdp.vram_read t (50 * 128) = 0x11 && Vdp.vram_read t ((50 * 128) + 63) = 0x11);

  (* A CPU VRAM access claims the first slot 16 ticks on. At cycle 65
     (openMSX 304) that is 324, the slot of HMMV's second byte, which moves
     to 332: one cycle later than alone. *)
  let t = screen5 Off in
  command t ~dy:50 ~nx:4 ~ny:1 ~col:0x11 hmmv;
  run t 5;
  seek t 0;
  Vdp.io_write t ~port:0x98 0;
  check "a CPU write takes the slot the engine wanted" (5 + duration t = 10);
  (* HMMM reads its byte at 276 and writes it at 300; a CPU write to the
     source between the two (cycle 62) does not reach the destination. *)
  let t = screen5 Off in
  poke t (300 * 128) 0x11;
  command t ~sy:300 ~dy:50 ~nx:2 ~ny:1 hmmm;
  run t 2;
  poke t (300 * 128) 0x22;
  ignore (duration t);
  check "HMMM writes the byte it read" (Vdp.vram_read t (50 * 128) = 0x11);

  (* The display fetch leaves fewer slots: screen off is fastest, sprites
     on slowest. *)
  let off = hmmm_time Off and spr_off = hmmm_time Sprites_off and spr_on = hmmm_time Sprites_on in
  check "screen off is faster than sprites off" (off < spr_off);
  check "sprites off is faster than sprites on" (spr_off < spr_on);

  (* The border lines have the screen-off slots: the same HMMM runs faster
     from line 215 (bottom border) than from line 10. *)
  let t = screen5 ~line:215 Sprites_on in
  command t ~sy:0 ~dy:100 ~nx:16 ~ny:2 hmmm;
  let border = duration t in
  let t = screen5 ~line:10 Sprites_on in
  command t ~sy:0 ~dy:100 ~nx:16 ~ny:2 hmmm;
  check "the bottom border runs a command faster" (border < duration t);

  (* A copy reads its source as it goes: the CPU rewrites row 100 while
     HMMM copies it to row 101, so the early bytes keep the old value and
     the late ones take the new one. *)
  let t = screen5 Off in
  for i = 0 to 127 do poke t ((100 * 128) + i) 0x11 done;
  command t ~sy:100 ~dy:101 ~nx:0 ~ny:1 hmmm;
  run t 100;
  for i = 0 to 127 do poke t ((100 * 128) + i) 0x22 done;
  ignore (duration t);
  check "copy before the CPU write keeps the old byte" (Vdp.vram_read t (101 * 128) = 0x11);
  check "copy after the CPU write takes the new byte" (Vdp.vram_read t ((101 * 128) + 127) = 0x22);

  (* COL is live: changing R#44 mid-HMMV changes the rest of the fill. *)
  let t = screen5 Off in
  command t ~dy:60 ~nx:0 ~ny:1 ~col:0x33 hmmv;
  run t 100;
  reg t 44 0x44;
  ignore (duration t);
  check "HMMV before the R#44 write" (Vdp.vram_read t (60 * 128) = 0x33);
  check "HMMV after the R#44 write" (Vdp.vram_read t ((60 * 128) + 127) = 0x44);

  (* Leaving GRAPHIC 4-7 ends the command at the next line start. *)
  let t = screen5 Off in
  command t ~dy:80 ~nx:0 ~ny:100 ~col:0x66 hmmv;
  run t 300;
  reg t 0 0x00;
  run t cycles_per_line;
  check "a text or character mode ends the command" (not (ce t));
  check "the rest of the area is not filled" (Vdp.vram_read t (179 * 128) = 0);

  (* A save in the middle of a command restores to the same progress: the
     restored VDP finishes at the same cycle with the same VRAM. *)
  let save t =
    let w = State_codec.writer () in
    Vdp.write_state w t;
    Vdp.write_appended w t;
    let r = State_codec.reader (State_codec.finish w) in
    let q = Vdp.create () in
    Vdp.read_state r q;
    Vdp.read_appended r q;
    State_codec.end_of_input r;
    q
  in
  (* Saved at every cycle of the first 40 of an LMMM (all three accesses
     of a dot, and the waits between), each copy finishes like the
     original. *)
  let lmmm_from screen k =
    let t = screen5 screen in
    for i = 0 to 255 do poke t (i * 3) i done;
    for i = 0 to (6 * 128) - 1 do poke t ((120 * 128) + i) (i * 7) done;
    command t ~sx:3 ~sy:0 ~dx:7 ~dy:120 ~nx:40 ~ny:6 (lmmm lor 0x03);
    run t k;
    t
  in
  List.iter
    (fun (name, screen) ->
      let same = ref true in
      for k = 0 to 40 do
        let t = lmmm_from screen k in
        let q = save t in
        let n = duration t in
        if not (ce q) || duration q <> n || not (Bytes.equal (Vdp.vram t) (Vdp.vram q)) then
          same := false
      done;
      check ("LMMM saved at any cycle finishes like the original, " ^ name) !same)
    [ ("screen off", Off); ("sprites on", Sprites_on) ];
  (* A long command's accesses can fall back into step at a gap in the
     slots; a short one shows a lost wait at once. HMMV of two bytes,
     saved at each cycle before its end (9 cycles alone). *)
  let same = ref true in
  for k = 0 to 8 do
    let t = screen5 Off in
    command t ~dy:50 ~nx:4 ~ny:1 ~col:0x11 hmmv;
    run t k;
    if k + duration (save t) <> 9 then same := false
  done;
  check "HMMV saved at any cycle ends at the same cycle" !same;
  (* A claimed slot can wait for a later engine access. Sprites on, HMMV
     of three bytes: 316, 380, 444. A CPU write at cycle 76 (openMSX 370)
     claims 444 while the engine still waits for 380; the third byte then
     moves to 476 (cycle 93.7). Alone it ends at 444 (cycle 88.3). *)
  let three () =
    let t = screen5 Sprites_on in
    command t ~dy:50 ~nx:6 ~ny:1 ~col:0x11 hmmv;
    run t 16;
    seek t 0;
    Vdp.io_write t ~port:0x98 0;
    t
  in
  check "HMMV of three bytes alone" (timed Sprites_on ~dy:50 ~nx:6 ~ny:1 hmmv = 29);
  check "a claimed slot waits for the engine access due there" (16 + duration (three ()) = 34);
  check "a claimed slot survives a save" (16 + duration (save (three ())) = 34);

  (* LINE saved before its first dot. *)
  let t = screen5 Off in
  command t ~dx:9 ~dy:9 ~nx:0 ~ny:0 ~col:0x0f line;
  let q = save t in
  ignore (duration t);
  ignore (duration q);
  check "LINE NX=0 restored before its dot draws it"
    (Bytes.equal (Vdp.vram t) (Vdp.vram q) && Vdp.vram_read q ((9 * 128) + 4) = 0x0f);

  if !failures > 0 then begin
    Printf.eprintf "cmd_timing_test: %d failure(s)\n" !failures;
    exit 1
  end;
  print_endline "cmd_timing_test: all passed"
