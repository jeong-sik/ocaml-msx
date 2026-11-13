(* Vertical frame timing against openMSX. Power-on is a frame start
   (VDP::reset calls frameStart), and frame starts then come every
   lines_per_frame lines whatever the display does. The display line
   counter resets 8 + ((R#18 >> 4) ^ 7) lines after a frame start (master
   scheduleLineCountReset) and reads 0, display line 0, 17/27/44/54 lines
   later by R#9 as it is at the reset (execLineCountReset). These checks
   drive the VDP through port 0x99 and advance only.

   This model's line starts at the right border, openMSX tick 1282, and one
   Z80 cycle is 6 VDP ticks. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let reg t r v =
  Vdp.io_write t ~port:0x99 v;
  Vdp.io_write t ~port:0x99 (0x80 lor r)

let line_cycles = 228
let frame_cycles = 262 * line_cycles
let vblank t = Vdp.status0 t land 0x80 <> 0

let eo t =
  reg t 15 2;
  let v = Vdp.io_read t ~port:0x99 in
  reg t 15 0;
  v land 0x02 <> 0

(* Clear S#0 F by reading S#0. *)
let ack t = ignore (Vdp.io_read t ~port:0x99)

(* Cycles until S#0 F rises; the beam stops on that cycle. *)
let until_vblank t =
  let n = ref 0 in
  while not (vblank t) do
    if !n > 2 * 313 * line_cycles then failwith "VBlank never came";
    ignore (Vdp.advance t ~cycles:1);
    incr n
  done;
  !n

let seek t line cycle =
  let budget = ref (2 * 313 * line_cycles) in
  while Vdp.line_now t <> (line, cycle) do
    if !budget = 0 then failwith (Printf.sprintf "the beam never reached %d:%d" line cycle);
    decr budget;
    ignore (Vdp.advance t ~cycles:1)
  done

(* Cycles from power-on, sampled once a cycle, at which EO flipped. *)
let eo_flips t ~cycles ~at =
  let flips = ref [] and before = ref (eo t) in
  for c = 1 to cycles do
    ignore (Vdp.advance t ~cycles:1);
    at c;
    let now = eo t in
    if now <> !before then flips := c :: !flips;
    before := now
  done;
  List.rev !flips

let () =
  (* openMSX 21.0 raises S#0 F 53386 cycles after power-on: display line 0
     is 42 lines below the frame start, plus 202 ticks, and the display
     ends 192 lines later, (234 * 1368 + 202) / 6 = 53385.7. This model's
     VScan is at the right border, 48 cycles before openMSX's (issue #63). *)
  let t = Vdp.create () in
  check "first VBlank 53338 cycles after power-on" (until_vblank t = 53338);
  check "the first VBlank is display line 192" (Vdp.line_now t = (192, 0));
  ack t;
  check "VBlanks are a frame apart" (until_vblank t = frame_cycles);

  (* Frame starts sit on the power-on grid: EO flips 262 lines apart,
     14 cycles before openMSX's tick 0, through 192/212 and R#18 vertical
     changes, which only move display line 0. *)
  let t = Vdp.create () in
  let flips =
    eo_flips t ~cycles:(6 * frame_cycles) ~at:(fun c ->
        if c = 30_000 then reg t 9 0x80;
        if c = 100_000 then reg t 18 0x30;
        if c = 170_000 then reg t 9 0x00;
        if c = 230_000 then reg t 18 0x00;
        (* between a reset and display line 0 *)
        if Vdp.line_now t = (240, 0) && c > 240_000 then reg t 18 0x70)
  in
  check "frame starts every 262 lines from power-on"
    (flips = List.init 6 (fun k -> ((k + 1) * frame_cycles) - 14));

  (* The display end is R#9 as it is when the counter gets there: a switch
     to 212 lines once 192 has passed ends the display again at 212, in the
     same frame (openMSX changeRegister case 9 calls scheduleVScan). *)
  let t = Vdp.create () in
  ignore (until_vblank t);
  ack t;
  reg t 9 0x80;
  check "192 to 212 lines at line 192: F again 20 lines on"
    (until_vblank t = 20 * line_cycles);

  (* A 212-line switch before the reset moves the next display line 0 ten
     lines up: openMSX VScan goes from frame start + 42 + 192 to the next
     frame start + 32 + 212, 272 lines on. *)
  let t = Vdp.create () in
  ignore (until_vblank t);
  ack t;
  seek t 215 0;
  reg t 9 0x80;
  check "192 to 212 lines in the bottom border: next VBlank 272 lines on"
    (until_vblank t = (272 - (215 - 192)) * line_cycles);
  check "display line 0 came at 252, 10 lines early" (Vdp.line_now t = (212, 0));
  ack t;
  check "then VBlanks are a frame apart again" (until_vblank t = frame_cycles);

  (* After the reset the display start is fixed: a switch then only moves
     the display end. *)
  let t = Vdp.create () in
  ignore (until_vblank t);
  ack t;
  seek t 240 0;
  reg t 9 0x80;
  check "212 lines after the reset: next VBlank 282 lines on"
    (until_vblank t = (282 - (240 - 192)) * line_cycles);

  (* R#18 vertical between the frame start (220) and the reset (235): the
     reset follows the adjust while it stays ahead of the beam. 0x70 is
     adjust -7, a reset 8 lines after the frame start. *)
  let wrap_after r18 ~at_line =
    let t = Vdp.create () in
    seek t at_line 0;
    reg t 18 r18;
    let rec last l =
      ignore (Vdp.advance t ~cycles:line_cycles);
      let now, _ = Vdp.line_now t in
      if now = 0 then l else if now > 400 then -1 else last now
    in
    last at_line
  in
  check "R#18 0x70 at line 225 moves the reset to 228: display line 0 at 255"
    (wrap_after 0x70 ~at_line:225 = 254);
  check "R#18 0x70 at line 230 is behind the beam: the reset stays at 235"
    (wrap_after 0x70 ~at_line:230 = 261);
  check "R#18 0x70 on line 228 itself is too late: the reset stays at 235"
    (wrap_after 0x70 ~at_line:228 = 261);
  check "R#18 0xF0 at line 230 moves the reset to 236: display line 0 at 263"
    (wrap_after 0xf0 ~at_line:230 = 262);

  (* No line interrupt between the reset and display line 0: the counter
     reads below zero there, so R#19 = 240 never matches in the power-on
     frame, while 230, before the reset at 235, does. *)
  let fh_by_261 r19 =
    let t = Vdp.create () in
    reg t 0 0x10;
    reg t 19 r19;
    seek t 261 227;
    Vdp.int_active t
  in
  check "FH on line 230, before the reset" (fh_by_261 230);
  check "no FH on line 240, after the reset" (not (fh_by_261 240));

  (* Save in each vertical phase, restore, and run both on: the same EO
     flips and the same beam. *)
  let save t =
    let w = State_codec.writer () in
    Vdp.write_state w t;
    Vdp.write_appended w t;
    State_codec.finish w
  in
  let restore s =
    let r = State_codec.reader s in
    let u = Vdp.create () in
    Vdp.read_state r u;
    Vdp.read_appended r u;
    State_codec.end_of_input r;
    u
  in
  List.iter
    (fun (name, line) ->
      let t = Vdp.create () in
      seek t 225 0;
      reg t 18 0x70;
      seek t line 0;
      let u = restore (save t) in
      let trace v = eo_flips v ~cycles:(2 * frame_cycles) ~at:(fun _ -> ()) in
      check (name ^ ": same EO flips after restore") (trace u = trace t);
      check (name ^ ": same beam after restore") (Vdp.line_now u = Vdp.line_now t))
    [ ("before the frame start", 100); ("before the moved reset", 226);
      ("before display line 0", 240) ];

  (* A v13 state has no frame start: it is taken from R#9 and R#18, and
     display line 0 stays where that build put it, lines_per_frame lines
     on. Saved on the reset line itself, the reset has happened. *)
  let as_v13 s =
    let header = String.length "OCAML-MSX\000\014" + 16 in
    let payload = String.sub s header (String.length s - header - (3 * 8)) in
    "OCAML-MSX\000\013" ^ Digest.string payload ^ payload
  in
  List.iter
    (fun line ->
      let t = Vdp.create () in
      seek t line 0;
      let u = restore (as_v13 (save t)) in
      let rec last l =
        ignore (Vdp.advance u ~cycles:line_cycles);
        let now, _ = Vdp.line_now u in
        if now = 0 then l else if now > 400 then -1 else last now
      in
      check (Printf.sprintf "v13 at line %d: display line 0 after line 261" line)
        (last line = 261);
      check (Printf.sprintf "v13 at line %d: frame start at 220" line)
        (seek u 219 227; let e = eo u in seek u 220 0; eo u <> e))
    [ 225; 235; 250 ];

  (* A restored state whose pending event is behind the beam is refused. *)
  let t = Vdp.create () in
  seek t 240 0;
  let s = save t in
  let header = String.length "OCAML-MSX\000\014" + 16 in
  let payload = Bytes.of_string (String.sub s header (String.length s - header)) in
  let n = Bytes.length payload in
  Bytes.set_int64_be payload (n - 16) 1L;
  Bytes.set_int64_be payload (n - 8) 230L;
  let payload = Bytes.to_string payload in
  let forged = "OCAML-MSX\000\014" ^ Digest.string payload ^ payload in
  check "a reset behind the beam is refused"
    (match restore forged with
     | _ -> false
     | exception State_codec.Invalid_state "invalid VDP vertical state" -> true);

  if !failures > 0 then exit 1;
  print_endline "frame_phase_test: all pass"
