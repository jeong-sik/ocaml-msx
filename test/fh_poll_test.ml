(* S#1 FH while IE1 is off, against openMSX 21.0 VDP::peekStatusReg. With
   the line interrupt disabled FH is not latched: bit 0 reads 1 while the
   beam is between the right border of line (R#19 - R#23) mod 256 and tick
   202 of the line below, and reading S#1 does not clear it. With IE1 on
   the latched FH is unchanged: the first read returns it and clears it.

   This model's line starts at openMSX tick 1284, so that window is the
   first 48 cycles (the display edge) of the line after the matching one.
   The display line counter that R#19 compares with stops at the counter
   reset: a match after it never comes (scheduleHScan), and the line that
   ended just before it still does. *)

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
let window = 48

let fh t =
  reg t 15 1;
  let v = Vdp.io_read t ~port:0x99 in
  reg t 15 0;
  v land 1

let seek t line cycle =
  let budget = ref (2 * 313 * line_cycles) in
  while Vdp.line_now t <> (line, cycle) do
    if !budget = 0 then failwith (Printf.sprintf "the beam never reached %d:%d" line cycle);
    decr budget;
    ignore (Vdp.advance t ~cycles:1)
  done

(* Graphic 4, display on, IE1 off, 192 lines. *)
let g4 () =
  let t = Vdp.create () in
  reg t 0 0x06;
  reg t 1 0x40;
  ignore (Vdp.advance t ~cycles:(2 * frame_cycles));
  t

let save t =
  let w = State_codec.writer () in
  Vdp.write_state w t;
  Vdp.write_appended w t;
  State_codec.finish w

let restore s =
  let r = State_codec.reader s in
  let u = Vdp.create () in
  Vdp.read_state r u;
  Vdp.read_appended r u;
  State_codec.end_of_input r;
  u

(* The same state with the v15 flag byte removed, labelled v14. *)
let as_v14 s =
  let header = String.length "OCAML-MSX\000\015" + 16 in
  let payload = String.sub s header (String.length s - header - 1) in
  "OCAML-MSX\000\014" ^ Digest.string payload ^ payload

let () =
  let t = g4 () in
  reg t 19 100;
  seek t 100 (line_cycles - 1);
  check "before the right border of the matching line: 0" (fh t = 0);
  seek t 101 0;
  check "the line below, cycle 0: 1" (fh t = 1);
  check "a read does not clear it" (fh t = 1);
  seek t 101 (window - 1);
  check "cycle 47: 1" (fh t = 1);
  seek t 101 window;
  check "cycle 48: 0" (fh t = 0);
  seek t 102 0;
  check "two lines below: 0" (fh t = 0);

  (* R#23 scrolls the counter: 110 - 10 matches line 100. *)
  let t = g4 () in
  reg t 19 110;
  reg t 23 10;
  seek t 101 0;
  check "R#19 - R#23 picks the line" (fh t = 1);
  reg t 23 0;
  check "an R#23 write moves the match at once" (fh t = 0);

  (* An R#19 write inside the window takes effect at once. *)
  let t = g4 () in
  reg t 19 50;
  seek t 101 10;
  check "R#19 elsewhere: 0" (fh t = 0);
  reg t 19 100;
  check "R#19 written to the line above: 1" (fh t = 1);
  reg t 19 101;
  check "R#19 written to this line: 0" (fh t = 0);

  (* Border lines count until the counter reset at line 235: line 234 still
     matches, the reset line itself does not. *)
  let t = g4 () in
  reg t 19 200;
  seek t 201 0;
  check "a bottom border line matches" (fh t = 1);
  reg t 19 234;
  seek t 235 0;
  check "the line before the counter reset matches" (fh t = 1);
  seek t 235 10;
  let s = save t in
  check "v15 restore keeps that match" (fh (restore s) = 1);
  check "v14 restore takes the flag from the counter state" (fh (restore (as_v14 s)) = 0);
  reg t 19 235;
  seek t 236 0;
  check "the counter reset line does not match" (fh t = 0);

  (* 212 lines with R#19 = 212 (Bozo's Big Adventure waits on this). *)
  let t = g4 () in
  reg t 9 0x80;
  ignore (Vdp.advance t ~cycles:(2 * frame_cycles));
  reg t 19 212;
  seek t 213 5;
  check "212 lines: line 212 matches" (fh t = 1);

  (* IE1 on: the latched FH is returned once and cleared by the read. *)
  let t = g4 () in
  reg t 0 0x16;
  reg t 19 100;
  seek t 101 0;
  check "IE1 on: latched FH reads 1" (fh t = 1);
  check "IE1 on: the read clears it" (fh t = 0);

  if !failures > 0 then exit 1;
  print_endline "fh_poll_test: all pass"
