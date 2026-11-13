(* Frame boundaries stay on the 59736-cycle grid.

   Msx.step stops a frame at the instruction that reaches the boundary. The
   cycles that instruction runs past it belong to the next frame; when each
   frame started a fresh 59736-cycle budget, N frames ran N * (59736 +
   overshoot) cycles, 2090 too many after 600 frames of an MSXgl sample,
   and a dump "at frame 600" was not at 600 * 59736 / 3579545 s. The CPU's
   T-state count and the frame count are both in the save state, so a
   restored machine keeps the same grid. Expected positions come from a
   VDP run alone for exactly k * 59736 cycles. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let frame = 262 * 228

(* Every page on RAM (slot 3-2, mapper segments 3-0) running LD SP,0xF000
   then a loop of EX (SP),HL (20), INC HL (7) twice, JP (11): 45 cycles,
   which does not divide 59736 (38 would), so the last instruction of a
   frame runs past the boundary by a varying amount. *)
let machine () =
  let t = Msx.create ~machine:{ Msx.ram_kb = 64; vram_kb = 128; roms = [ ""; ""; "" ] } in
  Msx.port_out t 0xa8 0xff;
  Msx.mem_write t 0xffff 0xaa;
  List.iteri (fun i seg -> Msx.port_out t (0xfc + i) seg) [ 3; 2; 1; 0 ];
  List.iteri (fun i b -> Msx.mem_write t i b) [ 0x31; 0x00; 0xf0; 0xe3; 0x23; 0x23; 0xc3; 0x03; 0x00 ];
  t

(* Cycles the machine's VDP is past the reference VDP; both are near the
   same frame boundary, inside one line pair. *)
let ahead t r =
  let l1, c1 = Msx.vdp_line t and l0, c0 = Vdp.line_now r in
  ((l1 * 228) + c1) - ((l0 * 228) + c0)

let () =
  let t = machine () in
  let r = Vdp.create () in
  let worst = ref 0 in
  for k = 1 to 600 do
    Msx.step t ~frames:1;
    ignore (Vdp.advance r ~cycles:frame);
    let d = ahead t r in
    worst := max !worst d;
    if d < 0 || d >= 20 then
      check (Printf.sprintf "frame %d ends within one instruction of %d cycles (%d past)" k (k * frame) d) false
  done;
  check "frame_number counts the frames" (Msx.frame_number t = 600);
  check "some frame ends past the boundary" (!worst > 0);

  (* Save at frame 300 and restore: the restored machine meets frame 600 at
     the same cycle as one that ran straight through. *)
  let a = machine () in
  Msx.step a ~frames:300;
  let saved = Msx.serialize a in
  Msx.step a ~frames:300;
  (match Msx.restore ~state:saved with
   | Error e -> check ("restore: " ^ e) false
   | Ok b ->
     Msx.step b ~frames:300;
     check "a restored machine ends frame 600 where the original did"
       (Msx.vdp_line b = Msx.vdp_line a && Msx.frame_number b = 600));

  if !failures > 0 then begin
    Printf.eprintf "frame_clock_test: %d failure(s)\n%!" !failures;
    exit 1
  end;
  print_endline "frame_clock_test: all pass"
