(* Determinism is what makes replay honest (RFC-0439 §3.6): the same cartridge
   and the same inputs at the same frames must reproduce the same frames, so a
   keeper's ledger replays to exactly what the keeper saw. No ROMs needed --
   the bus reads 0xFF and the property holds regardless of what is on screen. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let fresh () =
  Msx.create ~machine:{ Msx.ram_kb = 512; vram_kb = 128; roms = [ ""; ""; "" ] }

(* Drive a machine: boot, then at each (frame, key, down) apply the edge and
   step, collecting the RGB frame every [every]. *)
let run ~script ~total ~every =
  let t = fresh () in
  Msx.step t ~frames:45;
  let shots = ref [] in
  for f = 46 to total do
    List.iter
      (fun (at, k, down) -> if at = f then ignore (Msx.set_key t k ~pressed:down : bool))
      script;
    Msx.step t ~frames:1;
    if f mod every = 0 then shots := Msx.frame_rgb t :: !shots
  done;
  List.rev !shots

let () =
  let script =
    [ (60, Msx.Space, true); (65, Msx.Space, false)
    ; (90, Msx.Right, true); (110, Msx.Right, false)
    ; (120, Msx.Char 'z', true); (123, Msx.Char 'z', false)
    ]
  in
  let a = run ~script ~total:200 ~every:10 in
  let b = run ~script ~total:200 ~every:10 in
  check "the run is non-empty" (a <> []);
  check "same run count" (List.length a = List.length b);
  check "every frame is byte-identical across two replays"
    (List.for_all2 String.equal a b);
  (* Divergence on a different input needs a ROM whose code reads the keyboard;
     with no BIOS nothing samples it, so that half of the guarantee is proven
     visually by replaying a real cartridge, not here. *)

  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "replay_test: determinism holds"
