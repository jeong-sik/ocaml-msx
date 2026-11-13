(* An NMOS Z80 that takes an IRQ right after LD A,I or LD A,R leaves P/V at
   0, not at the IFF2 copy the instruction made (openMSX
   CPUCore::executeSlow). C-BIOS CALSLT keeps IFF2 with LD A,I; PUSH AF and
   runs EI on return only when P/V is set: an IRQ in that window returns
   the caller with interrupts off. *)

let ram = Bytes.make 0x10000 '\000'
let read a = Char.code (Bytes.get ram a)
let write a v = Bytes.set ram a (Char.chr (v land 0xff))
let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let pv z = Z80.dump_f z land 0x04 <> 0

(* LD SP,0xF000; IM 1; EI; NOP; LD A,n; LD I,A or LD R,A; LD A,I or LD A,R;
   then [gap] NOPs. Interrupts are on and IFF2 is 1 at the LD A,I/R. *)
let cpu_after ~load ~gap =
  Bytes.fill ram 0 0x10000 '\000';
  let set, get = match load with `I -> (0x47, 0x57) | `R -> (0x4f, 0x5f) in
  let code = [ 0x31; 0x00; 0xf0; 0xed; 0x56; 0xfb; 0x00; 0x3e; 0x80; 0xed; set; 0xed; get ] in
  List.iteri (fun i b -> write i b) code;
  let z = Z80.create ~read ~write ~port_in:(fun _ -> 0xff) ~port_out:(fun _ _ -> ()) in
  for _ = 1 to 7 + gap do ignore (Z80.step z) done;
  z

let () =
  let z = cpu_after ~load:`I ~gap:0 in
  check "LD A,I copies IFF2 = 1 into P/V" (pv z);
  check "IRQ right after LD A,I is accepted" (Z80.interrupt z);
  check "IRQ right after LD A,I clears P/V" (not (pv z));
  check "only P/V changes" (Z80.dump_f z = 0x80);
  let z = cpu_after ~load:`R ~gap:0 in
  check "IRQ right after LD A,R clears P/V" (Z80.interrupt z && not (pv z));
  let z = cpu_after ~load:`I ~gap:1 in
  check "IRQ one instruction later keeps P/V" (Z80.interrupt z && pv z);
  (* The window survives a save between LD A,I and the IRQ. *)
  let z = cpu_after ~load:`I ~gap:0 in
  let w = State_codec.writer () in
  Z80.write_state w z;
  let restored = Z80.create ~read ~write ~port_in:(fun _ -> 0xff) ~port_out:(fun _ _ -> ()) in
  let r = State_codec.reader (State_codec.finish w) in
  Z80.read_state r restored;
  State_codec.end_of_input r;
  check "restored CPU still clears P/V on the IRQ" (Z80.interrupt restored && not (pv restored));
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "z80_ldai_irq_test: all pass"
