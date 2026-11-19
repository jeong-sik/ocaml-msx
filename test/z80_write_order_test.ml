(* The order of the two byte accesses of a 16-bit load or store. openMSX
   CPUCore writes the high byte first for PUSH (and so CALL, RST and an
   interrupt) and for EX (SP),rr (WR_WORD_rev), and the low byte first for
   LD (nn),rr (WR_WORD). It reads the low byte first (RD_WORD). A
   memory-mapped register sees the order: the value left in it is the
   second byte. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let ram = Bytes.make 0x10000 '\000'
let log : (int * int) list ref = ref []
(* Data reads at or above 0x8000; the code stays below it. *)
let reads : int list ref = ref []
let read a =
  if a >= 0x8000 then reads := a :: !reads;
  Char.code (Bytes.get ram a)
let write a v =
  log := (a, v land 0xff) :: !log;
  Bytes.set ram a (Char.chr (v land 0xff))

(* Put [code] at [pc], set SP = 0x8000, run [setup], then [steps]
   instructions, and return the writes of those steps in order. *)
let writes ?(pc = 0x0100) ?(steps = 1) ?(setup = fun _ -> ()) code =
  Bytes.fill ram 0 0x10000 '\000';
  String.iteri (fun i c -> Bytes.set ram (pc + i) c) code;
  let z = Z80.create ~read ~write ~port_in:(fun _ -> 0xff) ~port_out:(fun _ _ -> ()) in
  Z80.set_pc z pc;
  Z80.set_sp z 0x8000;
  setup z;
  log := [];
  reads := [];
  for _ = 1 to steps do ignore (Z80.step z) done;
  (z, List.rev !log)

(* 0x1234 pushed from SP = 0x8000. *)
let high_first = [ (0x7fff, 0x12); (0x7ffe, 0x34) ]

let () =
  check "PUSH BC writes the high byte first"
    (snd (writes ~setup:(fun z -> Z80.set_bc z 0x1234) "\xc5") = high_first);
  check "PUSH IX writes the high byte first"
    (snd (writes ~setup:(fun z -> Z80.set_ix z 0x1234) "\xdd\xe5") = high_first);
  (* CALL 0x2000 at 0x1231 and RST 38h at 0x1233 both push 0x1234. *)
  check "CALL pushes the high byte first"
    (snd (writes ~pc:0x1231 "\xcd\x00\x20") = high_first);
  check "RST pushes the high byte first"
    (snd (writes ~pc:0x1233 "\xff") = high_first);
  (* IM 1, EI, NOP from 0x1230: the interrupt pushes 0x1234. *)
  let z, _ = writes ~pc:0x1230 ~steps:3 "\xed\x56\xfb\x00\x00" in
  log := [];
  check "the interrupt is taken" (Z80.interrupt z);
  check "an interrupt pushes the high byte first" (List.rev !log = high_first);
  (* EX (SP),rr with rr = 0x1234: (SP+1) first, then (SP). *)
  let ex = [ (0x8001, 0x12); (0x8000, 0x34) ] in
  check "EX (SP),HL writes the high byte first"
    (snd (writes ~setup:(fun z -> Z80.set_hl z 0x1234) "\xe3") = ex);
  check "EX (SP),IY writes the high byte first"
    (snd (writes ~setup:(fun z -> Z80.set_iy z 0x1234) "\xfd\xe3") = ex);
  (* LD (0x9000),HL and LD (0x9000),BC store the low byte first. *)
  let ld = [ (0x9000, 0x34); (0x9001, 0x12) ] in
  check "LD (nn),HL writes the low byte first"
    (snd (writes ~setup:(fun z -> Z80.set_hl z 0x1234) "\x22\x00\x90") = ld);
  check "LD (nn),BC writes the low byte first"
    (snd (writes ~setup:(fun z -> Z80.set_bc z 0x1234) "\xed\x43\x00\x90") = ld);
  (* POP, LD HL,(nn) and EX (SP),HL read the low byte first. *)
  let read_order code =
    ignore (writes code);
    List.rev !reads
  in
  check "POP reads the low byte first" (read_order "\xc1" = [ 0x8000; 0x8001 ]);
  check "LD HL,(nn) reads the low byte first"
    (read_order "\x2a\x00\x90" = [ 0x9000; 0x9001 ]);
  check "EX (SP),HL reads the low byte first" (read_order "\xe3" = [ 0x8000; 0x8001 ]);
  if !failures > 0 then begin
    Printf.eprintf "z80_write_order_test: %d failure(s)\n" !failures;
    exit 1
  end;
  print_endline "z80_write_order_test: all passed"
