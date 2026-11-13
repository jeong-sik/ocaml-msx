(* Undocumented X/Y flags (0x28) of SCF, CCF and IN r,(C), as openMSX
   CPUCore computes them on an NMOS Z80:
   - SCF and CCF: (F | A) & 0x28, so a bit already set in F stays set;
   - IN r,(C): X and Y (with S, Z, P/V) come from the byte read; only C stays.
   Each case loads AF with PUSH BC; POP AF first. *)

let ram = Bytes.make 0x10000 '\000'
let read a = Char.code (Bytes.get ram a)
let write a v = Bytes.set ram a (Char.chr (v land 0xff))
let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let scf = [ 0x37 ]
let ccf = [ 0x3f ]
let in_a_c = [ 0x01; 0x99; 0x00; 0xed; 0x78 ] (* LD BC,0x0099; IN A,(C) *)

(* LD SP,0xF000; LD BC,af; PUSH BC; POP AF; code — F after the last one. *)
let flags_after ?(port = 0xff) ~a ~f code =
  Bytes.fill ram 0 0x10000 '\000';
  let prologue = [ 0x31; 0x00; 0xf0; 0x01; f; a; 0xc5; 0xf1 ] in
  List.iteri (fun i b -> write i b) (prologue @ code);
  let z = Z80.create ~read ~write ~port_in:(fun _ -> port) ~port_out:(fun _ _ -> ()) in
  let steps = 4 + if code = in_a_c then 2 else 1 in
  for _ = 1 to steps do ignore (Z80.step z) done;
  Z80.dump_f z

let () =
  check "SCF keeps X/Y set in F (A = 0)" (flags_after ~a:0x00 ~f:0x28 scf = 0x29);
  check "SCF takes X/Y from A (F = 0)" (flags_after ~a:0x28 ~f:0x00 scf = 0x29);
  check "SCF keeps S Z P, clears H N" (flags_after ~a:0x00 ~f:0xd6 scf = 0xc5);
  check "CCF keeps X/Y set in F, H = old C" (flags_after ~a:0x03 ~f:0xbb ccf = 0xb8);
  check "CCF takes X/Y from A (F = 0)" (flags_after ~a:0x28 ~f:0x00 ccf = 0x29);
  check "IN A,(C) takes X/Y from the byte, keeps C"
    (flags_after ~port:0x6f ~a:0x00 ~f:0x01 in_a_c = 0x2d);
  check "IN A,(C) clears X/Y the byte does not have"
    (flags_after ~port:0x00 ~a:0x00 ~f:0x28 in_a_c = 0x44);
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "z80_xy_flags_test: all pass"
