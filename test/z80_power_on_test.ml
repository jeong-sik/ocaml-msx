(* Z80 power-on registers, as openMSX CPUCore::doReset sets them: every
   pair, the alternate set included, reads 0xFFFF; PC, I, R and IM are 0 and
   interrupts are off. The alternate set is read back through EXX and
   EX AF,AF'. *)

let ram = Bytes.make 0x10000 '\000'
let read a = Char.code (Bytes.get ram a)
let write a v = Bytes.set ram a (Char.chr (v land 0xff))
let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let () =
  (* 0x0000: EXX, EX AF,AF'. *)
  Bytes.set ram 0 '\xd9';
  Bytes.set ram 1 '\x08';
  let z = Z80.create ~read ~write ~port_in:(fun _ -> 0xff) ~port_out:(fun _ _ -> ()) in
  check "PC starts at 0" (Z80.dump_pc z = 0);
  check "SP is 0xFFFF" (Z80.dump_sp z = 0xffff);
  check "AF is 0xFFFF" (Z80.dump_a z = 0xff && Z80.dump_f z = 0xff);
  check "BC DE HL are 0xFFFF"
    (Z80.dump_bc z = 0xffff && Z80.dump_de z = 0xffff && Z80.dump_hl z = 0xffff);
  check "IX IY are 0xFFFF" (Z80.dump_ix z = 0xffff && Z80.dump_iy z = 0xffff);
  check "interrupts off" (not (Z80.dump_iff1 z));
  ignore (Z80.step z);
  check "BC' DE' HL' are 0xFFFF"
    (Z80.dump_bc z = 0xffff && Z80.dump_de z = 0xffff && Z80.dump_hl z = 0xffff);
  ignore (Z80.step z);
  check "AF' is 0xFFFF" (Z80.dump_a z = 0xff && Z80.dump_f z = 0xff);
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "z80_power_on_test: all pass"
