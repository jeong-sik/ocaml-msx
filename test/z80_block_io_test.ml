(* INI/IND/OUTI/OUTD flags, as openMSX CPUCore BLOCK_IN / BLOCK_OUT set
   them. Each expected F is worked out from that definition:
   S Z F5 F3 from the decremented B, N = bit 7 of the byte, H = C = carry of
   k (byte + L after the step for OUT, byte + (C +/- 1) for IN), P/V =
   parity of (k & 7) xor B.

   Zanac-Ex sends a tile with five OUTIs and JP NZ back, B counting 30 down:
   a Z left set by an earlier instruction must not survive an OUTI that
   leaves B non-zero, or the loop ends after one round. *)

let ram = Bytes.make 0x10000 '\000'
let read a = Char.code (Bytes.get ram a)
let write a v = Bytes.set ram a (Char.chr (v land 0xff))
let port_value = ref 0
let pin _ = !port_value
let outs = ref []
let pout port v = outs := (port, v) :: !outs
let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let poke a bs = String.iteri (fun i c -> Bytes.set ram (a + i) c) bs

(* Run one ED-prefixed block instruction [op] at 0x0100. *)
let run ~op ~af ~bc ~hl =
  poke 0x0100 (Printf.sprintf "\xed%c" (Char.chr op));
  let z = Z80.create ~read ~write ~port_in:pin ~port_out:pout in
  Z80.set_pc z 0x0100;
  Z80.set_af z af;
  Z80.set_bc z bc;
  Z80.set_hl z hl;
  outs := [];
  ignore (Z80.step z);
  z

let flags name z expected =
  check
    (Printf.sprintf "%s: F=%02x, expected %02x" name (Z80.dump_f z) expected)
    (Z80.dump_f z = expected)

let () =
  (* OUTI, Z and N set beforehand, B 0x1E -> 0x1D, byte 0x77, L after 0x01:
     k = 0x78, no carry; B 0x1D gives F3; parity(0 xor 0x1D) even. *)
  write 0x8000 0x77;
  let z = run ~op:0xa3 ~af:0x0042 ~bc:0x1e9b ~hl:0x8000 in
  flags "OUTI clears a stale Z" z 0x0c;
  check "OUTI sends (HL) to port C" (!outs = [ (0x1e9b, 0x77) ]);
  check "OUTI steps B and HL" (Z80.dump_bc z = 0x1d9b && Z80.dump_hl z = 0x8001);
  (* OUTI to B = 0, byte 0x80 (N), L wraps to 0x00: k = 0x80, no carry. *)
  write 0x80ff 0x80;
  flags "OUTI to B = 0" (run ~op:0xa3 ~af:0x0000 ~bc:0x0198 ~hl:0x80ff) 0x46;
  (* OUTI with a carry: byte 0xF0, L after 0x21, k = 0x111 (H, C); B 0x8F
     gives S and F3; parity(1 xor 0x8F) even. *)
  write 0x2020 0xf0;
  flags "OUTI carry" (run ~op:0xa3 ~af:0x0000 ~bc:0x9098 ~hl:0x2020) 0x9f;
  (* OUTD steps HL down: byte 0x01, L after 0xFF, k = 0x100 (H, C); B 0x20
     gives F5; parity(0 xor 0x20) odd. *)
  write 0x3000 0x01;
  let z = run ~op:0xab ~af:0x0000 ~bc:0x2198 ~hl:0x3000 in
  flags "OUTD" z 0x31;
  check "OUTD steps HL down" (Z80.dump_hl z = 0x2fff);
  (* INI: byte 0xC5 (N), C + 1 = 0x99, k = 0x15E (H, C); B 0x01;
     parity(6 xor 1) odd. *)
  port_value := 0xc5;
  let z = run ~op:0xa2 ~af:0x0040 ~bc:0x0298 ~hl:0x4000 in
  flags "INI" z 0x13;
  check "INI stores the byte at HL" (read 0x4000 = 0xc5 && Z80.dump_hl z = 0x4001);
  (* IND: byte 0x01, C - 1 = 0xFF, k = 0x100 (H, C); B 0x20 gives F5. *)
  port_value := 0x01;
  flags "IND" (run ~op:0xaa ~af:0x0000 ~bc:0x2100 ~hl:0x4000) 0x31;
  (* OTIR repeats until B = 0; the last round leaves k = 0 + 0x03 and Z. *)
  Bytes.fill ram 0x9000 3 '\000';
  poke 0x0100 "\xed\xb3";
  let z = Z80.create ~read ~write ~port_in:pin ~port_out:pout in
  Z80.set_pc z 0x0100;
  Z80.set_af z 0x0000;
  Z80.set_bc z 0x0398;
  Z80.set_hl z 0x9000;
  outs := [];
  for _ = 1 to 3 do ignore (Z80.step z) done;
  check "OTIR sends three bytes" (List.length !outs = 3);
  check "OTIR falls through when B = 0" (Z80.dump_pc z = 0x0102);
  flags "OTIR end" z 0x44;
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "z80_block_io_test: all pass"
