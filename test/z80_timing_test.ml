(* MSX 의 Z80 타이밍 계약 증명: M1 대기, R 레지스터, 인터럽트 접수 시간.

   기대값의 출처는 openMSX src/cpu/Z80.hh 다. MSX 의 Z80 은 M1 마다 대기
   1 사이클(WAIT_CYCLES = 1)이 붙어 한 M1 이 5 사이클이다. 표준 Z80 값에
   M1 수만큼 더한 값이 openMSX 의 CC_* 와 같다:
   NOP 4+1 = CC_NOP 5, LD A,n 7+1 = CC_LD_R_N 8, BIT 0,A 8+2 = CC_BIT_R 10,
   LDI 16+2 = CC_LDI 18, LD A,(IX+d) 19+2 = CC_LD_R_XIX + 5 = 21,
   BIT 0,(IX+d) 20+2 = CC_BIT_XIX + 5 = 22, IN A,(n) 11+1 = CC_IN_A_N 12,
   HALT 반복 4+1 = HALT_STATES 5. 인터럽트 접수는 대기를 따로 붙이지
   않는다: CC_IRQ1 = 7+3+3 = 13, CC_IRQ2 = 7+3+3+3+3 = 19.
   R 은 M1 마다 오른다 (CPUCore incR): 접두 바이트도 M1 이고, DD CB d op 의
   변위와 마지막 op 는 M1 이 아니다. HALT 반복과 인터럽트 접수도 M1 이다. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let mem = Bytes.make 0x10000 '\000'

let machine ~m1_wait code =
  Bytes.fill mem 0 0x10000 '\000';
  String.iteri (fun i c -> Bytes.set mem (0x100 + i) c) code;
  let z =
    Z80.create ~m1_wait
      ~read:(fun ~at:_ a -> Char.code (Bytes.get mem (a land 0xffff)))
      ~write:(fun ~at:_ a v -> Bytes.set mem (a land 0xffff) (Char.chr (v land 0xff)))
      ~port_in:(fun ~at:_ _ -> 0xff)
      ~port_out:(fun ~at:_ _ _ -> ())
  in
  Z80.set_pc z 0x100;
  Z80.set_sp z 0xf000;
  z

(* The [at] each port callback saw while [code]'s first instruction ran,
   and that instruction's T-states. B is 1, so a block instruction ends. *)
let port_cycles ~m1_wait code =
  Bytes.fill mem 0 0x10000 '\000';
  String.iteri (fun i c -> Bytes.set mem (0x100 + i) c) code;
  let seen = ref [] in
  let z =
    Z80.create ~m1_wait
      ~read:(fun ~at:_ a -> Char.code (Bytes.get mem (a land 0xffff)))
      ~write:(fun ~at:_ a v -> Bytes.set mem (a land 0xffff) (Char.chr (v land 0xff)))
      ~port_in:(fun ~at _ -> seen := at :: !seen; 0xff)
      ~port_out:(fun ~at _ _ -> seen := at :: !seen)
  in
  Z80.set_pc z 0x100;
  Z80.set_bc z 0x0199;
  Z80.set_hl z 0xc000;
  let total = Z80.step z in
  (List.rev !seen, total)

(* Every bus access of [code]'s first instruction, in order: (kind, at)
   with kind 'R' memory read, 'W' memory write, 'I' port in, 'O' port out.
   The opcode and operand fetches are reads too. BC = 0x0199, DE = 0xd000,
   HL = IX = IY = 0xc000, SP = 0xf000, F = 0 (NZ). *)
let bus_trace ~m1_wait code =
  Bytes.fill mem 0 0x10000 '\000';
  String.iteri (fun i c -> Bytes.set mem (0x100 + i) c) code;
  let seen = ref [] in
  let note kind at = seen := (kind, at) :: !seen in
  let z =
    Z80.create ~m1_wait
      ~read:(fun ~at a -> note 'R' at; Char.code (Bytes.get mem (a land 0xffff)))
      ~write:(fun ~at a v ->
        note 'W' at;
        Bytes.set mem (a land 0xffff) (Char.chr (v land 0xff)))
      ~port_in:(fun ~at _ -> note 'I' at; 0xff)
      ~port_out:(fun ~at _ _ -> note 'O' at)
  in
  Z80.set_pc z 0x100;
  Z80.set_bc z 0x0199;
  Z80.set_de z 0xd000;
  Z80.set_hl z 0xc000;
  Z80.set_ix z 0xc000;
  Z80.set_iy z 0xc000;
  Z80.set_sp z 0xf000;
  Z80.set_af z 0;
  let total = Z80.step z in
  (List.rev !seen, total)

(* T-states of the first instruction of [code]. *)
let cycles ~m1_wait code = Z80.step (machine ~m1_wait code)

(* R after running [code]'s first instruction, read back with LD A,R
   (ED 5F: two more M1s before A takes R). R starts at 0. *)
let r_after code =
  let z = machine ~m1_wait:0 (code ^ "\xed\x5f") in
  ignore (Z80.step z);
  ignore (Z80.step z);
  Z80.dump_a z - 2

let () =
  let table =
    [ ("NOP", "\x00", 4, 5);
      ("LD A,n", "\x3e\x12", 7, 8);
      ("BIT 0,A", "\xcb\x47", 8, 10);
      ("LDI", "\xed\xa0", 16, 18);
      ("LD A,(IX+d)", "\xdd\x7e\x05", 19, 21);
      ("BIT 0,(IX+d)", "\xdd\xcb\x05\x46", 20, 22);
      ("IN A,(n)", "\xdb\x99", 11, 12);
      ("PUSH BC", "\xc5", 11, 12);
      ("CALL nn", "\xcd\x00\x20", 17, 18) ]
  in
  List.iter
    (fun (name, code, bare, msx) ->
      check (name ^ ": bare Z80") (cycles ~m1_wait:0 code = bare);
      check (name ^ ": MSX") (cycles ~m1_wait:1 code = msx))
    table;

  (* The port cycle inside each I/O instruction (openMSX Z80.hh, one wait
     per M1): IN A,(n) / OUT (n),A 5+3+1, IN r,(C) / OUT (C),r 5+5+1,
     INI 5+6+1, OUTI 5+6+3+1. Bare: one fewer per M1. *)
  let ports =
    [ ("IN A,(n)", [ "\xdb\x99" ], 8, 9);
      ("OUT (n),A", [ "\xd3\x99" ], 8, 9);
      ( "IN r,(C)",
        List.map (fun op -> "\xed" ^ String.make 1 (Char.chr op))
          [ 0x40; 0x48; 0x50; 0x58; 0x60; 0x68; 0x70; 0x78 ],
        9, 11 );
      ( "OUT (C),r",
        List.map (fun op -> "\xed" ^ String.make 1 (Char.chr op))
          [ 0x41; 0x49; 0x51; 0x59; 0x61; 0x69; 0x71; 0x79 ],
        9, 11 );
      ("INI/IND/INIR/INDR", [ "\xed\xa2"; "\xed\xaa"; "\xed\xb2"; "\xed\xba" ], 10, 12);
      ("OUTI/OUTD/OTIR/OTDR", [ "\xed\xa3"; "\xed\xab"; "\xed\xb3"; "\xed\xbb" ], 13, 15) ]
  in
  List.iter
    (fun (name, codes, bare, msx) ->
      List.iter
        (fun code ->
          let op = Printf.sprintf "%s %02x" name (Char.code code.[String.length code - 1]) in
          List.iter
            (fun (m1_wait, want, label) ->
              let seen, total = port_cycles ~m1_wait code in
              check (op ^ ": one port access, " ^ label) (List.length seen = 1);
              check (op ^ ": port cycle, " ^ label) (seen = [ want ]);
              check (op ^ ": inside the instruction, " ^ label)
                (List.for_all (fun at -> at < total) seen))
            [ (0, bare, "bare Z80"); (1, msx, "MSX") ])
        codes)
    ports;

  (* Memory accesses inside the instruction (openMSX Z80.hh, one wait per
     M1; CPUCore hands a device T + cc). Each access lands one cycle into
     its machine cycle. The opcode fetch is CC_MAIN = 1, a prefixed opcode
     CC_DD + 1 = 6. Data: CC_LD_A_SS_1 = 5+1, CC_LD_A_NN_2 = 5+3+3+1,
     CC_LD_HL_N_2 = 5+3+1, CC_INC_XHL_2 = 5+4+1, CC_PUSH_1 = 6+1,
     CALL CC_PUSH_1 + EE_CALL 6, RET cc CC_POP_1 + EE_RET_C 1,
     DJNZ CC_JR_1 + EE_DJNZ 1, CC_EX_SP_HL_2 = 5+3+4+1,
     CC_BIT_XHL_1 = 5+5+1, CC_SET_XHL_2 = 5+5+4+1, ED (nn) EE_ED 5,
     RETN EE_RETN 5, CC_RLD_2 = 5+5+3+4+1, CC_LDI_2 = 5+5+3+1,
     CC_INI_2 = 5+6+4+1, CC_OUTI_2 = 5+6+3+1, CC_DD + CC_LD_R_XIX_2
     (5+3+5+1), CC_DD + CC_LD_XIX_N_2, CC_DD + CC_INC_XHL_2 + EE_INC_XIX 8,
     CC_DD + CC_DD_CB (5+1) for d and the op, CC_DD + CC_BIT_XIX_1,
     CC_SET_XHL_2 + CC_DD + EE_SET_XIX 3. *)
  let accesses =
    [ ("LD A,(BC)", "\x0a", [ 'R', 1; 'R', 6 ]);
      ("LD (DE),A", "\x12", [ 'R', 1; 'W', 6 ]);
      ("LD A,(nn)", "\x3a\x00\xc0", [ 'R', 1; 'R', 6; 'R', 9; 'R', 12 ]);
      ("LD (nn),A", "\x32\x00\xc0", [ 'R', 1; 'R', 6; 'R', 9; 'W', 12 ]);
      ("LD HL,(nn)", "\x2a\x00\xc0", [ 'R', 1; 'R', 6; 'R', 9; 'R', 12; 'R', 15 ]);
      ("LD (nn),HL", "\x22\x00\xc0", [ 'R', 1; 'R', 6; 'R', 9; 'W', 12; 'W', 15 ]);
      ("LD B,(HL)", "\x46", [ 'R', 1; 'R', 6 ]);
      ("LD (HL),B", "\x70", [ 'R', 1; 'W', 6 ]);
      ("LD (HL),n", "\x36\x12", [ 'R', 1; 'R', 6; 'W', 9 ]);
      ("CP (HL)", "\xbe", [ 'R', 1; 'R', 6 ]);
      ("INC (HL)", "\x34", [ 'R', 1; 'R', 6; 'W', 10 ]);
      ("DEC (HL)", "\x35", [ 'R', 1; 'R', 6; 'W', 10 ]);
      ("PUSH BC", "\xc5", [ 'R', 1; 'W', 7; 'W', 10 ]);
      ("PUSH AF", "\xf5", [ 'R', 1; 'W', 7; 'W', 10 ]);
      ("POP BC", "\xc1", [ 'R', 1; 'R', 6; 'R', 9 ]);
      ("CALL nn", "\xcd\x00\x20", [ 'R', 1; 'R', 6; 'R', 9; 'W', 13; 'W', 16 ]);
      ("CALL NZ,nn", "\xc4\x00\x20", [ 'R', 1; 'R', 6; 'R', 9; 'W', 13; 'W', 16 ]);
      ("CALL Z,nn not taken", "\xcc\x00\x20", [ 'R', 1; 'R', 6; 'R', 9 ]);
      ("RET", "\xc9", [ 'R', 1; 'R', 6; 'R', 9 ]);
      ("RET NZ", "\xc0", [ 'R', 1; 'R', 7; 'R', 10 ]);
      ("RST 38h", "\xff", [ 'R', 1; 'W', 7; 'W', 10 ]);
      ("EX (SP),HL", "\xe3", [ 'R', 1; 'R', 6; 'R', 9; 'W', 13; 'W', 16 ]);
      ("DJNZ", "\x10\xfe", [ 'R', 1; 'R', 7 ]);
      ("JR", "\x18\xfe", [ 'R', 1; 'R', 6 ]);
      ("BIT 0,(HL)", "\xcb\x46", [ 'R', 1; 'R', 6; 'R', 11 ]);
      ("SET 0,(HL)", "\xcb\xc6", [ 'R', 1; 'R', 6; 'R', 11; 'W', 15 ]);
      ("RLC (HL)", "\xcb\x06", [ 'R', 1; 'R', 6; 'R', 11; 'W', 15 ]);
      ("LD (nn),BC", "\xed\x43\x00\xc0", [ 'R', 1; 'R', 6; 'R', 11; 'R', 14; 'W', 17; 'W', 20 ]);
      ("LD BC,(nn)", "\xed\x4b\x00\xc0", [ 'R', 1; 'R', 6; 'R', 11; 'R', 14; 'R', 17; 'R', 20 ]);
      ("RETN", "\xed\x45", [ 'R', 1; 'R', 6; 'R', 11; 'R', 14 ]);
      ("RLD", "\xed\x6f", [ 'R', 1; 'R', 6; 'R', 11; 'W', 18 ]);
      ("RRD", "\xed\x67", [ 'R', 1; 'R', 6; 'R', 11; 'W', 18 ]);
      ("LDI", "\xed\xa0", [ 'R', 1; 'R', 6; 'R', 11; 'W', 14 ]);
      ("LDDR", "\xed\xb8", [ 'R', 1; 'R', 6; 'R', 11; 'W', 14 ]);
      ("CPI", "\xed\xa1", [ 'R', 1; 'R', 6; 'R', 11 ]);
      ("INI", "\xed\xa2", [ 'R', 1; 'R', 6; 'I', 12; 'W', 16 ]);
      ("OUTI", "\xed\xa3", [ 'R', 1; 'R', 6; 'R', 12; 'O', 15 ]);
      ("IN A,(n)", "\xdb\x99", [ 'R', 1; 'R', 6; 'I', 9 ]);
      ("LD A,(IX+d)", "\xdd\x7e\x05", [ 'R', 1; 'R', 6; 'R', 11; 'R', 19 ]);
      ("ADD A,(IY+d)", "\xfd\x86\x05", [ 'R', 1; 'R', 6; 'R', 11; 'R', 19 ]);
      ("LD (IX+d),B", "\xdd\x70\x05", [ 'R', 1; 'R', 6; 'R', 11; 'W', 19 ]);
      ("LD (IX+d),n", "\xdd\x36\x05\x12", [ 'R', 1; 'R', 6; 'R', 11; 'R', 14; 'W', 19 ]);
      ("INC (IX+d)", "\xdd\x34\x05", [ 'R', 1; 'R', 6; 'R', 11; 'R', 19; 'W', 23 ]);
      ("BIT 0,(IX+d)", "\xdd\xcb\x05\x46", [ 'R', 1; 'R', 6; 'R', 11; 'R', 14; 'R', 19 ]);
      ("SET 0,(IX+d)", "\xdd\xcb\x05\xc6", [ 'R', 1; 'R', 6; 'R', 11; 'R', 14; 'R', 19; 'W', 23 ]);
      ("PUSH IX", "\xdd\xe5", [ 'R', 1; 'R', 6; 'W', 12; 'W', 15 ]);
      ("POP IX", "\xdd\xe1", [ 'R', 1; 'R', 6; 'R', 11; 'R', 14 ]);
      ("EX (SP),IX", "\xdd\xe3", [ 'R', 1; 'R', 6; 'R', 11; 'R', 14; 'W', 18; 'W', 21 ]);
      ("LD IX,(nn)", "\xdd\x2a\x00\xc0", [ 'R', 1; 'R', 6; 'R', 11; 'R', 14; 'R', 17; 'R', 20 ]) ]
  in
  List.iter
    (fun (name, code, want) ->
      let seen, total = bus_trace ~m1_wait:1 code in
      check (name ^ ": bus accesses, MSX") (seen = want);
      check (name ^ ": inside the instruction")
        (List.for_all (fun (_, at) -> at < total) seen))
    accesses;
  (* A bare Z80 has no M1 wait: LD A,(BC) reads at 5, LD A,(IX+d) at 17. *)
  check "LD A,(BC): bare Z80" (fst (bus_trace ~m1_wait:0 "\x0a") = [ 'R', 1; 'R', 5 ]);
  check "LD A,(IX+d): bare Z80"
    (fst (bus_trace ~m1_wait:0 "\xdd\x7e\x05") = [ 'R', 1; 'R', 5; 'R', 9; 'R', 17 ]);

  (* Interrupt acknowledge: the pushes write at 8 and 11 (CC_PUSH_1 +
     EE_IRQ1_1), and IM 2 then reads the vector at 14 and 17 (CC_IRQ2_2). *)
  let irq_trace code =
    Bytes.fill mem 0 0x10000 '\000';
    String.iteri (fun i c -> Bytes.set mem (0x100 + i) c) code;
    let seen = ref [] and armed = ref false in
    let note kind at = if !armed then seen := (kind, at) :: !seen in
    let z =
      Z80.create ~m1_wait:1
        ~read:(fun ~at a -> note 'R' at; Char.code (Bytes.get mem (a land 0xffff)))
        ~write:(fun ~at a v ->
          note 'W' at;
          Bytes.set mem (a land 0xffff) (Char.chr (v land 0xff)))
        ~port_in:(fun ~at:_ _ -> 0xff)
        ~port_out:(fun ~at:_ _ _ -> ())
    in
    Z80.set_pc z 0x100;
    Z80.set_sp z 0xf000;
    for _ = 1 to 3 do ignore (Z80.step z) done;
    armed := true;
    ignore (Z80.interrupt z);
    List.rev !seen
  in
  check "IM 1 acknowledge writes" (irq_trace "\xfb\xed\x56\x76" = [ 'W', 8; 'W', 11 ]);
  check "IM 2 acknowledge writes, then reads the vector"
    (irq_trace "\xfb\xed\x5e\x76" = [ 'W', 8; 'W', 11; 'R', 14; 'R', 17 ]);

  (* R counts M1 cycles *)
  check "R: NOP is one M1" (r_after "\x00" = 1);
  check "R: CB op is two" (r_after "\xcb\x47" = 2);
  check "R: ED op is two" (r_after "\xed\xa0" = 2);
  check "R: DD op is two" (r_after "\xdd\x7e\x05" = 2);
  check "R: DD CB d op is two" (r_after "\xdd\xcb\x05\x46" = 2);

  (* HALT repeats an M1: 5 T-states on an MSX, and R keeps counting *)
  let z = machine ~m1_wait:1 "\x76" in
  check "HALT itself" (Z80.step z = 5);
  check "HALT repetition" (Z80.step z = 5 && Z80.step z = 5);
  check "halted" (Z80.halted z);

  (* Interrupt acknowledge: 13 in IM 1, 19 in IM 2, 0 when refused; R
     counts it. EI; IM 1; HALT, then the INT line. *)
  let z = machine ~m1_wait:1 "\xfb\xed\x56\x76" in
  for _ = 1 to 3 do ignore (Z80.step z) done;
  let t0 = Z80.t_states z in
  check "IM 1 acknowledge is 13 T-states" (Z80.interrupt z = 13);
  check "t_states counts the acknowledge" (Z80.t_states z = t0 + 13);
  check "IM 1 jumps to 0x38" (Z80.dump_pc z = 0x38);
  check "a second INT with IFF1 off is refused" (Z80.interrupt z = 0);
  (* DI; LD A,R: R = 3 (DI, ED, 5F) without an acknowledge; one more with. *)
  let z = machine ~m1_wait:0 "\xfb\x00\xed\x5f" in
  ignore (Z80.step z);
  ignore (Z80.step z);
  ignore (Z80.interrupt z);
  Z80.set_pc z 0x102;
  ignore (Z80.step z);
  check "the acknowledge is an M1 for R" (Z80.dump_a z = 5);

  if !failures > 0 then exit 1;
  print_endline "z80_timing_test: all pass"
