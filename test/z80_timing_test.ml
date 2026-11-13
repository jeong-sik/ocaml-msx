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
      ~read:(fun a -> Char.code (Bytes.get mem (a land 0xffff)))
      ~write:(fun a v -> Bytes.set mem (a land 0xffff) (Char.chr (v land 0xff)))
      ~port_in:(fun _ -> 0xff)
      ~port_out:(fun _ _ -> ())
  in
  Z80.set_pc z 0x100;
  Z80.set_sp z 0xf000;
  z

(* T-states of the first instruction of [code]. *)
let cycles ~m1_wait code = Z80.step (machine ~m1_wait code)

(* T-states of [code]'s first instruction with F and B set (C is 1). *)
let cycles_in ~m1_wait ~f ~b code =
  let z = machine ~m1_wait code in
  Z80.set_af z f;
  Z80.set_bc z ((b lsl 8) lor 1);
  Z80.step z

(* Opcodes a DD/FD prefix changes: they name HL, H, L or (HL) (z80.info
   decoding). Every other opcode runs as if unprefixed. *)
let uses_hl op =
  let x = op lsr 6 and y = (op lsr 3) land 7 and w = op land 7 in
  let hl r = r = 4 || r = 5 || r = 6 in
  match x with
  | 0 ->
    List.mem op
      [ 0x09; 0x19; 0x29; 0x39; 0x21; 0x22; 0x2a; 0x23; 0x2b;
        0x24; 0x25; 0x26; 0x2c; 0x2d; 0x2e; 0x34; 0x35; 0x36 ]
  | 1 -> hl y || hl w
  | 2 -> hl w
  | _ -> List.mem op [ 0xe1; 0xe3; 0xe5; 0xe9; 0xf9 ]

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
      ("CALL nn", "\xcd\x00\x20", 17, 18);
      (* Zilog: JR cc,e is 12 (4,3,5) taken, 7 (4,3) not; openMSX
         CC_JR_A = 5+3+5, CC_JR_B = 5+3, CC_DJNZ = 6+3+5. F and B are 0. *)
      ("JR NZ,e taken", "\x20\x05", 12, 13);
      ("JR Z,e not taken", "\x28\x05", 7, 8);
      ("JR C,e not taken", "\x38\x05", 7, 8);
      ("DJNZ taken", "\x10\x05", 13, 14);
      (* A DD/FD prefix is an M1 of its own, 4 T-states (openMSX CC_DD
         = 5 with the wait): 4 + the unprefixed opcode, or the IX
         instruction's full time. *)
      ("DD NOP", "\xdd\x00", 8, 10);
      ("INC IXH", "\xdd\x24", 8, 10);
      ("LD IXL,n", "\xdd\x2e\x12", 11, 13);
      ("LD IX,nn", "\xdd\x21\x00\x20", 14, 16);
      ("ADD IX,BC", "\xdd\x09", 15, 17);
      ("INC (IX+d)", "\xdd\x34\x05", 23, 25);
      ("LD (IX+d),n", "\xdd\x36\x05\x12", 19, 21);
      ("ADD A,(IY+d)", "\xfd\x86\x05", 19, 21);
      ("PUSH IX", "\xdd\xe5", 15, 17);
      ("POP IY", "\xfd\xe1", 14, 16);
      ("EX (SP),IX", "\xdd\xe3", 23, 25);
      ("JP (IX)", "\xdd\xe9", 8, 10);
      ("LD SP,IY", "\xfd\xf9", 10, 12);
      ("RLC (IX+d)", "\xdd\xcb\x05\x06", 23, 25) ]
  in
  List.iter
    (fun (name, code, bare, msx) ->
      check (name ^ ": bare Z80") (cycles ~m1_wait:0 code = bare);
      check (name ^ ": MSX") (cycles ~m1_wait:1 code = msx))
    table;

  (* Every opcode a DD/FD prefix leaves alone costs the prefix's M1 more:
     4 on a bare Z80, 5 on an MSX. Conditional ones under both flag
     states, DJNZ both ways. *)
  for op = 0 to 255 do
    if not (List.mem op [ 0xcb; 0xdd; 0xed; 0xfd; 0x76 ] || uses_hl op) then begin
      let code = String.make 1 (Char.chr op) ^ "\x05\x00\x00" in
      List.iter
        (fun (f, b) ->
          List.iter
            (fun (prefix, m1_wait, extra) ->
              let plain = cycles_in ~m1_wait ~f ~b code in
              let prefixed = cycles_in ~m1_wait ~f ~b (prefix ^ code) in
              check
                (Printf.sprintf "%02x %02x (F=%02x B=%d, wait %d): prefix costs %d"
                   (Char.code prefix.[0]) op f b m1_wait extra)
                (prefixed = plain + extra))
            [ ("\xdd", 0, 4); ("\xfd", 0, 4); ("\xdd", 1, 5); ("\xfd", 1, 5) ])
        [ (0x00, 1); (0xff, 1); (0x00, 2); (0xff, 2) ]
    end
  done;

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
