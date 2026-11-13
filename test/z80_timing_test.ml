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

  (* HALT ends with an accepted interrupt only. DI; HALT, then the INT line:
     refused, still halted, PC still past the HALT. *)
  let z = machine ~m1_wait:1 "\xf3\x76" in
  ignore (Z80.step z);
  ignore (Z80.step z);
  check "DI; HALT: an INT is refused" (Z80.interrupt z = 0);
  check "DI; HALT: still halted" (Z80.halted z);
  check "DI; HALT: PC stays past the HALT" (Z80.dump_pc z = 0x102);
  check "DI; HALT: the HALT repeats" (Z80.step z = 5 && Z80.halted z);
  (* EI; HALT: accepted, the HALT ends and the return address is past it. *)
  let z = machine ~m1_wait:1 "\xfb\x76" in
  ignore (Z80.step z);
  ignore (Z80.step z);
  let sp0 = Z80.dump_sp z in
  check "EI; HALT: an INT is accepted" (Z80.interrupt z = 13);
  check "EI; HALT: no longer halted" (not (Z80.halted z));
  check "EI; HALT: returns past the HALT"
    (Z80.dump_sp z = sp0 - 2
     && Char.code (Bytes.get mem (sp0 - 2)) = 0x02
     && Char.code (Bytes.get mem (sp0 - 1)) = 0x01);
  (* EI then the INT line at once: refused (the instruction after EI runs
     first), and a CPU that is not halted is not made to halt. *)
  let z = machine ~m1_wait:1 "\xfb\x00" in
  ignore (Z80.step z);
  check "right after EI: an INT is refused" (Z80.interrupt z = 0);
  check "right after EI: not halted" (not (Z80.halted z));

  if !failures > 0 then exit 1;
  print_endline "z80_timing_test: all pass"
