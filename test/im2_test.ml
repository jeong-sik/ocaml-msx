(* IM2 인터럽트 계약 — 벡터 테이블 조회. 실기 Z80 은 I<<8|데이터버스
   주소에서 word 를 읽어 그곳으로 call 한다. 곧장 점프하면 게임이
   워크에이어에 심는 벡터 테이블(룬마스터 1 의 디스크 인터럽트)을
   읽지 않는다. *)

let ram = Bytes.make 0x10000 '\000'
let read a = Char.code (Bytes.get ram a)
let write a v = Bytes.set ram a (Char.chr (v land 0xff))
let pin _ = 0xff
let pout _ _ = ()

let poke a bs = String.iteri (fun i c -> Bytes.set ram (a + i) c) bs

let check name cond =
  if not cond then begin
    prerr_endline ("im2: FAIL " ^ name);
    exit 1
  end

let () =
  (* 벡터 테이블: I=0xC7, 데이터 버스 0xFF → 0xC7FF 의 word = 0x1234. *)
  poke 0xc7ff "\x34\x12";
  (* 핸들러 0x1234: ret (0xC9). *)
  poke 0x1234 "\xc9";
  (* 0x0100: ld a,0xC7 / ld i,a / im 2 / ei / halt. *)
  poke 0x0100 "\x3e\xc7\xed\x47\xed\x5e\xfb\x76";
  let z = Z80.create ~read ~write ~port_in:pin ~port_out:pout in
  Z80.set_pc z 0x0100;
  (* 설정 코드 5명령 + halt. *)
  for _ = 1 to 6 do ignore (Z80.step z) done;
  check "halted at ei+halt" (Z80.dump_pc z = 0x0107 || Z80.dump_pc z = 0x0108);
  let sp0 = Z80.dump_sp z in
  check "interrupt accepted" (Z80.interrupt z);
  check "vector read from table" (Z80.dump_pc z = 0x1234);
  check "return address pushed" (Z80.dump_sp z = sp0 - 2);
  (* 핸들러 ret 는 중단된 위치로 돌린다. *)
  ignore (Z80.step z);
  check "handler ret restores pc" (Z80.dump_pc z = 0x0108 || Z80.dump_pc z = 0x0107);
  print_endline "im2: all passed"
