(* 라인 인터럽트(S#1 bit0 = FH)의 래치 계약 증명. 전부 Z80 이 보는 I/O 포트
   경로(0x99 레지스터 쓰기 → 0x99 상태 읽기)로 판정한다. 기대값의 정본:
   V9938 매뉴얼 — S#1 bit0 는 R#19 로 지정한 주사줄 인터럽트 플래그(FH) 이고
   "If IE1 is set, an interrupt is enabled", S#1 읽기가 FH 를 지운다. 그리고
   openMSX VDP.cc — execHScan() 은 controlRegs[0] & 0x10(IE1) 일 때만
   irqHorizontal 를 세우고, R#0 쓰기가 IE1 을 끄면 reset() 한다. IE1 없이 FH
   가 래치되면 IE1=0 으로 사는 게임(룬마스터 II 의 갤러리 핸들러)이 매 프레임
   S#1 읽기에서 1 을 보고, 5109 프레임 클록 증가 경로를 건너뛰어 멈춘다.
   ROM 없이 돈다. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let machine () =
  Msx.create ~machine:{ Msx.ram_kb = 64; vram_kb = 128; roms = [ ""; ""; "" ] }

(* 0x99 두 바이트 쌍(값, 0x80|번호) — 레지스터 쓰기 프로토콜. *)
let set_reg t r v =
  Msx.port_out t 0x99 v;
  Msx.port_out t 0x99 (0x80 lor r)

(* R#15 로 상태 레지스터를 골라 읽는다. S#1 읽기는 FH 를 지운다. *)
let read_status t s =
  set_reg t 15 s;
  Msx.port_in t 0x99

let () =
  let t = machine () in
  (* 기본 배치(192 표시줄)에서 R#19=100 — 한 프레임이면 반드시 지나가는 비교줄. *)
  set_reg t 19 100;
  Msx.step t ~frames:1;
  check "IE1 off: FH not latched" (read_status t 1 land 1 = 0);
  Msx.step t ~frames:1;
  check "IE1 off: FH stays clear" (read_status t 1 land 1 = 0);
  (* IE1(R#0 bit4) 을 켜면 비교줄 통과 뒤 FH=1, S#1 읽기가 지운다. *)
  set_reg t 0 0x10;
  Msx.step t ~frames:1;
  check "IE1 on: FH latched at compare line" (read_status t 1 land 1 = 1);
  check "S#1 read clears FH" (read_status t 1 land 1 = 0);
  (* 다시 래치시킨 뒤 IE1 을 끊는 R#0 쓰기 — 서 있던 FH 도 지운다. *)
  Msx.step t ~frames:1;
  set_reg t 0 0x00;
  check "IE1 clear resets latched FH" (read_status t 1 land 1 = 0);
  Msx.step t ~frames:1;
  check "IE1 off after reset: FH not latched" (read_status t 1 land 1 = 0);
  (if !failures = 0 then Printf.printf "vdp_line_test: all passed\n"
   else begin
     Printf.printf "vdp_line_test: %d failure(s)\n" !failures;
     exit 1
   end)
