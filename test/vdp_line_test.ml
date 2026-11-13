(* 라인 인터럽트(S#1 bit0 = FH)의 래치 계약 증명. 전부 Z80 이 보는 I/O 포트
   경로(0x99 레지스터 쓰기 → 0x99 상태 읽기)로 판정한다. 기대값의 정본:
   V9938 매뉴얼 — S#1 bit0 는 R#19 로 지정한 주사줄 인터럽트 플래그(FH) 이고
   "If IE1 is set, an interrupt is enabled", S#1 읽기가 FH 를 지운다. 그리고
   openMSX VDP.cc — execHScan() 은 controlRegs[0] & 0x10(IE1) 일 때만
   irqHorizontal 를 세우고, R#0 쓰기가 IE1 을 끄면 reset() 한다. IE1 없이 FH
   가 래치되면 IE1=0 으로 사는 게임(룬마스터 II 의 갤러리 핸들러)이 매 프레임
   S#1 읽기에서 1 을 보고, 5109 프레임 클록 증가 경로를 건너뛰어 멈춘다.
   ROM 없이 돈다. 끝의 두 블록은 포트 읽기가 명령 안의 포트 사이클에
   VDP 에 닿는지(#76)를 RAM 에 둔 프로그램으로 확인한다. *)

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
  (* 인터럽트 발생 시 상태 레지스터 선택(R#15)은 0 으로 돌아간다 — V9938
     계약. 게임이 S#1 선택을 남긴 채 VBlank 가 걸리면 BIOS KEYINT 의
     `in a,(0x99)` 가 S#1 을 읽고 스캔 없이 종료한다(룬마스터 1 관측). *)
  set_reg t 15 1;
  Msx.step t ~frames:200;
  check "interrupt resets S# select to 0"
    ((Msx.vdp_regs t).(15) land 0x0f = 0);
  (* A port access reaches the VDP at its port cycle, not at the start of
     the instruction (issue #76; openMSX CPUCore READ_PORT schedules up to
     getTimeFast(CC_IN_A_N_2), 5+3+1 = 9 cycles in). A program in RAM starts
     IN A,(0x99) [lead] cycles before S#0 F rises and stores what it read:
     9 cycles before, the read lands on F; 10 before, one cycle short. A
     loop of IN A,(0x2E) before it checks that every port access hands the
     VDP the rest of its instruction afterwards: a lost or doubled cycle
     would move the read. *)
  let f_at =
    let v = Vdp.create () and n = ref 0 in
    while Vdp.status0 v land 0x80 = 0 do
      ignore (Vdp.advance v ~cycles:1);
      incr n
    done;
    !n
  in
  (* Code placed at [at] and entered at cycle [now] that starts
     IN A,(0x99) at cycle [target], stores it at 0xC000 and stays in a
     JP to itself (a HALT would wake on F with interrupts off). LD BC,n
     (11); n loops of DEC BC (7), IN A,(0x2E) (12), LD A,B (5), OR C (5),
     JP NZ (11): 40n; then NOP (5) and LD A,0 (8) for the rest. *)
  let read_at ~at ~now ~target =
    let n = (target - now - 11 - 28) / 40 in
    let fill = target - now - 11 - (40 * n) in
    let b = List.find (fun b -> (fill - (8 * b)) mod 5 = 0) [ 0; 1; 2; 3; 4 ] in
    let a = (fill - (8 * b)) / 5 in
    let loop = at + 3 in
    let head =
      [ 0x01; n land 0xff; n lsr 8;
        0x0b; 0xdb; 0x2e; 0x78; 0xb1; 0xc2; loop land 0xff; loop lsr 8 ]
      @ List.init a (fun _ -> 0x00)
      @ List.concat (List.init b (fun _ -> [ 0x3e; 0x00 ]))
    in
    let stop = at + List.length head + 5 in
    (head @ [ 0xdb; 0x99; 0x32; 0x00; 0xc0; 0xc3; stop land 0xff; stop lsr 8 ], stop)
  in
  let load t ~at code = List.iteri (fun i b -> Msx.mem_write t (at + i) b) code in
  (* Every page on the RAM: slot 3-2, mapper segments 3-0 as the BIOS
     leaves them. *)
  let ram_machine () =
    let t = machine () in
    Msx.port_out t 0xa8 0xff;
    Msx.mem_write t 0xffff 0xaa;
    List.iteri (fun i seg -> Msx.port_out t (0xfc + i) seg) [ 3; 2; 1; 0 ];
    Msx.mem_write t 0xc000 0x55;
    t
  in
  let result (t, stop) ~frames name =
    Msx.step t ~frames;
    check (name ^ ": the program ran") (Msx.dump_pc t = stop && Msx.mem_read t 0xc000 <> 0x55);
    Msx.mem_read t 0xc000 land 0x80 <> 0
  in
  (* 0x0000 JP 0x0200 (11); DI (5); then the read. *)
  let read_with ~lead =
    let t = ram_machine () in
    load t ~at:0 [ 0xc3; 0x00; 0x02 ];
    let code, stop = read_at ~at:0x201 ~now:16 ~target:(f_at - lead) in
    load t ~at:0x200 (0xf3 :: code);
    result (t, stop) ~frames:1 (Printf.sprintf "port timing (lead %d)" lead)
  in
  check "IN A,(0x99) 9 cycles before F reads F" (read_with ~lead:9);
  check "IN A,(0x99) 10 cycles before F misses F" (not (read_with ~lead:10));

  (* The interrupt acknowledge (IM 1, 13 cycles) runs the VDP before the
     instruction, and the frame budget counts it. IM 1 (10), EI (5),
     JP 0x0100 (11), then NOPs: the VBlank interrupt comes at the first
     NOP boundary at or after F. The handler at 0x0038 clears F and reads
     S#0 again [lead] cycles before the next F. *)
  let f_next =
    let v = Vdp.create () and n = ref 0 in
    let seen = ref 0 in
    while !seen < 2 do
      ignore (Vdp.advance v ~cycles:1);
      incr n;
      if Vdp.status0 v land 0x80 <> 0 then begin
        incr seen;
        ignore (Vdp.io_read v ~port:0x99)
      end
    done;
    !n
  in
  let after_irq ~lead =
    let t = ram_machine () in
    set_reg t 1 0x20;
    load t ~at:0 [ 0xed; 0x56; 0xfb; 0xc3; 0x00; 0x01 ];
    load t ~at:0x100 (List.init 9000 (fun _ -> 0x00) @ [ 0xc3; 0x28; 0x24 ]);
    let ack = 26 + (5 * ((f_at - 26 + 4) / 5)) in
    let code, stop = read_at ~at:0x3a ~now:(ack + 13 + 12) ~target:(f_next - lead) in
    load t ~at:0x38 (0xdb :: 0x99 :: code);
    (t, stop)
  in
  let (t, _) as run = after_irq ~lead:9 in
  Msx.step t ~frames:1;
  (* The CPU is in the handler's loop (12 cycles at most an instruction):
     the VDP ran one frame and less than one more instruction. *)
  let ran =
    let v = Vdp.create () in
    ignore (Vdp.advance v ~cycles:59736);
    let rec find k =
      if k > 11 then None
      else if Vdp.line_now v = Msx.vdp_line t then Some k
      else begin
        ignore (Vdp.advance v ~cycles:1);
        find (k + 1)
      end
    in
    find 0
  in
  check "a frame with an interrupt runs the VDP one frame" (ran <> None);
  check "after an interrupt, IN A,(0x99) 9 cycles before F reads F"
    (result run ~frames:1 "after an interrupt (lead 9)");
  check "after an interrupt, IN A,(0x99) 10 cycles before F misses F"
    (not (result (after_irq ~lead:10) ~frames:2 "after an interrupt (lead 10)"));
  (if !failures = 0 then Printf.printf "vdp_line_test: all passed\n"
   else begin
     Printf.printf "vdp_line_test: %d failure(s)\n" !failures;
     exit 1
   end)
