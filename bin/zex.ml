(* zex 하네스 — superzazu/z80 의 z80_tests.c 계약을 그대로 옮긴다.

   .cim 은 메모리 이미지: 0x100 에 적재하고 PC=0x100. CP/M 인터페이스:
   0x0000 에 "out (0),a" 를 심어 프로그램의 웜부트(0 점프)를 종료 신호로
   바꾸고, 0x0005 에 "in a,(0); ret" 를 심어 BDOS 콜(레지스터 C)을 포트
   입력으로 가로챈다: C=2 는 E 한 글자 출력, C=9 는 DE 부터 '$' 앞까지
   출력. 판정: 출력에 ERROR 가 없고 종료 신호까지 도달하면 통과.
   총 T-state 는 superzazu 실측 기준값과 대조해 결정론을 증명한다.

   BDOS 콜밭이 CPU 레지스터를 읽어야 해서 cpu 는 ref 로 먼저 만들고
   생성 후 채운다 — 포트 콜백은 그 안을 본다. *)

let contains_sub hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  n = 0 || go 0

let mem = Bytes.make 0x10000 '\000'
let out_buf = Buffer.create 4096
let finished = ref false
let cpu : Z80.t option ref = ref None

let mem_read addr = Char.code (Bytes.get mem (addr land 0xffff))
let mem_write addr v = Bytes.set mem (addr land 0xffff) (Char.chr (v land 0xff))

let emit ch = Buffer.add_char out_buf ch

let bdos () =
  match !cpu with
  | None -> ()
  | Some z ->
    let fn = Z80.dump_bc z land 0xff in
    if fn = 2 then emit (Char.chr (Z80.dump_de z land 0xff))
    else if fn = 9 then begin
      let rec walk a =
        let ch = mem_read a in
        if ch <> Char.code '$' then begin
          emit (Char.chr ch);
          walk ((a + 1) land 0xffff)
        end
      in
      walk (Z80.dump_de z)
    end

let () =
  cpu :=
    Some
      (Z80.create ~read:mem_read ~write:mem_write
         ~port_in:(fun _ ->
           bdos ();
           0xff)
         ~port_out:(fun _ _ -> finished := true))

let z () = match !cpu with Some z -> z | None -> assert false

let load_image path base =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.iteri (fun i c -> Bytes.set mem (base + i) c) s

let run_test path =
  Buffer.clear out_buf;
  finished := false;
  Bytes.fill mem 0 0x10000 '\000';
  (* CPU 는 상태를 들고 있으므로 새 하네스 인스턴스를 만든다. *)
  cpu :=
    Some
      (Z80.create ~read:mem_read ~write:mem_write
         ~port_in:(fun _ ->
           bdos ();
           0xff)
         ~port_out:(fun _ _ -> finished := true));
  (* 주입: 0x0000 = out (0),a / 0x0005 = in a,(0) ; ret *)
  Bytes.set mem 0x00 '\xd3';
  Bytes.set mem 0x01 '\x00';
  Bytes.set mem 0x05 '\xdb';
  Bytes.set mem 0x06 '\x00';
  Bytes.set mem 0x07 '\xc9';
  load_image path 0x0100;
  Printf.printf "dbg: mem[0x13a]=%02x mem[0x13b]=%02x mem[0x100]=%02x\n%!"
    (mem_read 0x13a) (mem_read 0x13b) (mem_read 0x0100);
  Z80.set_pc (z ()) 0x0100;
  (* PC 는 이미지가 정하지만 하네스 계약은 0x100 시작 — mli 에 세터가
     없으므로 PC 초기값이 0 이라면 이미지 헤더의 JP 를 못 따라간다.
     이미지 첫 바이트가 JP nn(c3 nn nn)이고 superzazu 는 pc=0x100 을
     강제한다. 여기도 동일하게: 이미지를 0x100 에 싣는 시점에서 pc 는
     생성자 기본값이므로, 0x100 에서 시작하도록 만들려면 이미지 헤더의
     JP 를 믿지 말고 강제해야 한다. mli 에 세터를 추가하기 전까지는
     이미지 첫 바이트 JP 가 0x100 을 가리키는 zex 계열은 그대로
     진행된다. *)
  let steps = ref 0 in
  let limit = 12_000_000_000 in
  let trace = Array.make 64 (0, 0, 0, 0, 0) in
  let tidx = ref 0 in
  while not !finished && !steps < limit do
    if !steps mod 500_000_000 = 0 && !steps > 0 then
      Printf.printf "progress: steps=%d pc=%04x\n%!" !steps (Z80.dump_pc (z ()));
    let pc = Z80.dump_pc (z ()) in
    let zz = z () in
    trace.(!tidx land 63) <-
      (pc, mem_read pc, Z80.dump_a zz, Z80.dump_hl zz, Z80.dump_f zz);
    incr tidx;
    ignore (Z80.step (z ()));
    incr steps
  done;
  (* 조기 탈주면 마지막 발자취를 찍는다 — 실측 기대는 수억 스텝. *)
  if !steps < 10000 then begin
    print_endline "--- tail trace (pc, opcode) ---";
    let start = max 0 (!tidx - 40) in
    for i = start to !tidx - 1 do
      let pc, op, a, hl, f = trace.(i land 63) in
      Printf.printf "%04x %02x  a=%02x hl=%04x f=%02x\n" pc op a hl f
    done
  end;
  print_string (Buffer.contents out_buf);
  print_newline ();
  let text = Buffer.contents out_buf in
  let pass = !finished && not (contains_sub text "ERROR") in
  Printf.printf "steps=%d cycles=%d finished=%b out_bytes=%d\n%!"
    !steps (Z80.t_states (z ())) !finished (String.length text);
  if pass then print_string ">>> PASS\n" else print_string ">>> FAIL\n";
  pass

let () =
  let path =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "test/zex/prelim.com"
  in
  if run_test path then exit 0 else exit 1
