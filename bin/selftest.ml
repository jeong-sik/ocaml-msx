(* 최소 자기검증: BDOS 9번(문자열 출력) → JP 0(종료) 을 손으로 심어
   코어의 기본 경로(LD/_CALL/IN/JP)와 하네스의 BDOS 가로채기가 맞는지
   본다. 이게 깨지면 zexdoc 실패의 원인도 이 안에 있다. *)

let mem = Bytes.make 0x10000 '\000'
let out_buf = Buffer.create 256
let finished = ref false
let cpu : Z80.t option ref = ref None

let mem_read addr = Char.code (Bytes.get mem (addr land 0xffff))
let mem_write addr v = Bytes.set mem (addr land 0xffff) (Char.chr (v land 0xff))

let bdos () =
  match !cpu with
  | None -> ()
  | Some z ->
    let fn = Z80.dump_bc z land 0xff in
    if fn = 2 then Buffer.add_char out_buf (Char.chr ((Z80.dump_de z) land 0xff))
    else if fn = 9 then begin
      let rec walk a =
        let ch = mem_read a in
        if ch <> Char.code '$' then begin
          Buffer.add_char out_buf (Char.chr ch);
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
         ~port_out:(fun _ _ -> finished := true));
  let z = match !cpu with Some z -> z | None -> assert false in
  (* 종료 신호 *)
  Bytes.set mem 0x00 '\xd3';
  Bytes.set mem 0x01 '\x00';
  (* BDOS 진입: in a,(0) ; ret *)
  Bytes.set mem 0x05 '\xdb';
  Bytes.set mem 0x06 '\x00';
  Bytes.set mem 0x07 '\xc9';
  (* 0x100: LD C,9 / LD DE,0x120 / CALL 0005 / JP 0000 *)
  let prog = [ 0x0e; 0x09; 0x11; 0x20; 0x01; 0xcd; 0x05; 0x00; 0xc3; 0x00; 0x00 ] in
  List.iteri (fun i b -> Bytes.set mem (0x100 + i) (Char.chr b)) prog;
  (* 0x120: "hi$" *)
  Bytes.set mem 0x120 'h';
  Bytes.set mem 0x121 'i';
  Bytes.set mem 0x122 '$';
  Z80.set_pc z 0x0100;
  let steps = ref 0 in
  while not !finished && !steps < 200 do
    if !steps < 24 then Printf.printf "step %d pc=%04x op=%02x\n" !steps (Z80.dump_pc z) (mem_read (Z80.dump_pc z));
    ignore (Z80.step z);
    incr steps
  done;
  Printf.printf "steps=%d finished=%b output=%S pc=%04x a=%02x\n%!"
    !steps !finished (Buffer.contents out_buf) (Z80.dump_pc z) (Z80.dump_a z);
  (* 검증 2: LD HL,nn / LD A,(HL) / INC A — A=0xAB 여야 한다. *)
  Bytes.fill mem 0 0x10000 '\000';
  Bytes.set mem 0x00 '\xd3';
  Bytes.set mem 0x01 '\000';
  Bytes.set mem 0x1234 '\170';
  finished := false;
  let prog2 = [ 0x21; 0x34; 0x12; 0x7e; 0x3c; 0xc3; 0x00; 0x00 ] in
  List.iteri (fun i b -> Bytes.set mem (0x100 + i) (Char.chr b)) prog2;
  let cpu2 =
    Z80.create ~read:mem_read ~write:mem_write
      ~port_in:(fun _ -> 0xff)
      ~port_out:(fun _ _ -> finished := true)
  in
  Z80.set_pc cpu2 0x0100;
  let n2 = ref 0 in
  while not !finished && !n2 < 50 do ignore (Z80.step cpu2); incr n2 done;
  Printf.printf "prog2: steps=%d finished=%b A=%02x (want AB) HL=%04x\n%!"
    !n2 !finished (Z80.dump_a cpu2) (Z80.dump_hl cpu2);
  exit
    (if !finished && Buffer.contents out_buf = "hi" && Z80.dump_a cpu2 = 0xAB
     then 0
     else 1)
