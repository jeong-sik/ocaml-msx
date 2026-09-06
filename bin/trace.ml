(* 차등 테스팅용 트레이서 — C 드라이버(test/zex/diff_driver.c) 와 같은
   포맷으로 매 스텝 상태를 찍는다. diff 로 첫 분기 스텝을 찾는다. *)

let mem = Bytes.make 0x10000 '\000'

let mem_read a = Char.code (Bytes.get mem (a land 0xffff))
let mem_write a v = Bytes.set mem (a land 0xffff) (Char.chr (v land 0xff))

let () =
  let path = if Array.length Sys.argv > 1 then Sys.argv.(1) else "test/zex/zexdoc.cim" in
  let max_steps =
    if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 6000
  in
  let skip =
    if Array.length Sys.argv > 3 then int_of_string Sys.argv.(3) else 0
  in
  Bytes.fill mem 0 0x10000 '\000';
  Bytes.set mem 0x00 '\xd3';
  Bytes.set mem 0x01 '\x00';
  Bytes.set mem 0x05 '\xdb';
  Bytes.set mem 0x06 '\x00';
  Bytes.set mem 0x07 '\xc9';
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.iteri (fun i c -> Bytes.set mem (0x100 + i) c) s;
  let z =
    Z80.create ~read:mem_read ~write:mem_write
      ~port_in:(fun _ -> 0xff)
      ~port_out:(fun _ _ -> ())
  in
  Z80.set_pc z 0x0100;
  Z80.set_af z 0xffff;
  for n = 0 to max_steps - 1 do
    if n < skip || (skip = 0 && n mod 1024 <> 0) then ignore (Z80.step z)
    else begin
    let mchk = ref (2166136261 land 0xffffffff) in
    for i = 0x100 to 0xffff do
      mchk := (!mchk lxor Char.code (Bytes.get mem i)) land 0xffffffff;
      mchk := (!mchk * 16777619) land 0xffffffff
    done;
    Printf.printf "m %d %08x\n" n !mchk;
    Printf.printf "%d op=%02x pc=%04x af=%04x bc=%04x de=%04x hl=%04x ix=%04x iy=%04x sp=%04x\n%!"
      n (mem_read (Z80.dump_pc z)) (Z80.dump_pc z)
      (((Z80.dump_a z) lsl 8) lor Z80.dump_f z)
      (Z80.dump_bc z) (Z80.dump_de z) (Z80.dump_hl z)
      (Z80.dump_ix z) (Z80.dump_iy z) (Z80.dump_sp z);
    ignore (Z80.step z)
    end
  done
