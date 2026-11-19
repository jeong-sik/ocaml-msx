(* A program that owns page 0 and runs its own code across a BIOS vector
   address is not calling the BIOS.

   [Msx.disk_trap] serves the fixed page-0 vectors (CHSNS 0x009C ... KILBUF
   0x0156) with a plain CALL while page 0 is RAM, for disk kernels that
   assume the BIOS is still there (chsns_hle_call_test). Europa Sensen's
   bootstrap, after `OUT (A8),FF`, copies its own vector table and loader into
   page 0 and executes LD BC,0080 at 0x0156 as one instruction of a straight
   run. The trap took that for a KILBUF call, switched page 0 to the BIOS and
   returned through garbage; the machine ended in a RST 38h storm.

   Both sides are pinned on a fresh machine each: bytes at the vector that
   are still a power-on fill keep the trap (a CALL reaches main_rom), bytes a
   program planted there run as the program's code. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

(* main_rom: KILBUF's vector slot is a JP to a stub that stores 0x42 at 0xC100. *)
let build_main_rom () =
  let main = Bytes.make 0x4000 '\000' in
  Bytes.set main 0x156 '\xc3'; Bytes.set main 0x157 '\x20'; Bytes.set main 0x158 '\x00';
  List.iteri (fun i b -> Bytes.set main (0x20 + i) (Char.chr b))
    [ 0x3e; 0x42; 0x32; 0x00; 0xc1; 0xc9 ];
  main

(* The DOS-kernel-replay shape of chsns_hle_call_test: disk mounted, pages 0
   and 3 on slot 3, slot 3's sub-slot select on the RAM mapper. *)
let machine () =
  let t =
    Msx.create
      ~machine:{ Msx.ram_kb = 64; vram_kb = 128;
                 roms = [ Bytes.to_string (build_main_rom ()) ] }
  in
  Msx.load_disk ~interface_rom:false t "\000";
  Msx.port_out t 0xa8 0xc3;
  Msx.mem_write t 0xffff 0xaa;
  t

let plant t addr bytes = List.iteri (fun i b -> Msx.mem_write t (addr + i) b) bytes

let () =
  (* A call to the untouched vector still reaches the BIOS. *)
  let t = machine () in
  plant t 0 [ 0x31; 0xf0; 0xff; 0xcd; 0x56; 0x01; 0x76 ];
  Msx.step t ~frames:1;
  check "a CALL to an untouched 0x0156 reaches main_rom's stub" (Msx.mem_read t 0xc100 = 0x42);
  check "the call returned to HALT" (Msx.cpu_halted t);
  check "page 0 is RAM again after the call" (Msx.ppi_a t land 3 = 3);
  (* Straight-line code through 0x0156 stays the program's. *)
  let t = machine () in
  plant t 0 [ 0x31; 0xf0; 0xff; 0xc3; 0x50; 0x01 ];
  plant t 0x150
    [ 0x3e; 0x5a;              (* 0150 LD A,5A *)
      0x32; 0x00; 0xc1;        (* 0152 LD (C100),A *)
      0x00;                    (* 0155 NOP *)
      0x3e; 0x77;              (* 0156 LD A,77   <- the vector address *)
      0x32; 0x01; 0xc1;        (* 0158 LD (C101),A *)
      0x76 ];                  (* 015B HALT *)
  Msx.step t ~frames:1;
  check "the program's code ran up to 0x0156" (Msx.mem_read t 0xc100 = 0x5a);
  check "the instruction at 0x0156 ran as the program's" (Msx.mem_read t 0xc101 = 0x77);
  check "main_rom's stub did not run" (Msx.mem_read t 0xc100 <> 0x42);
  check "the run ended in the program's HALT" (Msx.cpu_halted t);
  check "page 0 stayed RAM" (Msx.ppi_a t land 3 = 3);
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "page0_own_code_vector_test: all pass"
