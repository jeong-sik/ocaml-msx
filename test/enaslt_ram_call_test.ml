(* A plain CALL 0x0024 while page 0 is RAM runs what RAM holds there.

   0x0024 is the BIOS ENASLT vector (select a slot for a page); SNSMAT is
   0x0141. [Msx.disk_trap] serves a few read-only BIOS vectors called with a
   plain CALL as an implicit CALSLT into slot 0, and restores ppi_a when the
   call returns. That restore is right for CHSNS/CHGET/CHPUT, which leave the
   slots alone, and wrong for ENASLT, whose whole effect is the slot change it
   makes: serving 0x0024 that way undoes every ENASLT. While 0x0024 was served
   (ocaml-msx #41), a warm-boot of Sangokushi II drew nothing but black.

   The scenario plants a routine at RAM 0x0024 and a different one behind
   main_rom's 0x0024 vector, calls 0x0024 with page 0 mapped to RAM, and
   checks that the RAM routine ran, main_rom's did not, and ppi_a is what the
   RAM routine left. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

(* main_rom: 0x0024 is a JP to a body that writes 0x99 to 0xC102 -- the
   marker that would land if the call were served from slot 0. *)
let build_main_rom () =
  let main = Bytes.make 0x4000 '\000' in
  Bytes.set main 0x24 '\xc3'; Bytes.set main 0x25 '\x30'; Bytes.set main 0x26 '\x00';
  Bytes.set main 0x30 '\x3e'; Bytes.set main 0x31 '\x99';
  Bytes.set main 0x32 '\x32'; Bytes.set main 0x33 '\x02'; Bytes.set main 0x34 '\xc1';
  Bytes.set main 0x35 '\xc9';
  main

let () =
  let t =
    Msx.create
      ~machine:{ Msx.ram_kb = 64; vram_kb = 128; roms = [ Bytes.to_string (build_main_rom ()) ] }
  in
  (* The warm-up replay shape chsns_hle_call_test uses: a disk mounted,
     page 0 and page 3 on slot 3, slot 3's sub-slot select on the RAM. *)
  Msx.load_disk ~interface_rom:false t "\000";
  Msx.port_out t 0xa8 0xc3;
  Msx.mem_write t 0xffff 0xaa;
  (* RAM 0x0024: LD A,0x77; LD (0xC102),A; RET -- what a kernel planted. *)
  List.iteri (fun i b -> Msx.mem_write t (0x24 + i) b) [ 0x3e; 0x77; 0x32; 0x02; 0xc1; 0xc9 ];
  (* Caller at RAM 0: LD SP,0xFFF0; CALL 0x0024; HALT. *)
  List.iteri (fun i b -> Msx.mem_write t i b) [ 0x31; 0xf0; 0xff; 0xcd; 0x24; 0x00; 0x76 ];
  Msx.step t ~frames:1;
  check "the call returned to HALT" (Msx.cpu_halted t);
  check "the RAM routine at 0x0024 ran" (Msx.mem_read t 0xc102 = 0x77);
  check "main_rom's 0x0024 was not dispatched" (Msx.mem_read t 0xc102 <> 0x99);
  check "ppi_a is what the RAM routine left" (Msx.ppi_a t = 0xc3);
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "enaslt_ram_call_test: all pass"
