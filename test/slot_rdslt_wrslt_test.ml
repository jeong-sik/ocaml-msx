(* RDSLT (0x000C) and WRSLT (0x0014) called with a slot id while page 0 is
   RAM, the shape of the DOS-kernel replay.

   Rune Master 1's slot scan tests each slot for RAM by writing the
   complement of a byte through CALL 0x0014 and reading it back through
   CALL 0x000C. On the real machine (openMSX, NMS8250) page 0 is RAM then
   and both vectors are RDSLT/WRSLT copies the disk ROM's kernel put there.
   The scan accepts slot 3-2 (the RAM mapper) and rejects slot 3-3 (nothing
   there); a scan that finds no RAM makes the kernel leave for BASIC.
   [Msx.disk_trap] serves both vectors; the machine is the replay shape of
   chsns_hle_call_test: page 0 and 3 on slot 3, slot 3's sub-slot select
   on the RAM mapper. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let run program ~pre =
  let t =
    Msx.create
      ~machine:{ Msx.ram_kb = 64; vram_kb = 128;
                 roms = [ String.make 0x4000 '\000' ] }
  in
  Msx.load_disk ~interface_rom:false t "\000";
  Msx.port_out t 0xa8 0xc3;
  Msx.mem_write t 0xffff 0xaa;
  List.iteri (fun i b -> Msx.mem_write t i b)
    ([ 0x31; 0xf0; 0xff ] @ program @ [ 0x76 ]);
  List.iter (fun (a, b) -> Msx.mem_write t a b) pre;
  Msx.step t ~frames:1;
  t

let ld_a n = [ 0x3e; n ]
let ld_hl nn = [ 0x21; nn land 0xff; nn lsr 8 ]
let ld_e n = [ 0x1e; n ]
let call nn = [ 0xcd; nn land 0xff; nn lsr 8 ]
let st_a nn = [ 0x32; nn land 0xff; nn lsr 8 ]
let ram_slot = 0x8b (* expanded slot 3, sub-slot 2: the RAM mapper *)
let empty_slot = 0x8f (* expanded slot 3, sub-slot 3: nothing there *)

let () =
  (* RDSLT reads the byte at HL in the slot A names. *)
  let t =
    run ~pre:[ (0x1000, 0x5a) ]
      (ld_a ram_slot @ ld_hl 0x1000 @ call 0x000c @ st_a 0xc100)
  in
  check "RDSLT: control returned" (Msx.cpu_halted t);
  check "RDSLT: the byte at HL of the RAM slot comes back in A"
    (Msx.mem_read t 0xc100 = 0x5a);
  check "RDSLT: ppi_a restored" (Msx.ppi_a t = 0xc3);

  (* WRSLT stores E at HL in the slot A names. *)
  let t =
    run ~pre:[]
      (ld_a ram_slot @ ld_hl 0x2000 @ ld_e 0x77 @ call 0x0014)
  in
  check "WRSLT: control returned" (Msx.cpu_halted t);
  check "WRSLT: E landed at HL in the RAM slot" (Msx.mem_read t 0x2000 = 0x77);
  check "WRSLT: ppi_a restored" (Msx.ppi_a t = 0xc3);

  (* The scan's RAM test: complement written and read back in RAM, not in a
     slot with nothing in it. *)
  let rt slot =
    run ~pre:[ (0x2000, 0x11) ]
      (ld_a slot @ ld_hl 0x2000 @ ld_e 0xee @ call 0x0014
       @ ld_a slot @ ld_hl 0x2000 @ call 0x000c @ st_a 0xc100)
  in
  let t = rt ram_slot in
  check "RAM test: the complement reads back from the RAM slot"
    (Msx.mem_read t 0xc100 = 0xee);
  let t = rt empty_slot in
  check "RAM test: the empty sub-slot does not hold the byte"
    (Msx.mem_read t 0xc100 <> 0xee);
  check "RAM test: a write to the empty sub-slot leaves the RAM slot alone"
    (Msx.mem_read t 0x2000 = 0x11);
  check "RAM test: slot registers restored after the empty-slot access"
    (Msx.ppi_a t = 0xc3);

  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "slot_rdslt_wrslt_test: all pass"
