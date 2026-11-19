(* A CALL 0x0024 while page 0 is RAM and nothing was planted there is an
   ENASLT served in OCaml (issue #147).

   Europa Sensen's loader, entered through the warm-up replay, ends with
   ENASLT(A=0, H=0) to put the BIOS back in page 0. Page 0 is RAM there and
   the vector still holds the power-on fill, a RST 38h that recurses until
   the stack is gone. enaslt_ram_call_test pins the other side: a kernel that
   planted its own routine at 0x0024 keeps running it. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

(* The caller below puts the return address of its CALL at 0x000A. *)
let return_addr = 0x0a

(* main_rom has a HALT at the return address: the CPU reaches it only when
   the call switched page 0 to slot 0. *)
let build_main_rom () =
  let main = Bytes.make 0x4000 '\000' in
  Bytes.set main return_addr '\x76';
  main

(* The warm-up replay shape: a disk mounted, page 0 and page 3 on slot 3,
   slot 3's sub-slot select on the RAM. *)
let machine () =
  let t =
    Msx.create
      ~machine:{ Msx.ram_kb = 64; vram_kb = 128; roms = [ Bytes.to_string (build_main_rom ()) ] }
  in
  Msx.load_disk ~interface_rom:false t "\000";
  Msx.port_out t 0xa8 0xc3;
  Msx.mem_write t 0xffff 0xaa;
  t

(* RAM 0: LD SP,0xFFF0; LD A,slot; LD H,hi; CALL 0x0024. The CALL is the last
   RAM instruction; [halt_in_ram] adds the HALT it returns to. *)
let plant_caller t ~slot ~hi ~halt_in_ram =
  List.iteri
    (fun i b -> Msx.mem_write t i b)
    [ 0x31; 0xf0; 0xff; 0x3e; slot; 0x26; hi; 0xcd; 0x24; 0x00 ];
  if halt_in_ram then Msx.mem_write t return_addr 0x76

let ppi_page t page = (Msx.ppi_a t lsr (page * 2)) land 3
let sub_slot t page = (Msx.slot3_sel t lsr (page * 2)) land 3

let () =
  (* ENASLT(A=0, H=0x00): page 0 goes to slot 0, the CPU returns into main_rom. *)
  let t = machine () in
  plant_caller t ~slot:0 ~hi:0x00 ~halt_in_ram:false;
  Msx.step t ~frames:1;
  check "page 0 reads slot 0" (ppi_page t 0 = 0);
  check "pages 1-3 are left alone" (Msx.ppi_a t land 0xfc = 0xc0);
  check "the call returned into main_rom's HALT" (Msx.cpu_halted t);
  (* ENASLT(A=2, H=0x40): page 1 goes to slot 2, page 0 stays RAM. *)
  let t = machine () in
  plant_caller t ~slot:2 ~hi:0x40 ~halt_in_ram:true;
  Msx.step t ~frames:1;
  check "page 1 reads slot 2" (ppi_page t 1 = 2);
  check "page 0 is still slot 3" (ppi_page t 0 = 3);
  check "the call returned into RAM's HALT" (Msx.cpu_halted t);
  (* ENASLT(A=0x87, H=0x80): slot 3 sub-slot 1 for page 2. *)
  let t = machine () in
  plant_caller t ~slot:0x87 ~hi:0x80 ~halt_in_ram:true;
  Msx.step t ~frames:1;
  check "page 2 reads slot 3" (ppi_page t 2 = 3);
  check "page 2 takes sub-slot 1" (sub_slot t 2 = 1);
  check "the other pages keep sub-slot 2" (sub_slot t 0 = 2 && sub_slot t 1 = 2 && sub_slot t 3 = 2);
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "enaslt_untouched_vector_test: all pass"
