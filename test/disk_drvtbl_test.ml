(* boot_disk plants what a one-drive disk ROM's INIT leaves for the BIOS
   (issue #147): DRVTBL (0xFB21) and the H.PHYD hook (0xFFA7). Europa Sensen's
   boot sector reads DRVTBL, halts on "Reset and hold CTRL!" unless the drive
   count is 1, then loads its program with CALL 0xFFA7 and the standard DSKIO
   registers. This test boots a synthetic sector with the same two steps on a
   bare machine: boot_disk needs only the image. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let sector_bytes = 512
let marker_sector = 5
let marker = 0x5a
let result_addr = 0xc0f0
let buffer_addr = 0xc100

(* Boot code at +0x1E, in the order Europa's does it. The result byte is 0xEE
   when the drive count is not 1, and otherwise the first byte DSKIO read. *)
let boot_code =
  [ 0xd0;                         (* RET NC *)
    0x3a; 0x21; 0xfb;             (* LD A,(0xFB21)   DRVTBL: drive count *)
    0x3d;                         (* DEC A *)
    0x20; 0x12;                   (* JR NZ,fail *)
    0x21; buffer_addr land 0xff; buffer_addr lsr 8;  (* LD HL,buffer_addr *)
    0x11; marker_sector; 0x00;    (* LD DE,marker_sector *)
    0x01; 0xf9; 0x01;             (* LD BC,0x01F9    B=1 sector, C=media F9 *)
    0xaf;                         (* XOR A           drive 0, carry clear = read *)
    0xcd; 0xa7; 0xff;             (* CALL 0xFFA7     H.PHYD *)
    0x3a; buffer_addr land 0xff; buffer_addr lsr 8;  (* LD A,(buffer_addr) *)
    0x18; 0x02;                   (* JR store *)
    0x3e; 0xee;                   (* fail: LD A,0xEE *)
    0x32; result_addr land 0xff; result_addr lsr 8;  (* store: LD (result),A *)
    0x76 ]                        (* HALT *)

let image () =
  let b = Bytes.make (720 * 1024) '\000' in
  List.iteri (fun i v -> Bytes.set b (0x1e + i) (Char.chr v)) boot_code;
  Bytes.set b (marker_sector * sector_bytes) (Char.chr marker);
  Bytes.to_string b

let () =
  let t = Msx.create ~machine:{ ram_kb = 64; vram_kb = 128; roms = [ ""; ""; "" ] } in
  Msx.load_disk ~interface_rom:false t (image ());
  (match Msx.boot_disk t with
   | Error e -> check ("boot_disk: " ^ e) false
   | Ok () ->
     check "DRVTBL says one drive" (Msx.mem_read t 0xfb21 = 1);
     (* the interface ROM boot_disk mounts sits in slot 2, not expanded *)
     check "DRVTBL names slot 2 for the interface" (Msx.mem_read t 0xfb22 = 2);
     Msx.step t ~frames:2;
     let got = Msx.mem_read t result_addr in
     check
       (Printf.sprintf "the sector read through H.PHYD arrives (got 0x%02x, want 0x%02x)"
          got marker)
       (got = marker));
  if !failures > 0 then exit 1 else print_endline "disk_drvtbl_test ok"
