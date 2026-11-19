(* A standard MSXDOS.SYS cannot run without the real disk ROM's work areas, so
   when a boot sector jumps into one, [Msx.disk_trap] skips the kernel: it
   plants the page-0 vectors the kernel leaves, loads COMMAND.COM at 0100h and
   serves BDOS in OCaml.

   The image here is a 720K FAT12 disk with MSXDOS.SYS, COMMAND.COM and
   HELLO.TXT. Its boot sector reads MSXDOS.SYS to 0100h through H.PHYD and
   jumps there. COMMAND.COM is a small program that stores what it saw in
   0C1xxh:

   - C100 marker: COMMAND.COM ran (not the kernel image at the same address)
   - SFIRST with ????????TXT finds HELLO.TXT; DTA = drive 1 + the 32-byte entry
   - FCB opened at one address, copied, then read through the copy (_RDBLK) --
     what COMMAND.COM does with the entry a search leaves in its DTA
   - SFIRST of a name that is not there fails
   - _CONOUT, _STROUT, _CONIN and _BUFIN reach the BIOS CHPUT and CHGET with
     the default DTA (0080h-00FFh, which overlaps the fixed vectors at 009Fh
     and 00A2h) full of file data. CHGET hands out the keys K a b c d BS e CR in
     turn; _BUFIN (3 keys at most) must end with "abe" and echo every key
     it kept.
   - CALSLT into a ROM routine that runs at the address it will return to:
     the caller sits in page 1 RAM at 410Bh, the ROM routine jumps to 410Bh
     of the ROM and returns from there. The wiring must come back only when
     the routine returns.

   A disk whose first file lacks the kernel's bytes is not taken for MSXDOS.SYS:
   its code runs as it is. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let lo n = n land 0xff
let hi n = (n lsr 8) land 0xff

let ld_a n = [ 0x3e; n ]
let st_a nn = [ 0x32; lo nn; hi nn ]
let ld_a_mem nn = [ 0x3a; lo nn; hi nn ]
let ld_c n = [ 0x0e; n ]
let ld_de nn = [ 0x11; lo nn; hi nn ]
let ld_hl nn = [ 0x21; lo nn; hi nn ]
let ld_bc nn = [ 0x01; lo nn; hi nn ]
let call nn = [ 0xcd; lo nn; hi nn ]
let jp nn = [ 0xc3; lo nn; hi nn ]
let ldir = [ 0xed; 0xb0 ]
let di = [ 0xf3 ]
let halt = [ 0x76 ]

let result = 0xc100
let bdos = 0x0005
let dta = 0x0080
let fcb_copy = 0x0400
let fcb_size = 0x25
let page1_stub = 0x4100
let calslt_vector = 0x001c
let rom_routine = 0x4000
let chget_vector = 0x009f
let chput_vector = 0x00a2
let chget_routine = 0x0200
let line_buffer = 0x0500

(* What CHGET hands out: the key for _CONIN, then a line for a 3 key buffer: a
   key past the limit (d) is dropped and BS takes one back. *)
let key_queue = List.map Char.code [ 'K'; 'a'; 'b'; 'c'; 'd'; '\b'; 'e'; '\r' ]

let fcb name = 0x00 :: List.init 11 (fun i -> Char.code name.[i]) @ List.init (fcb_size - 12) (fun _ -> 0)

(* The caller in page 1 RAM: CALSLT to rom_routine in slot 0, then store a
   marker. The CALL's return address is [page1_stub] + 4 + 4 + 3. *)
let stub_return = page1_stub + 11
let stub_code =
  [ 0xdd; 0x21; lo rom_routine; hi rom_routine ] (* LD IX,rom_routine *)
  @ [ 0xfd; 0x21; 0x00; 0x00 ]                   (* LD IY,0 : slot 0 *)
  @ call calslt_vector
  @ ld_a 0x99 @ st_a 0xc110 @ halt

(* COMMAND.COM: code, then the three FCBs, then the page 1 stub. *)
let command_com () =
  let code ~pat ~hello ~none ~msg ~keys ~stub =
    di
    @ ld_a 0x5a @ st_a result
    (* SFIRST ????????TXT *)
    @ ld_de pat @ ld_c 0x11 @ call bdos @ st_a 0xc101
    @ ld_a_mem dta @ st_a 0xc10a
    @ ld_a_mem (dta + 1) @ st_a 0xc102
    (* open HELLO.TXT, copy the FCB, read one record through the copy *)
    @ ld_de hello @ ld_c 0x0f @ call bdos @ st_a 0xc103
    @ ld_hl hello @ ld_de fcb_copy @ ld_bc fcb_size @ ldir
    @ ld_de fcb_copy @ [ 0x21; 0x01; 0x00 ] @ ld_c 0x27 @ call bdos @ st_a 0xc104
    @ ld_a_mem dta @ st_a 0xc105
    @ ld_a_mem (dta + 1) @ st_a 0xc106
    (* SFIRST of a name that is not on the disk *)
    @ ld_de none @ ld_c 0x11 @ call bdos @ st_a 0xc107
    (* console: CHPUT stores each character at (C130), CHGET reads the keys
       copied to (C170) *)
    @ ld_hl keys @ ld_de 0xc170 @ ld_bc (List.length key_queue) @ ldir
    @ ld_hl 0xc170 @ [ 0x22; 0x32; 0xc1 ]
    @ ld_hl 0xc140 @ [ 0x22; 0x30; 0xc1 ]
    @ [ 0x1e; Char.code 'A' ] @ ld_c 0x02 @ call bdos
    @ ld_de msg @ ld_c 0x09 @ call bdos
    @ ld_c 0x01 @ call bdos @ st_a 0xc12f
    @ ld_a 3 @ st_a line_buffer @ ld_a 0 @ st_a (line_buffer + 1)
    @ ld_de line_buffer @ ld_c 0x0a @ call bdos
    (* CALSLT whose callee runs at the return address *)
    @ ld_hl stub @ ld_de page1_stub @ ld_bc (List.length stub_code) @ ldir
    @ jp page1_stub
  in
  let code_len = List.length (code ~pat:0 ~hello:0 ~none:0 ~msg:0 ~keys:0 ~stub:0) in
  let pat = 0x100 + code_len in
  let hello = pat + fcb_size in
  let none = hello + fcb_size in
  let msg = none + fcb_size in
  let keys = msg + 3 in
  let stub = keys + List.length key_queue in
  code ~pat ~hello ~none ~msg ~keys ~stub
  @ fcb "????????TXT" @ fcb "HELLO   TXT" @ fcb "NOSUCH  TXT"
  @ [ Char.code 'B'; Char.code 'C'; Char.code '$' ]
  @ key_queue
  @ stub_code

(* Slot 0 page 1: the ROM routine jumps to the address its caller will return
   to and returns from there. *)
let main_rom ?(wait = false) () =
  let rom = Bytes.make 0x8000 '\000' in
  let put addr bytes = List.iteri (fun i b -> Bytes.set rom (addr + i) (Char.chr b)) bytes in
  (* CHGET: the next key of the queue at (C132) *)
  put chget_vector (jp chget_routine);
  put chget_routine [ 0x2a; 0x32; 0xc1; 0x7e; 0x23; 0x22; 0x32; 0xc1; 0xc9 ];
  (* CHPUT: store A at (C130) and advance it *)
  put chput_vector
    [ 0xe5; 0x2a; 0x30; 0xc1; 0x77; 0x23; 0x22; 0x30; 0xc1; 0xe1; 0xc9 ];
  put rom_routine (jp stub_return);
  (* [wait]: the routine spins until (C150) is non-zero, so a save can be
     taken while the call is pending. *)
  let spin = if wait then ld_a_mem 0xc150 @ [ 0xb7; 0x28; 0xfa ] else [] in
  put stub_return (spin @ ld_a 0x11 @ st_a 0xc111 @ [ 0xc9 ]);
  Bytes.to_string rom

let bytes_of l = Bytes.of_string (String.init (List.length l) (fun i -> Char.chr (List.nth l i)))

(* MSXDOS.SYS as the standard kernel starts: JP 0200h, and at 0238h
   LD SP,9000h / LD HL,F1C9h / CALL 4030h. *)
let kernel_sys () =
  let b = Bytes.make 2432 '\000' in
  let put at l = List.iteri (fun i v -> Bytes.set b (at + i) (Char.chr v)) l in
  put 0x000 [ 0xc3; 0x00; 0x02 ];
  put 0x138 [ 0x31; 0x00; 0x90; 0x21; 0xc9; 0xf1; 0xcd; 0x30; 0x40 ];
  b

(* A first file that is not the kernel: it just stores a marker. *)
let other_sys () =
  let b = Bytes.make 2432 '\000' in
  List.iteri (fun i v -> Bytes.set b i (Char.chr v)) (ld_a 0x77 @ st_a result @ halt);
  b

let word b i = Char.code (Bytes.get b i) lor (Char.code (Bytes.get b (i + 1)) lsl 8)
let set_word b i n =
  Bytes.set b i (Char.chr (lo n));
  Bytes.set b (i + 1) (Char.chr (hi n))

let sectors_per_fat = 3
let fat1 = 1
let fat2 = fat1 + sectors_per_fat
let root_sector = fat2 + sectors_per_fat
let data_sector = root_sector + 7
let sectors_per_cluster = 2

let blank_image () =
  let b = Bytes.make (720 * 1024) '\000' in
  set_word b 11 512; Bytes.set b 13 (Char.chr sectors_per_cluster); set_word b 14 1;
  Bytes.set b 16 '\002'; set_word b 17 112; set_word b 19 1440;
  Bytes.set b 21 '\xf9'; set_word b 22 sectors_per_fat;
  List.iter (fun s -> List.iteri (fun i v -> Bytes.set b ((s * 512) + i) (Char.chr v))
                        [ 0xf9; 0xff; 0xff ]) [ fat1; fat2 ];
  b

let put image name data =
  match Disk_fat12.put_file ~image ~name ~data with
  | Ok b -> b
  | Error _ -> failwith "FAT12 write unexpectedly rejected"

(* Boot sector code at +0x1E: DSKIO through H.PHYD reads the first file
   (5 sectors) to 0100h and jumps there. *)
let image ~sys =
  let b = blank_image () in
  let b = put b "MSXDOS  SYS" sys in
  let b = put b "COMMAND COM" (bytes_of (command_com ())) in
  let b = put b "HELLO   TXT" (Bytes.of_string ("HELLO" ^ String.make 123 'x')) in
  let cluster = word b ((root_sector * 512) + 26) in
  let sector = data_sector + ((cluster - 2) * sectors_per_cluster) in
  let boot =
    [ 0xd0 ] @ di @ ld_hl 0x0100 @ ld_de sector @ [ 0x01; 0xf9; 0x05 ]
    @ [ 0xaf ] @ call 0xffa7 @ jp 0x0100
  in
  List.iteri (fun i v -> Bytes.set b (0x1e + i) (Char.chr v)) boot;
  Bytes.to_string b

let boot ?wait sys =
  let t =
    Msx.create ~machine:{ Msx.ram_kb = 64; vram_kb = 128; roms = [ main_rom ?wait (); ""; "" ] }
  in
  (* The C-BIOS leaves the mapper on segments 3,2,1,0; at power-on all four
     pages see segment 0, and the stub copied to page 1 would overwrite the
     program. *)
  List.iteri (fun i seg -> Msx.port_out t (0xfc + i) seg) [ 3; 2; 1; 0 ];
  Msx.load_disk ~interface_rom:false t (image ~sys);
  (match Msx.boot_disk t with
   | Error e -> check ("boot_disk: " ^ e) false
   | Ok () -> Msx.step t ~frames:4);
  t

let () =
  let t = boot (kernel_sys ()) in
  let byte = Msx.mem_read t in
  check "COMMAND.COM ran in place of the kernel image" (byte result = 0x5a);
  check "COMMAND.COM sits at 0100h" (byte 0x100 = 0xf3);
  check "BDOS vector at 0005h leads to DC06h"
    (byte 0x05 = 0xc3 && byte 0x06 = 0x06 && byte 0x07 = 0xdc);
  check "warm boot vector at 0000h leads to E203h"
    (byte 0x00 = 0xc3 && byte 0x01 = 0x03 && byte 0x02 = 0xe2);
  check "CALSLT vector at 001Ch leads to E465h"
    (byte 0x1c = 0xc3 && byte 0x1d = 0x65 && byte 0x1e = 0xe4);
  check "SFIRST found HELLO.TXT" (byte 0xc101 = 0x00);
  check "SFIRST left drive 1 in the DTA" (byte 0xc10a = 0x01);
  check "SFIRST left the entry's name in the DTA" (byte 0xc102 = Char.code 'H');
  check "open succeeded" (byte 0xc103 = 0x00);
  check "_RDBLK through a copy of the FCB succeeded" (byte 0xc104 = 0x00);
  check "the copy's read filled the DMA with the file"
    (byte 0xc105 = Char.code 'H' && byte 0xc106 = Char.code 'E');
  check "SFIRST of an absent name fails" (byte 0xc107 = 0xff);
  check "_CONOUT, _STROUT, _CONIN and _BUFIN printed A, BC and every echoed key through CHPUT"
    (List.init 10 (fun i -> byte (0xc140 + i))
     = List.map Char.code [ 'A'; 'B'; 'C'; 'K'; 'a'; 'b'; 'c'; '\b'; 'e'; '\r' ]);
  check "_BUFIN ended with the line abe"
    (byte (line_buffer + 1) = 3
     && List.init 3 (fun i -> byte (line_buffer + 2 + i))
        = List.map Char.code [ 'a'; 'b'; 'e' ]);
  check "_CONIN returned the key CHGET gave" (byte 0xc12f = Char.code 'K');
  check "the DTA holds file data where the fixed vectors are" (byte 0x9f = Char.code 'x' && byte 0xa2 = Char.code 'x');
  check "CALSLT: the ROM routine ran at the return address under its own wiring"
    (byte 0xc111 = 0x11);
  check "CALSLT: the caller continued after the return" (byte 0xc110 = 0x99);
  check "all four pages are RAM again" (Msx.ppi_a t = 0xff && Msx.slot3_sel t = 0xaa);
  check "the program ended in its HALT" (Msx.cpu_halted t);
  (* A save taken while the CALSLT callee runs keeps the pending return: the
     restored machine finishes the call and puts the wiring back. *)
  let t = boot ~wait:true (kernel_sys ()) in
  check "the callee is still running when the save is taken"
    (Msx.mem_read t 0xc110 = 0x00 && Msx.ppi_a t land 0x0c = 0x00);
  (match Msx.restore ~state:(Msx.serialize t) with
   | Error e -> check ("restore: " ^ e) false
   | Ok r ->
     Msx.mem_write r 0xc150 1;
     Msx.mem_write r result 0x00;
     Msx.step r ~frames:2;
     check "restored: the program was not started again" (Msx.mem_read r result = 0x00);
     check "restored: the callee returned through the trampoline" (Msx.mem_read r 0xc111 = 0x11);
     check "restored: the caller continued after the return" (Msx.mem_read r 0xc110 = 0x99);
     check "restored: all four pages are RAM again" (Msx.ppi_a r = 0xff));
  (* A first file without the kernel's bytes is code, not MSXDOS.SYS. *)
  let t = boot (other_sys ()) in
  check "a first file without the kernel bytes runs as it is" (Msx.mem_read t result = 0x77);
  check "COMMAND.COM did not run" (Msx.mem_read t 0xc101 = 0x00);
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "dos1_kernel_test: all pass"
