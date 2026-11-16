(* MegaROM mappers: the banking mechanism and the type guess. Synthetic ROMs
   only, no copyrighted content — each 8KB segment's first byte is its own index,
   so reading through a window tells us which segment its bank register selected.
   The banking is exercised through the real address space (mem_read/mem_write
   with the cart mapped into slot 2), the same path the Z80 takes. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let machine () =
  Msx.create ~machine:{ Msx.ram_kb = 64; vram_kb = 128; roms = [ ""; ""; "" ] }

(* [nseg] 8KB segments; byte 0 of segment s is s, the rest zero. *)
let synth nseg =
  let b = Bytes.make (nseg * 0x2000) '\000' in
  for s = 0 to nseg - 1 do
    Bytes.set b (s * 0x2000) (Char.chr (s land 0xff))
  done;
  Bytes.to_string b

(* Map the cart into slot 2 across pages 1 and 2 (0x4000-0xBFFF), the whole cart
   window. PPI port A = 0xA8: two bits per page, page1=slot2, page2=slot2. *)
let show_cart t = Msx.port_out t 0xA8 0x28

(* seg index shown in the window at [addr]. *)
let win t addr = Msx.mem_read t addr

let konami () =
  let t = machine () in
  Msx.load_cartridge ~mapper:Msx.Konami t (synth 8);
  show_cart t;
  check "konami boots linear 0x4000=seg0" (win t 0x4000 = 0);
  check "konami boots linear 0x6000=seg1" (win t 0x6000 = 1);
  check "konami boots linear 0x8000=seg2" (win t 0x8000 = 2);
  check "konami boots linear 0xA000=seg3" (win t 0xA000 = 3);
  Msx.mem_write t 0x8000 5;
  check "konami 0x8000 selects seg5" (win t 0x8000 = 5);
  check "konami 0x4000 stays seg0 (fixed)" (win t 0x4000 = 0);
  Msx.mem_write t 0x6000 7;
  check "konami 0x6000 selects seg7" (win t 0x6000 = 7);
  Msx.mem_write t 0xA000 6;
  check "konami 0xA000 selects seg6" (win t 0xA000 = 6);
  (* a write into the fixed window changes nothing *)
  Msx.mem_write t 0x4000 3;
  check "konami 0x4000 write is inert" (win t 0x4000 = 0)

let konami_scc () =
  let t = machine () in
  Msx.load_cartridge ~mapper:Msx.Konami_scc t (synth 8);
  show_cart t;
  (* SCC selects all four windows: 0x5000/0x7000/0x9000/0xB000. *)
  Msx.mem_write t 0x5000 4;
  check "scc 0x5000 selects window0 seg4" (win t 0x4000 = 4);
  Msx.mem_write t 0x9000 6;
  check "scc 0x9000 selects window2 seg6" (win t 0x8000 = 6);
  Msx.mem_write t 0xB000 7;
  check "scc 0xB000 selects window3 seg7" (win t 0xA000 = 7);
  (* a write that misses the register address does not bank *)
  Msx.mem_write t 0x8000 1;
  check "scc 0x8000 write is inert" (win t 0x8000 = 6)

let ascii8 () =
  let t = machine () in
  Msx.load_cartridge ~mapper:Msx.Ascii8 t (synth 8);
  show_cart t;
  (* registers at 0x6000/0x6800/0x7000/0x7800 for windows 0..3 *)
  Msx.mem_write t 0x6000 4;
  check "ascii8 0x6000 selects window0 seg4" (win t 0x4000 = 4);
  Msx.mem_write t 0x7000 5;
  check "ascii8 0x7000 selects window2 seg5" (win t 0x8000 = 5);
  Msx.mem_write t 0x7800 6;
  check "ascii8 0x7800 selects window3 seg6" (win t 0xA000 = 6)

let ascii16 () =
  let t = machine () in
  Msx.load_cartridge ~mapper:Msx.Ascii16 t (synth 8);
  show_cart t;
  check "ascii16 boots linear 0x4000=seg0" (win t 0x4000 = 0);
  check "ascii16 boots linear 0x6000=seg1" (win t 0x6000 = 1);
  check "ascii16 boots linear 0x8000=seg2" (win t 0x8000 = 2);
  (* 0x6000 selects the 16KB bank at 0x4000-0x7FFF: bank 2 -> segs 4,5 *)
  Msx.mem_write t 0x6000 2;
  check "ascii16 0x6000 low half is seg4" (win t 0x4000 = 4);
  check "ascii16 0x6000 high half is seg5" (win t 0x6000 = 5);
  (* 0x7000 selects the 16KB bank at 0x8000-0xBFFF: bank 3 -> segs 6,7 *)
  Msx.mem_write t 0x7000 3;
  check "ascii16 0x7000 low half is seg6" (win t 0x8000 = 6);
  check "ascii16 0x7000 high half is seg7" (win t 0xA000 = 7)

(* A ROM whose code writes to the given register addresses via [ld (nn),a]. *)
let rom_with_writes addrs =
  let b = Bytes.of_string (synth 8) in
  List.iteri
    (fun i addr ->
      let p = 0x100 + (i * 3) in
      Bytes.set b p '\x32';
      Bytes.set b (p + 1) (Char.chr (addr land 0xff));
      Bytes.set b (p + 2) (Char.chr ((addr lsr 8) land 0xff)))
    addrs;
  Bytes.to_string b

let detection () =
  check "flat for a 32KB rom" (Msx.guess_mapper (synth 4) = Msx.Flat);
  check "konami from 0x6000/0x8000/0xA000"
    (Msx.guess_mapper (rom_with_writes [ 0x6000; 0x8000; 0xa000 ]) = Msx.Konami);
  check "scc from 0x5000/0x9000/0xB000"
    (Msx.guess_mapper (rom_with_writes [ 0x5000; 0x9000; 0xb000 ]) = Msx.Konami_scc);
  check "ascii8 from 0x6800/0x7800"
    (Msx.guess_mapper (rom_with_writes [ 0x6800; 0x7800 ]) = Msx.Ascii8_sram);
  (* An ASCII8 game also writes 0x6000/0x7000 (two of its four registers), so a
     naive count reads it as ASCII16. The 0x6800/0x7800 writes must still win.
     This is the Deep Dungeon case. *)
  check "ascii8 wins over ascii16 when both register sets appear"
    (Msx.guess_mapper (rom_with_writes [ 0x6000; 0x6800; 0x7000; 0x7800; 0x6000; 0x7000 ])
     = Msx.Ascii8_sram);
  check "ascii16 from 0x6000/0x7000"
    (Msx.guess_mapper (rom_with_writes [ 0x6000; 0x7000 ]) = Msx.Ascii16);
  (* a flat 32KB cart reads through the flat path, not a bank register *)
  let t = machine () in
  Msx.load_cartridge t (synth 4);
  check "a 32KB cart is Flat" (Msx.cart_mapper t = Msx.Flat)

(* Koei ASCII8 with battery RAM: a 256KB cart has 32 segments, so bank bit 0x20
   is the first above the segment range and selects SRAM. A store into an SRAM
   window stays; a ROM bank still reads its segment. *)
let koei_sram () =
  let t = machine () in
  Msx.load_cartridge ~mapper:Msx.Ascii8_sram t (synth 32);
  show_cart t;
  (* register 0x7000 selects window 2 (0x8000); a plain segment reads ROM *)
  Msx.mem_write t 0x7000 5;
  check "koei rom bank reads its segment" (win t 0x8000 = 5);
  (* bank 0x20 maps SRAM into the window; a store there is read back *)
  Msx.mem_write t 0x7000 0x20;
  Msx.mem_write t 0x8000 0xab;
  check "koei sram store is read back" (win t 0x8000 = 0xab);
  check "koei sram starts clear" (win t 0x8001 = 0);
  (* back to a ROM bank shows ROM again; the SRAM keeps its byte *)
  Msx.mem_write t 0x7000 5;
  check "koei rom again after sram" (win t 0x8000 = 5);
  Msx.mem_write t 0x7000 0x20;
  check "koei sram retained across bank switch" (win t 0x8000 = 0xab)

(* The 64KB RAM cartridge (openMSX -extb ram64k): RAM over all four pages of
   primary slot 2, every byte 0xFF at power-on, saved with the machine. *)
let pages = [ 0x0000; 0x4000; 0x8000; 0xc000 ]

(* PPI port A = 0xAA: every page on slot 2. 0x00: every page on slot 0. *)
let all_slot2 t = Msx.port_out t 0xA8 0xAA

(* A save whose mapper code is [code]: the blobs ram, main, logo, sub, cart,
   cart SRAM and disk each carry an 8-byte length, and the mapper code
   follows them. The checksum is recomputed so only the restore's own
   checks can refuse the result. *)
let with_mapper_code state code =
  let magic_and_digest = 11 + 16 in
  let b = Bytes.of_string state in
  let pos = ref magic_and_digest in
  for _ = 1 to 7 do
    pos := !pos + 8 + Int64.to_int (Bytes.get_int64_be b !pos)
  done;
  Bytes.set_int64_be b !pos (Int64.of_int code);
  let payload = Bytes.sub_string b magic_and_digest (Bytes.length b - magic_and_digest) in
  Bytes.blit_string (Digest.string payload) 0 b 11 16;
  Bytes.to_string b

let ram_cart () =
  let t = machine () in
  let ppi = Msx.port_in t 0xA8 in
  Msx.insert_ram_cartridge t;
  check "ram cart: the slot selection is left alone" (Msx.port_in t 0xA8 = ppi);
  check "ram cart: the mapper reads Ram64" (Msx.cart_mapper t = Msx.Ram64);
  all_slot2 t;
  List.iter
    (fun a ->
      check (Printf.sprintf "ram cart: 0x%04x powers up 0xFF" a) (Msx.mem_read t a = 0xff);
      check (Printf.sprintf "ram cart: 0x%04x top powers up 0xFF" a)
        (Msx.mem_read t (a + 0x3fff) = 0xff))
    pages;
  List.iteri (fun i a -> Msx.mem_write t a (0x10 + i)) pages;
  Msx.mem_write t 0xffff 0x5a;
  List.iteri
    (fun i a ->
      check (Printf.sprintf "ram cart: 0x%04x keeps its byte" a) (Msx.mem_read t a = 0x10 + i))
    pages;
  check "ram cart: 0xFFFF is RAM, slot 2 is not expanded" (Msx.mem_read t 0xffff = 0x5a);
  (* the main RAM in slot 3-2 is a different memory *)
  Msx.port_out t 0xA8 0xFF;
  Msx.mem_write t 0xffff 0xAA;
  Msx.mem_write t 0x8000 0x77;
  all_slot2 t;
  check "ram cart: slot 3 writes do not reach it" (Msx.mem_read t 0x8000 = 0x12);
  (* a save keeps the contents; the restored copy is its own memory *)
  (match Msx.restore ~state:(Msx.serialize t) with
   | Error e -> check ("ram cart: restore " ^ e) false
   | Ok copy ->
     check "ram cart: the mapper survives a save" (Msx.cart_mapper copy = Msx.Ram64);
     check "ram cart: the bytes survive a save" (Msx.mem_read copy 0xc000 = 0x13);
     Msx.mem_write copy 0xc000 0x99;
     check "ram cart: the copy does not alias" (Msx.mem_read t 0xc000 = 0x13));
  (* a save that claims a RAM cart over a 32KB ROM is refused *)
  let rom = machine () in
  Msx.load_cartridge ~mapper:Msx.Flat rom (synth 4);
  check "ram cart: a forged save keeps a valid checksum"
    (Result.is_ok (Msx.restore ~state:(with_mapper_code (Msx.serialize rom) 0)));
  check "ram cart: a 32KB cart cannot restore as Ram64"
    (Result.is_error (Msx.restore ~state:(with_mapper_code (Msx.serialize rom) 6)));
  check "ram cart: load_cartridge refuses Ram64"
    (match Msx.load_cartridge ~mapper:Msx.Ram64 (machine ()) (synth 8) with
     | () -> false
     | exception Invalid_argument _ -> true);
  (* a ROM cart plugged in afterwards replaces the RAM *)
  Msx.load_cartridge t (synth 4);
  all_slot2 t;
  check "ram cart: a ROM cart replaces it" (Msx.cart_mapper t = Msx.Flat && win t 0x6000 = 1)

let () =
  ram_cart ();
  konami ();
  konami_scc ();
  ascii8 ();
  ascii16 ();
  koei_sram ();
  detection ();
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "mapper_test ok"
