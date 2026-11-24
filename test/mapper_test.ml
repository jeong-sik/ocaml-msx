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
  (* openMSX RomAscii8kB::reset: setRom(i, 0) for every window *)
  List.iter
    (fun a -> check (Printf.sprintf "ascii8 reset 0x%04X=seg0" a) (win t a = 0))
    [ 0x4000; 0x6000; 0x8000; 0xA000 ];
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
  (* openMSX RomAscii16kB::reset: setRom(1, 0), setRom(2, 0) -- both 16KB
     windows show bank 0 *)
  check "ascii16 reset 0x4000=seg0" (win t 0x4000 = 0);
  check "ascii16 reset 0x6000=seg1" (win t 0x6000 = 1);
  check "ascii16 reset 0x8000=seg0" (win t 0x8000 = 0);
  check "ascii16 reset 0xA000=seg1" (win t 0xA000 = 1);
  (* 0x6000 selects the 16KB bank at 0x4000-0x7FFF: bank 2 -> segs 4,5 *)
  Msx.mem_write t 0x6000 2;
  check "ascii16 0x6000 low half is seg4" (win t 0x4000 = 4);
  check "ascii16 0x6000 high half is seg5" (win t 0x6000 = 5);
  (* 0x7000 selects the 16KB bank at 0x8000-0xBFFF: bank 3 -> segs 6,7 *)
  Msx.mem_write t 0x7000 3;
  check "ascii16 0x7000 low half is seg6" (win t 0x8000 = 6);
  check "ascii16 0x7000 high half is seg7" (win t 0xA000 = 7)

(* A 64KB ROM whose code writes to the given register addresses via
   [ld (nn),a]. It starts with an "AB" header, as a MegaROM's segment 0 does:
   a 64KB ROM without one is plain (openMSX RomFactory). *)
let rom_with_writes addrs =
  let b = Bytes.of_string (synth 8) in
  Bytes.blit_string "AB" 0 b 0 2;
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
  check "a 64KB rom without a header at its first byte is plain"
    (Msx.guess_mapper (synth 8) = Msx.Flat);
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
  (* a ROM cart plugged in afterwards replaces the RAM. A plain ROM with no
     "AB" header starts at 0x0000 (openMSX RomPlain), so 0x6000 shows its
     segment 3. *)
  Msx.load_cartridge t (synth 4);
  all_slot2 t;
  check "ram cart: a ROM cart replaces it" (Msx.cart_mapper t = Msx.Flat && win t 0x6000 = 3)

(* Plain ROMs (openMSX RomPlain, type Mirrored). Each 8KB page of the file
   holds its own index at offset 0x100; the "AB" header at file offset
   [header] with routine pointers [ptrs] (INIT, STATEMENT, ...) decides where
   the ROM starts. All four CPU pages are switched to slot 2 and each 8KB page
   is read at +0x100. *)
let plain_rom ~size ~header ptrs =
  let b = Bytes.make size '\000' in
  for p = 0 to (size / 0x2000) - 1 do
    Bytes.set b ((p * 0x2000) + 0x100) (Char.chr p)
  done;
  Bytes.blit_string "AB" 0 b header 2;
  List.iteri
    (fun i ptr ->
      Bytes.set b (header + 2 + (2 * i)) (Char.chr (ptr land 0xff));
      Bytes.set b (header + 3 + (2 * i)) (Char.chr (ptr lsr 8)))
    ptrs;
  Bytes.to_string b

let plain () =
  let shows name rom expect =
    let t = machine () in
    Msx.load_cartridge t rom;
    Msx.port_out t 0xA8 0xAA;
    let got = List.init 8 (fun p -> Msx.mem_read t ((p * 0x2000) + 0x100)) in
    check (name ^ " is plain") (Msx.cart_mapper t = Msx.Flat);
    check
      (Printf.sprintf "%s: pages %s" name (String.concat "," (List.map string_of_int got)))
      (got = expect)
  in
  let at_4000 = [ 2; 3; 0; 1; 2; 3; 0; 1 ] and at_8000 = [ 0; 1; 2; 3; 0; 1; 2; 3 ] in
  shows "8KB at 0x4000" (plain_rom ~size:0x2000 ~header:0 [ 0x4010 ]) [ 0; 0; 0; 0; 0; 0; 0; 0 ];
  shows "16KB at 0x4000" (plain_rom ~size:0x4000 ~header:0 [ 0x4010 ]) [ 0; 1; 0; 1; 0; 1; 0; 1 ];
  shows "32KB at 0x4000" (plain_rom ~size:0x8000 ~header:0 [ 0x4010 ]) at_4000;
  shows "32KB at 0x8000" (plain_rom ~size:0x8000 ~header:0 [ 0x8010 ]) at_8000;
  (* A pointer counts from its header's place in the file: a header at file
     offset 0x4000 pointing into 0x8000 votes for the file starting at 0x4000. *)
  shows "32KB, header at 0x4000 into 0x8000" (plain_rom ~size:0x8000 ~header:0x4000 [ 0x8010 ]) at_4000;
  (* One vote each for 0x0000 and 0x4000: 0x4000 wins the tie. One each for
     0x0000 and 0x8000: 0x0000 wins. A 24KB ROM tells the two apart: from
     0x0000 page 3 shows 3 land 2 = 2; from 0x8000 page 1 shows -3 land 2 = 0. *)
  shows "32KB, tie 0x0000/0x4000" (plain_rom ~size:0x8000 ~header:0 [ 0x0010; 0x4020 ]) at_4000;
  shows "24KB, tie 0x0000/0x8000"
    (plain_rom ~size:0x6000 ~header:0 [ 0x0010; 0x8020 ])
    [ 0; 1; 2; 2; 0; 0; 2; 2 ];
  shows "24KB at 0x8000" (plain_rom ~size:0x6000 ~header:0 [ 0x8020 ]) [ 0; 0; 2; 2; 0; 1; 2; 2 ];
  (* The header sits at file offset 0x4000 and points into 0x4000: the file
     starts at 0x0000. 48KB is six pages: page 6 shows 6 land 5 = 4. *)
  shows "48KB at 0x0000" (plain_rom ~size:0xc000 ~header:0x4000 [ 0x4010 ]) [ 0; 1; 2; 3; 4; 5; 4; 5 ];
  shows "64KB at 0x0000" (plain_rom ~size:0x10000 ~header:0x4000 [ 0x4010 ]) [ 0; 1; 2; 3; 4; 5; 6; 7 ]

(* A MegaROM is no plain ROM: outside 0x4000-0xBFFF the ASCII mappers show
   an empty bus, not a mirror of the file (openMSX RomAscii8kB, RomAscii16kB,
   RomAscii8_8 reset). *)
let megarom_outside () =
  List.iter
    (fun (name, mapper) ->
      let t = machine () in
      Msx.load_cartridge ~mapper t (synth 8);
      Msx.port_out t 0xA8 0xAA;
      List.iter
        (fun a ->
          check (Printf.sprintf "%s: 0x%04x reads 0xFF" name a) (win t a = 0xff))
        [ 0x0000; 0x2000; 0xc000; 0xe000 ])
    [ ("ascii8", Msx.Ascii8); ("ascii16", Msx.Ascii16); ("koei", Msx.Ascii8_sram) ]

(* The RAM mapper in slot 3-2 (openMSX MSXMemoryMapperBase). A register
   keeps bit_ceil(segments) - 1 and reads back with the bits above set; one
   past the last segment folds with segments - 1. *)
let ram_mapper () =
  let ram kb =
    let t = Msx.create ~machine:{ Msx.ram_kb = kb; vram_kb = 128; roms = [ ""; ""; "" ] } in
    (* every page in slot 3, every page of slot 3 in subslot 2 *)
    Msx.port_out t 0xA8 0xff;
    Msx.mem_write t 0xffff 0xaa;
    (* byte 0x8000 of each segment holds its index *)
    for s = 0 to (kb / 16) - 1 do
      Msx.port_out t 0xFE s;
      Msx.mem_write t 0x8000 s
    done;
    t
  in
  let t = ram 512 in
  Msx.port_out t 0xFE 0x23;
  check "512KB mapper reads segment 3 as 0xE3" (Msx.port_in t 0xFE = 0xe3);
  check "512KB mapper keeps 5 bits" (Msx.mem_read t 0x8000 = 3);
  Msx.port_out t 0xFE 31;
  check "512KB mapper reaches segment 31" (Msx.mem_read t 0x8000 = 31 && Msx.port_in t 0xFE = 0xff);
  let t = ram 64 in
  Msx.port_out t 0xFE 0x05;
  check "64KB mapper reads segment 1 as 0xFD" (Msx.port_in t 0xFE = 0xfd);
  check "64KB mapper keeps 2 bits" (Msx.mem_read t 0x8000 = 1);
  let t = ram 96 in
  Msx.port_out t 0xFE 7;
  check "96KB mapper keeps 3 bits" (Msx.port_in t 0xFE = 0xff);
  check "96KB mapper folds segment 7 to 5" (Msx.mem_read t 0x8000 = 5);
  Msx.port_out t 0xFE 6;
  check "96KB mapper folds segment 6 to 4" (Msx.mem_read t 0x8000 = 4);
  let t = ram 4096 in
  Msx.port_out t 0xFE 200;
  check "4MB mapper reaches segment 200" (Msx.mem_read t 0x8000 = 200 && Msx.port_in t 0xFE = 200)

(* Pages 0 and 3 of the cart slot (openMSX RomKonami / RomKonamiSCC
   bankSwitch, RomAscii8kB / RomAscii16kB reset): Konami mirrors the
   0x4000-0x7FFF windows at 0x0000 and the 0x8000-0xBFFF ones at 0xC000,
   Konami SCC the other way round, and an ASCII cart leaves both pages empty.
   Byte 0 of each 8KB segment is its number (see [synth]). *)
let outer_pages () =
  let shows name mapper switch expect =
    let t = machine () in
    Msx.load_cartridge ~mapper t (synth 8);
    Msx.port_out t 0xA8 0xAA;
    List.iter (fun (a, v) -> Msx.mem_write t a v) switch;
    let got = List.map (win t) [ 0x0000; 0x2000; 0xc000; 0xe000 ] in
    check
      (Printf.sprintf "%s: 0000 2000 c000 e000 show %s" name
         (String.concat "," (List.map string_of_int got)))
      (got = expect)
  in
  shows "konami at reset" Msx.Konami [] [ 0; 1; 2; 3 ];
  shows "konami after 0x6000=5 0x8000=6" Msx.Konami [ (0x6000, 5); (0x8000, 6) ] [ 0; 5; 6; 3 ];
  shows "konami scc at reset" Msx.Konami_scc [] [ 2; 3; 0; 1 ];
  shows "konami scc after 0x5000=4 0x9000=6" Msx.Konami_scc [ (0x5000, 4); (0x9000, 6) ]
    [ 6; 3; 4; 1 ];
  shows "ascii8" Msx.Ascii8 [] [ 0xff; 0xff; 0xff; 0xff ];
  shows "ascii16" Msx.Ascii16 [] [ 0xff; 0xff; 0xff; 0xff ];
  (* NEO carts cover 0x0000-0xBFFF: page 0 shows the cart, 0xC000 up stays empty *)
  shows "neo8 at reset" Msx.Neo8 [] [ 0; 0; 0xff; 0xff ];
  shows "neo8 after 0x5000=3 0x5800=5" Msx.Neo8 [ (0x5000, 3); (0x5800, 5) ] [ 3; 5; 0xff; 0xff ];
  shows "neo16 at reset" Msx.Neo16 [] [ 0; 1; 0xff; 0xff ];
  shows "neo16 after 0x5000=2" Msx.Neo16 [ (0x5000, 2) ] [ 4; 5; 0xff; 0xff ]

(* Plugging a cart does not touch slot selection: PPI port A stays at its
   power-on 0x00 (every page on slot 0), as on the real machine and in
   openMSX. C-BIOS finds the cart by scanning the slots itself; a page 1
   left on slot 2 would outlive the scan, which only rewrites the page 2
   and 3 bits. *)
let ppi_untouched () =
  let t = machine () in
  Msx.load_cartridge t (synth 2);
  check "cart leaves PPI A at 0x00" (Msx.port_in t 0xA8 = 0x00)

(* The NEO flash carts (openMSX RomNeo8 / RomNeo16): 14-bit bank registers
   written as low byte (even address) then high six bits (odd address), decoded
   from A11-A13 only, every region at block 0 after reset, regions over
   0x0000-0xBFFF (the whole of slot 2, page 3 empty). Segment s of this ROM
   holds s land 0xff at byte 0 and s lsr 8 at byte 1, so a 14-bit bank shows. *)
let synth_wide nseg =
  let b = Bytes.make (nseg * 0x2000) '\000' in
  for s = 0 to nseg - 1 do
    Bytes.set b (s * 0x2000) (Char.chr (s land 0xff));
    Bytes.set b ((s * 0x2000) + 1) (Char.chr (s lsr 8))
  done;
  Bytes.to_string b

let seg_at t addr = Msx.mem_read t addr lor (Msx.mem_read t (addr + 1) lsl 8)

let neo8 () =
  let t = machine () in
  Msx.load_cartridge ~mapper:Msx.Neo8 t (synth_wide 0x120);
  Msx.port_out t 0xA8 0xAA;
  check "neo8 resets every region to block 0"
    (List.for_all (fun a -> seg_at t a = 0) [ 0x0000; 0x2000; 0x4000; 0x6000; 0x8000; 0xA000 ]);
  check "neo8 leaves 0xC000-0xFFFF empty" (win t 0xC000 = 0xff && win t 0xE000 = 0xff);
  Msx.mem_write t 0x5000 4;
  Msx.mem_write t 0x5800 5;
  check "neo8 0x5000/0x5800 select the regions at 0x0000/0x2000"
    (seg_at t 0x0000 = 4 && seg_at t 0x2000 = 5 && seg_at t 0x4000 = 0);
  Msx.mem_write t 0x9000 6;
  check "neo8 0x9000 mirrors the 0x5000 register" (seg_at t 0x0000 = 6);
  Msx.mem_write t 0x1000 7;
  check "neo8 a write at 0x1000 decodes as the 0x5000 register" (seg_at t 0x0000 = 7);
  Msx.mem_write t 0x6000 5;
  check "neo8 0x6000 low byte selects window0 seg5" (seg_at t 0x4000 = 5);
  check "neo8 0x6000 leaves window1 alone" (seg_at t 0x6000 = 0);
  Msx.mem_write t 0x6001 1;
  check "neo8 odd address sets the high bits (seg 0x105)" (seg_at t 0x4000 = 0x105);
  Msx.mem_write t 0x6000 6;
  check "neo8 a low byte write keeps the high bits (seg 0x106)" (seg_at t 0x4000 = 0x106);
  Msx.mem_write t 0x6800 3;
  Msx.mem_write t 0x7000 4;
  Msx.mem_write t 0x7800 6;
  check "neo8 0x6800/0x7000/0x7800 select windows 1..3"
    (seg_at t 0x6000 = 3 && seg_at t 0x8000 = 4 && seg_at t 0xA000 = 6);
  (* 0x41: only the low six bits reach the register, so the bank is 0x100|2. *)
  Msx.mem_write t 0x7000 2;
  Msx.mem_write t 0x7001 0x41;
  check "neo8 high byte keeps six bits" (seg_at t 0x8000 = 0x102);
  (* the registers repeat 0x4000 higher *)
  Msx.mem_write t 0xA000 7;
  Msx.mem_write t 0xA001 0;
  check "neo8 0xA000 mirrors the 0x6000 register" (seg_at t 0x4000 = 7);
  (* A11-A13 = 0 and 1 (0x4000, 0x4800) hit no register *)
  List.iter (fun a -> Msx.mem_write t a 9) [ 0x4000; 0x4800; 0x0000; 0x0800 ];
  check "neo8 writes at A11-A13 = 0,1 are inert"
    (seg_at t 0x0000 = 7 && seg_at t 0x2000 = 5 && seg_at t 0x4000 = 7 && seg_at t 0x6000 = 3
     && seg_at t 0x8000 = 0x102 && seg_at t 0xA000 = 6)

let neo16 () =
  let t = machine () in
  Msx.load_cartridge ~mapper:Msx.Neo16 t (synth_wide 0x208);
  Msx.port_out t 0xA8 0xAA;
  check "neo16 resets every region to block 0"
    (seg_at t 0x0000 = 0 && seg_at t 0x2000 = 1 && seg_at t 0x4000 = 0 && seg_at t 0x6000 = 1
     && seg_at t 0x8000 = 0 && seg_at t 0xA000 = 1);
  check "neo16 leaves 0xC000-0xFFFF empty" (win t 0xC000 = 0xff);
  Msx.mem_write t 0x5000 3;
  check "neo16 0x5000 selects the 16KB bank at 0x0000 (segs 6,7)"
    (seg_at t 0x0000 = 6 && seg_at t 0x2000 = 7 && seg_at t 0x4000 = 0);
  Msx.mem_write t 0x6000 2;
  check "neo16 0x6000 selects the 16KB bank at 0x4000 (segs 4,5)"
    (seg_at t 0x4000 = 4 && seg_at t 0x6000 = 5);
  Msx.mem_write t 0x7000 3;
  check "neo16 0x7000 selects the 16KB bank at 0x8000 (segs 6,7)"
    (seg_at t 0x8000 = 6 && seg_at t 0xA000 = 7);
  Msx.mem_write t 0x7001 1;
  check "neo16 odd address sets the high bits (bank 0x103, segs 0x206,0x207)"
    (seg_at t 0x8000 = 0x206 && seg_at t 0xA000 = 0x207);
  (* odd A11-A13 (0x5800, 0x6800, 0x7800) and 0, 1 (0x4000, 0x4800) are not registers. *)
  List.iter (fun a -> Msx.mem_write t a 9) [ 0x5800; 0x6800; 0x7800; 0x4000; 0x4800 ];
  check "neo16 writes at odd A11-A13 are inert"
    (seg_at t 0x0000 = 6 && seg_at t 0x4000 = 4 && seg_at t 0x8000 = 0x206);
  Msx.mem_write t 0xA000 4;
  check "neo16 0xA000 mirrors the 0x6000 register (segs 8,9)"
    (seg_at t 0x4000 = 8 && seg_at t 0x6000 = 9)

(* A bank number past the ROM, after openMSX RomBlocks::setRom: a block
   below nrBlocks is used as is, a larger one is masked with blockMask
   (nrBlocks - 1, or 31 for RomKonami), and one still past the end leaves
   the window unmapped (0xFF). *)
let out_of_range () =
  let bank mapper rom reg v addr =
    let t = machine () in
    Msx.load_cartridge ~mapper t rom;
    show_cart t;
    Msx.mem_write t reg v;
    win t addr
  in
  (* 16 segments: 255 -> 15, 16 -> 0, 17 -> 1 (the MSXgl s_mapper case) *)
  check "ascii8 255 wraps to 15" (bank Msx.Ascii8 (synth 16) 0x7800 255 0xA000 = 15);
  check "ascii8 16 wraps to 0" (bank Msx.Ascii8 (synth 16) 0x7800 16 0xA000 = 0);
  check "ascii8 17 wraps to 1" (bank Msx.Ascii8 (synth 16) 0x7800 17 0xA000 = 1);
  (* 12 segments: the mask is 11, not the next power of two minus one *)
  check "ascii8 13 of 12 -> 9" (bank Msx.Ascii8 (synth 12) 0x7800 13 0xA000 = 9);
  check "ascii8 12 of 12 -> 8" (bank Msx.Ascii8 (synth 12) 0x7800 12 0xA000 = 8);
  check "ascii8 4 of 12 is not masked" (bank Msx.Ascii8 (synth 12) 0x7800 4 0xA000 = 4);
  (* ASCII16 counts 16KB banks: 8 segments are 4 banks, 5 -> 1 = segs 2,3 *)
  check "ascii16 5 of 4 -> 1" (bank Msx.Ascii16 (synth 8) 0x7000 5 0x8000 = 2);
  (* Konami masks with 31 whatever the size: 8 segments, 13 -> 13 is past
     the end and reads 0xFF; 35 -> 3 *)
  check "konami 13 of 8 is unmapped" (bank Msx.Konami (synth 8) 0x8000 13 0x8000 = 0xff);
  check "konami 35 of 8 -> 3" (bank Msx.Konami (synth 8) 0x8000 35 0x8000 = 3);
  check "scc 63 of 8 -> 7" (bank Msx.Konami_scc (synth 8) 0x9000 63 0x8000 = 7);
  (* A ROM 3 bytes short of 8 segments is padded with 0xFF to 64KB: block
     63 is block 7, at its start and not 3 bytes into it, and its last
     bytes are the padding. *)
  let short = String.sub (synth 8) 0 ((8 * 0x2000) - 3) in
  check "short rom: 63 -> 7 at its start" (bank Msx.Konami_scc short 0x9000 63 0x8000 = 7);
  check "short rom: padding reads 0xFF" (bank Msx.Konami_scc short 0x9000 7 0x9fff = 0xff)

(* Koei SRAM: openMSX RomAscii8_8 KOEI (sramPages 0x34) stores through
   windows 0, 2 and 3; a store into window 1 (0x6000-0x7FFF) is a bank
   select and reads through ROM. The SRAM bit is the ROM's block count: 24 segments -> 0x18,
   so bank 8 selects SRAM. *)
let koei_windows () =
  let t = machine () in
  Msx.load_cartridge ~mapper:Msx.Ascii8_sram t (synth 32);
  show_cart t;
  Msx.mem_write t 0x6000 0x20;
  Msx.mem_write t 0x4000 0xcd;
  check "koei window 0 stores into sram" (win t 0x4000 = 0xcd);
  Msx.mem_write t 0x6800 0x21;
  check "koei window 1 reads rom bank 1" (win t 0x6000 = 1);
  Msx.mem_write t 0x6800 0x20;
  check "koei window 1 reads rom bank 0" (win t 0x6000 = 0);
  let t = machine () in
  Msx.load_cartridge ~mapper:Msx.Ascii8_sram t (synth 24);
  show_cart t;
  Msx.mem_write t 0x7000 8;
  Msx.mem_write t 0x8000 0x5a;
  check "koei 24 segments: bank 8 has the sram bit" (win t 0x8000 = 0x5a);
  Msx.mem_write t 0x7000 7;
  check "koei 24 segments: bank 7 is rom" (win t 0x8000 = 7)

let () =
  ram_cart ();
  ppi_untouched ();
  konami ();
  out_of_range ();
  koei_windows ();
  konami_scc ();
  ascii8 ();
  ascii16 ();
  neo8 ();
  neo16 ();
  koei_sram ();
  detection ();
  plain ();
  megarom_outside ();
  ram_mapper ();
  outer_pages ();
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "mapper_test ok"
