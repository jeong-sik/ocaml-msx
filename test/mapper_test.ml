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
    (Msx.guess_mapper (rom_with_writes [ 0x6800; 0x7800 ]) = Msx.Ascii8_sram Koei);
  (* An ASCII8 game also writes 0x6000/0x7000 (two of its four registers), so a
     naive count reads it as ASCII16. The 0x6800/0x7800 writes must still win.
     This is the Deep Dungeon case. *)
  check "ascii8 wins over ascii16 when both register sets appear"
    (Msx.guess_mapper (rom_with_writes [ 0x6000; 0x6800; 0x7000; 0x7800; 0x6000; 0x7000 ])
     = Msx.Ascii8_sram Koei);
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
  Msx.load_cartridge ~mapper:(Msx.Ascii8_sram Koei) t (synth 32);
  show_cart t;
  (* register 0x7000 selects window 2 (0x8000); a plain segment reads ROM *)
  Msx.mem_write t 0x7000 5;
  check "koei rom bank reads its segment" (win t 0x8000 = 5);
  (* bank 0x20 maps SRAM into the window; a store there is read back *)
  Msx.mem_write t 0x7000 0x20;
  Msx.mem_write t 0x8000 0xab;
  check "koei sram store is read back" (win t 0x8000 = 0xab);
  check "koei sram starts blank (0xFF)" (win t 0x8001 = 0xff);
  (* back to a ROM bank shows ROM again; the SRAM keeps its byte *)
  Msx.mem_write t 0x7000 5;
  check "koei rom again after sram" (win t 0x8000 = 5);
  Msx.mem_write t 0x7000 0x20;
  check "koei sram retained across bank switch" (win t 0x8000 = 0xab)

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
   select. The SRAM bit is the ROM's block count: 24 segments -> 0x18,
   so bank 8 selects SRAM. *)
let koei_windows () =
  let t = machine () in
  Msx.load_cartridge ~mapper:(Msx.Ascii8_sram Koei) t (synth 32);
  show_cart t;
  Msx.mem_write t 0x6000 0x20;
  Msx.mem_write t 0x4000 0xcd;
  check "koei window 0 stores into sram" (win t 0x4000 = 0xcd);
  Msx.mem_write t 0x6800 0x21;
  check "koei window 1 reads sram page 1" (win t 0x6000 = 0xff);
  Msx.mem_write t 0x6800 0x20;
  check "koei window 1 reads sram page 0" (win t 0x6000 = 0xcd);
  let t = machine () in
  Msx.load_cartridge ~mapper:(Msx.Ascii8_sram Koei) t (synth 24);
  show_cart t;
  Msx.mem_write t 0x7000 8;
  Msx.mem_write t 0x8000 0x5a;
  check "koei 24 segments: bank 8 has the sram bit" (win t 0x8000 = 0x5a);
  Msx.mem_write t 0x7000 7;
  check "koei 24 segments: bank 7 is rom" (win t 0x8000 = 7)

(* openMSX RomAscii8_8 ASCII8_2 / ASCII8_8 / WIZARDRY (sramPages 0x30): any
   window whose bank has the SRAM bit shows the SRAM, but only windows 2 and
   3 (0x8000-0xBFFF) take stores. A 2KB SRAM repeats through the 8KB window.
   16 segments make the SRAM bit 0x10; Wizardry's is 0x80 whatever the size. *)
let ascii8_sram () =
  let t = machine () in
  Msx.load_cartridge ~mapper:(Msx.Ascii8_sram Ascii8_2kb) t (synth 16);
  show_cart t;
  Msx.mem_write t 0x7000 0x10;
  check "ascii8sram2 starts blank" (win t 0x8000 = 0xff);
  Msx.mem_write t 0x8000 0x5a;
  check "ascii8sram2 window 2 stores" (win t 0x8000 = 0x5a);
  check "ascii8sram2 repeats every 2KB" (win t 0x8800 = 0x5a && win t 0x9800 = 0x5a);
  Msx.mem_write t 0x7800 0x13;
  check "ascii8sram2 has one page" (win t 0xa000 = 0x5a);
  Msx.mem_write t 0xa001 0x77;
  check "ascii8sram2 window 3 stores" (win t 0x8001 = 0x77);
  Msx.mem_write t 0x6000 0x10;
  check "ascii8sram2 window 0 shows sram" (win t 0x4000 = 0x5a);
  Msx.mem_write t 0x4000 0x11;
  check "ascii8sram2 window 0 is read-only" (win t 0x4000 = 0x5a);
  Msx.mem_write t 0x7000 3;
  check "ascii8sram2 rom bank again" (win t 0x8000 = 3);
  let t = machine () in
  Msx.load_cartridge ~mapper:(Msx.Ascii8_sram Ascii8_8kb) t (synth 16);
  show_cart t;
  Msx.mem_write t 0x7000 0x10;
  Msx.mem_write t 0x8000 0x11;
  Msx.mem_write t 0x8800 0x22;
  check "ascii8sram8 holds 8KB" (win t 0x8000 = 0x11 && win t 0x8800 = 0x22);
  Msx.mem_write t 0x7800 0x17;
  check "ascii8sram8 has one page" (win t 0xa800 = 0x22);
  let t = machine () in
  Msx.load_cartridge ~mapper:(Msx.Ascii8_sram Wizardry) t (synth 16);
  show_cart t;
  Msx.mem_write t 0x7000 0x10;
  check "wizardry 0x10 is a rom bank" (win t 0x8000 = 0);
  Msx.mem_write t 0x7000 0x80;
  Msx.mem_write t 0x8000 0x42;
  check "wizardry 0x80 selects sram" (win t 0x8000 = 0x42);
  Msx.mem_write t 0x8800 0x43;
  check "wizardry holds 8KB" (win t 0x8000 = 0x42 && win t 0x8800 = 0x43)

(* openMSX RomAscii16_2: the bank value 0x10 maps the SRAM into a 16KB page,
   repeated through it, even when the ROM has a block 16. The page at 0x8000
   takes stores; the one at 0x4000 is read-only. *)
let ascii16_sram () =
  let t = machine () in
  Msx.load_cartridge ~mapper:(Msx.Ascii16_sram Ascii16_2kb) t (synth 16);
  show_cart t;
  Msx.mem_write t 0x7000 0x10;
  check "ascii16sram2 starts blank" (win t 0x8000 = 0xff);
  Msx.mem_write t 0x8000 0x33;
  check "ascii16sram2 page 2 stores" (win t 0x8000 = 0x33);
  check "ascii16sram2 repeats every 2KB" (win t 0x8800 = 0x33 && win t 0xb800 = 0x33);
  Msx.mem_write t 0x6000 0x10;
  check "ascii16sram2 page 1 shows sram" (win t 0x4000 = 0x33);
  Msx.mem_write t 0x4000 0x44;
  check "ascii16sram2 page 1 is read-only" (win t 0x4000 = 0x33);
  Msx.mem_write t 0x7000 2;
  check "ascii16sram2 rom bank again" (win t 0x8000 = 4);
  let t = machine () in
  Msx.load_cartridge ~mapper:(Msx.Ascii16_sram Ascii16_8kb) t (synth 40);
  show_cart t;
  Msx.mem_write t 0x7000 0x10;
  check "ascii16sram8: 0x10 is sram although block 16 exists" (win t 0x8000 = 0xff);
  Msx.mem_write t 0x9fff 0x66;
  check "ascii16sram8 holds 8KB"
    (win t 0x9fff = 0x66 && win t 0xbfff = 0x66 && win t 0x8000 = 0xff);
  Msx.mem_write t 0x7000 0x11;
  check "ascii16sram8 0x11 is rom block 17" (win t 0x8000 = 34)

(* openMSX RomRType: block 0x17 stays at 0x4000, and a write anywhere in
   0x4000-0x7FFF picks the block at 0x8000. Bit 4 selects the second ROM
   chip, which keeps bits 0-2 (mask 0x17); the first keeps bits 0-3 (0x1F).
   A 512KB ROM (32 blocks, wrap mask 31) shows the 0x17 mask, a 1MB one
   the 0x1F mask. *)
let r_type () =
  let t = machine () in
  Msx.load_cartridge ~mapper:Msx.R_type t (synth 64);
  show_cart t;
  check "r-type 0x4000 is block 0x17" (win t 0x4000 = 46 && win t 0x6000 = 47);
  check "r-type 0x8000 starts at block 0" (win t 0x8000 = 0 && win t 0xa000 = 1);
  Msx.mem_write t 0x7000 5;
  check "r-type 0x7000 picks block 5" (win t 0x8000 = 10);
  Msx.mem_write t 0x4123 7;
  check "r-type any write below 0x8000 selects" (win t 0x8000 = 14);
  Msx.mem_write t 0x7800 0x18;
  check "r-type chip 2 keeps bits 0-2" (win t 0x8000 = 32);
  Msx.mem_write t 0x8000 3;
  Msx.mem_write t 0xa000 3;
  check "r-type ignores writes from 0x8000" (win t 0x8000 = 32);
  check "r-type 0x4000 stays fixed" (win t 0x4000 = 46);
  let t = machine () in
  Msx.load_cartridge ~mapper:Msx.R_type t (synth 128);
  show_cart t;
  Msx.mem_write t 0x7000 0x2f;
  check "r-type chip 1 keeps bits 0-3" (win t 0x8000 = 30)

(* openMSX RomHarryFox: bit 0 of a write to 0x6xxx picks block 0 or 2 for
   0x4000, of a write to 0x7xxx block 1 or 3 for 0x8000. The ROM here has 8
   blocks, so a value used whole would reach blocks 4-7 instead of wrapping
   onto 0-3. *)
let harry_fox () =
  let t = machine () in
  Msx.load_cartridge ~mapper:Msx.Harry_fox t (synth 16);
  show_cart t;
  check "harry fox starts at blocks 0 and 1" (win t 0x4000 = 0 && win t 0x8000 = 2);
  Msx.mem_write t 0x6fff 1;
  check "harry fox 0x6xxx odd = block 2" (win t 0x4000 = 4 && win t 0x6000 = 5);
  Msx.mem_write t 0x7abc 0xfd;
  check "harry fox 0x7xxx odd = block 3" (win t 0x8000 = 6 && win t 0xa000 = 7);
  Msx.mem_write t 0x7000 2;
  check "harry fox 0x7xxx even = block 1" (win t 0x8000 = 2);
  Msx.mem_write t 0x6000 2;
  check "harry fox 0x6xxx even = block 0" (win t 0x4000 = 0);
  Msx.mem_write t 0x5000 1;
  Msx.mem_write t 0x8000 1;
  check "harry fox ignores other writes" (win t 0x4000 = 0 && win t 0x8000 = 2)

(* Slot 2 in all four pages: PPI port A = 0b10101010. *)
let cart_everywhere t = Msx.port_out t 0xA8 0xAA

(* openMSX RomCrossBlaim: any write in the slot switches, wherever it lands.
   Values 0 and 1 show block 1 at 0x8000 and mirror it into pages 0 and 3;
   2 and 3 show blocks 2 and 3 and leave pages 0 and 3 unmapped. Block 0
   stays at 0x4000. The ROM has 4 blocks. *)
let cross_blaim () =
  let t = machine () in
  Msx.load_cartridge ~mapper:Msx.Cross_blaim t (synth 8);
  cart_everywhere t;
  check "cross blaim starts at blocks 0 and 1" (win t 0x4000 = 0 && win t 0x8000 = 2);
  check "cross blaim mirrors block 1 into pages 0 and 3"
    (win t 0x0000 = 2 && win t 0x2000 = 3 && win t 0xc000 = 2 && win t 0xe000 = 3);
  Msx.mem_write t 0x4045 2;
  check "cross blaim 2 picks block 2" (win t 0x8000 = 4 && win t 0xa000 = 5);
  check "cross blaim 2 unmaps pages 0 and 3" (win t 0x0000 = 0xff && win t 0xc000 = 0xff);
  Msx.mem_write t 0xa123 7;
  check "cross blaim keeps the low two bits" (win t 0x8000 = 6);
  Msx.mem_write t 0x0000 0;
  check "cross blaim page 0 write, 0 picks block 1" (win t 0x8000 = 2 && win t 0x0000 = 2);
  Msx.mem_write t 0xc000 2;
  check "cross blaim page 3 write selects" (win t 0x8000 = 4);
  check "cross blaim 0x4000 stays block 0" (win t 0x4000 = 0 && win t 0x6000 = 1)

(* openMSX RomWonderKid: block 0 at 0x0000 and 0x4000, a write to
   0x8000-0xBFFF picks the block there (reset: block 2), nothing at 0xC000.
   The ROM has 8 blocks, so block 9 wraps to 1. *)
let wonder_kid () =
  let t = machine () in
  Msx.load_cartridge ~mapper:Msx.Wonder_kid t (synth 16);
  cart_everywhere t;
  check "wonder kid starts at blocks 0 and 2" (win t 0x4000 = 0 && win t 0x8000 = 4);
  check "wonder kid shows block 0 at 0x0000" (win t 0x2000 = 1);
  check "wonder kid leaves 0xC000 unmapped" (win t 0xc000 = 0xff);
  Msx.mem_write t 0x8000 5;
  check "wonder kid 0x8000 picks block 5" (win t 0x8000 = 10 && win t 0xa000 = 11);
  Msx.mem_write t 0xbfff 3;
  check "wonder kid 0xBFFF picks block 3" (win t 0x8000 = 6);
  Msx.mem_write t 0x4000 1;
  Msx.mem_write t 0x7fff 1;
  Msx.mem_write t 0xc000 1;
  Msx.mem_write t 0x0000 1;
  check "wonder kid ignores writes outside 0x8000-0xBFFF" (win t 0x8000 = 6 && win t 0x4000 = 0);
  Msx.mem_write t 0x9000 9;
  check "wonder kid block 9 of 8 wraps to 1" (win t 0x8000 = 2)

(* openMSX RomSuperLodeRunner: ROM only at 0x8000-0xBFFF, block 0 at reset.
   The cart switches on a write to 0x0000 whatever slot page 0 shows (here
   slot 0), not on writes to its own window. *)
let super_lode_runner () =
  let t = machine () in
  Msx.load_cartridge ~mapper:Msx.Super_lode_runner t (synth 8);
  show_cart t;
  check "super lode runner 0x4000-0x7FFF unmapped" (win t 0x4000 = 0xff && win t 0x6000 = 0xff);
  check "super lode runner starts at block 0" (win t 0x8000 = 0 && win t 0xa000 = 1);
  Msx.mem_write t 0x8000 2;
  Msx.mem_write t 0x4000 2;
  Msx.mem_write t 0x0001 2;
  check "super lode runner ignores other writes" (win t 0x8000 = 0);
  Msx.mem_write t 0x0000 3;
  check "super lode runner 0x0000 in slot 0 picks block 3" (win t 0x8000 = 6 && win t 0xa000 = 7);
  cart_everywhere t;
  check "super lode runner pages 0 and 3 unmapped" (win t 0x0000 = 0xff && win t 0xc000 = 0xff)

(* openMSX RomGeneric8kB / RomGeneric16kB: a write anywhere in a window
   selects that window's bank; the banks start at 0-3 (8kB) and 0,1
   (16kB). *)
let generic () =
  let t = machine () in
  Msx.load_cartridge ~mapper:Msx.Generic8 t (synth 8);
  show_cart t;
  List.iteri
    (fun i a -> check (Printf.sprintf "generic8 reset 0x%04X=seg%d" a i) (win t a = i))
    [ 0x4000; 0x6000; 0x8000; 0xA000 ];
  List.iter
    (fun (reg, seg, a) ->
      Msx.mem_write t reg seg;
      check (Printf.sprintf "generic8 write 0x%04X selects seg%d" reg seg) (win t a = seg))
    [ (0x4000, 5, 0x4000); (0x7fff, 6, 0x6000); (0x9234, 7, 0x8000); (0xa001, 4, 0xa000) ];
  check "generic8 window 0 kept" (win t 0x4000 = 5);
  let t = machine () in
  Msx.load_cartridge ~mapper:Msx.Generic16 t (synth 8);
  show_cart t;
  List.iteri
    (fun i a -> check (Printf.sprintf "generic16 reset 0x%04X=seg%d" a i) (win t a = i))
    [ 0x4000; 0x6000; 0x8000; 0xA000 ];
  Msx.mem_write t 0x5555 3;
  check "generic16 0x4000-0x7FFF write selects bank 3 low" (win t 0x4000 = 6);
  check "generic16 bank 3 high half" (win t 0x6000 = 7);
  Msx.mem_write t 0xbfff 2;
  check "generic16 0x8000-0xBFFF write selects bank 2" (win t 0x8000 = 4 && win t 0xA000 = 5);
  check "generic16 5 of 4 banks -> 1" (Msx.mem_write t 0x8000 5; win t 0x8000 = 2)

let () =
  r_type ();
  harry_fox ();
  cross_blaim ();
  wonder_kid ();
  super_lode_runner ();
  ascii8_sram ();
  ascii16_sram ();
  generic ();
  konami ();
  out_of_range ();
  koei_windows ();
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
