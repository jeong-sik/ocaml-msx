(* The real disk interface ROM (load_disk ~disk_rom) sits in slot 3-3 in
   its own buffer, so a cartridge in slot 2 stays plugged while a disk boots,
   as on an NMS 8250 with a Konami SCC cart and a game disk. Before, the ROM
   took the cart buffer and slot 2 read as an empty bus. *)

let check name cond = if not cond then failwith name

let rom_size = 0x4000
let rom_marker_at = 0x0100
let rom_marker = 0x5a

(* A 16KB interface ROM with an "AB" header, supplied to load_disk. *)
let interface_rom =
  let b = Bytes.make rom_size '\xc9' in
  Bytes.blit_string "AB\x10\x40" 0 b 0 4;
  Bytes.set b rom_marker_at (Char.chr rom_marker);
  Bytes.to_string b

let disk_rom =
  match Msx.disk_rom_of_string interface_rom with
  | Ok rom -> rom
  | Error e -> failwith e

let machine () = Msx.create ~machine:{ ram_kb = 64; vram_kb = 128; roms = [ ""; ""; "" ] }
let disk = String.make 512 '\000'

(* A 32KB plain cart: page 1 and page 2 markers, and known bytes where the
   FDC window would sit (0x7FF8-0x7FFF). *)
let plain_cart =
  let b = Bytes.make 0x8000 '\000' in
  Bytes.blit_string "AB\x10\x41" 0 b 0 4;
  Bytes.set b 0x0100 '\xa5';
  Bytes.set b 0x4100 '\x3c';
  for k = 0 to 7 do Bytes.set b (0x3ff8 + k) (Char.chr (0x11 + k)) done;
  Bytes.to_string b

(* A 128KB Konami MegaROM: each 8KB segment starts with its own number. *)
let konami_cart =
  String.init 0x20000 (fun i -> if i land 0x1fff = 0 then Char.chr (i lsr 13) else '\000')

(* Slot views: page 1 and 2 on slot 2, or page 1 on slot 3-3. Page 3 stays on
   slot 3 so 0xFFFF reaches the subslot register. *)
let view_slot2 t = Msx.port_out t 0xa8 0xe8
let view_slot33 t =
  Msx.port_out t 0xa8 0xcc;
  Msx.mem_write t 0xffff 0x0c

let side_reg = 0x7ffc

let () =
  (* Cart first, then the disk with the real interface ROM. *)
  let t = machine () in
  Msx.load_cartridge t plain_cart;
  Msx.load_disk ~disk_rom t disk;
  view_slot2 t;
  check "slot 2 page 1 is the cart" (Msx.mem_read t 0x4100 = 0xa5);
  check "slot 2 page 2 is the cart's second half" (Msx.mem_read t 0x8100 = 0x3c);
  check "slot 2 shows cart bytes where the FDC window would be" (Msx.mem_read t 0x7ff8 = 0x11);
  view_slot33 t;
  check "slot 3-3 page 1 is the interface ROM" (Msx.mem_read t (0x4000 + rom_marker_at) = rom_marker);
  Msx.mem_write t side_reg 1;
  check "the FDC window lives in slot 3-3" (Msx.mem_read t side_reg = 1);
  view_slot2 t;
  Msx.mem_write t side_reg 0;
  view_slot33 t;
  check "a slot 2 write does not reach the FDC" (Msx.mem_read t side_reg = 1);

  (* Save and restore keep both the cart and the interface ROM. *)
  let s = Msx.serialize t in
  (match Msx.restore ~state:s with
   | Error e -> failwith ("restore: " ^ e)
   | Ok r ->
     check "round trip is stable" (Msx.serialize r = s);
     view_slot2 r;
     check "restored slot 2 is the cart" (Msx.mem_read r 0x4100 = 0xa5);
     view_slot33 r;
     check "restored slot 3-3 is the interface ROM" (Msx.mem_read r (0x4000 + rom_marker_at) = rom_marker));

  (* A MegaROM keeps its mapper next to the interface ROM. *)
  let k = machine () in
  Msx.load_cartridge ~mapper:Msx.Konami k konami_cart;
  Msx.load_disk ~disk_rom k disk;
  check "the cart keeps its mapper" (Msx.cart_mapper k = Msx.Konami);
  view_slot2 k;
  Msx.mem_write k 0x6000 5;
  check "slot 2 bank switching still works" (Msx.mem_read k 0x6000 = 5);
  view_slot33 k;
  check "slot 3-3 page 1 is still the interface ROM" (Msx.mem_read k (0x4000 + rom_marker_at) = rom_marker);

  (* No cart: the cartridge slot is an empty bus, as before. *)
  let e = machine () in
  Msx.load_disk ~disk_rom e disk;
  Msx.port_out e 0xa8 0xea;
  check "empty slot 2 page 0" (Msx.mem_read e 0x0000 = 0xff);
  check "empty slot 2 page 1" (Msx.mem_read e 0x4000 = 0xff);
  check "empty slot 2 page 2" (Msx.mem_read e 0x8000 = 0xff);
  check "no FDC window in empty slot 2" (Msx.mem_read e 0x7ffc = 0xff);

  (* The firmware is visible only in page 1; the FDC registers span all pages. *)
  Msx.port_out e 0xa8 0xff;
  Msx.mem_write e 0xffff 0xff;
  check "disk ROM is absent from page 0" (Msx.mem_read e 0x0100 = 0xff);
  check "disk ROM is absent from page 2" (Msx.mem_read e 0x8100 = 0xff);
  check "disk ROM is absent from page 3" (Msx.mem_read e 0xc100 = 0xff);

  let state = Msx.serialize e in
  let header = "OCAML-MSX\000\010" in
  let offset = String.length header + 16 in
  let payload_end = String.length state - 8 - rom_size in
  let prefix = String.sub state offset (payload_end - offset) in
  let bad_len = Bytes.create 8 in
  Bytes.set_int64_be bad_len 0 5L;
  let bad = prefix ^ Bytes.to_string bad_len ^ "abcde" in
  (match Msx.restore ~state:(header ^ Digest.string bad ^ bad) with
   | Error e -> check "invalid disk ROM size is named" (e = "invalid disk interface ROM size")
   | Ok _ -> failwith "a five-byte disk ROM restored");

  (* boot_disk mounts the HLE interface ROM as the cart only when no real
     interface ROM is present. *)
  (match Msx.boot_disk e with
   | Error err -> failwith ("boot_disk: " ^ err)
   | Ok () ->
     Msx.port_out e 0xa8 0xea;
     check "boot_disk leaves slot 2 empty next to the real ROM" (Msx.mem_read e 0x4000 = 0xff);
     view_slot33 e;
     check "boot_disk keeps the real ROM in 3-3" (Msx.mem_read e (0x4000 + rom_marker_at) = rom_marker))

(* With the real interface ROM in 3-3, its own code serves the disk BIOS,
   so the HLE traps stay off, also after a save and restore. The probe is a
   plain CALL 0x009C (CHSNS) from page-0 RAM, a vector the HLE trap serves
   by running main ROM's stub, which writes 0x42 to 0xC100. Without the
   trap the CALL runs RAM power-on bytes (00 FF: NOP, RST 38h) and halts on
   the HALT planted at 0x0038; 0xC100 keeps its power-on 0xFF. *)
let chsns_marker = 0xc100

let probe_machine ~real_rom =
  let main = Bytes.make 0x4000 '\000' in
  (* vector 0x009C: JP 0x0020; 0x0020: LD A,0x42 / LD (0xC100),A / RET *)
  Bytes.blit_string "\xc3\x20\x00" 0 main 0x9c 3;
  Bytes.blit_string "\x3e\x42\x32\x00\xc1\xc9" 0 main 0x20 6;
  let t = Msx.create ~machine:{ ram_kb = 64; vram_kb = 128; roms = [ Bytes.to_string main ] } in
  if real_rom then Msx.load_disk ~disk_rom t disk
  else Msx.load_disk ~interface_rom:false t disk;
  (* page 0 and 3 on slot 3-2 (RAM), as a disk kernel runs *)
  Msx.port_out t 0xa8 0xc3;
  Msx.mem_write t 0xffff 0xaa;
  (* LD SP,0xFFF0 / CALL 0x009C / HALT *)
  List.iteri (fun i b -> Msx.mem_write t i b) [ 0x31; 0xf0; 0xff; 0xcd; 0x9c; 0x00; 0x76 ];
  Msx.mem_write t 0x38 0x76;
  t

let run_probe t =
  Msx.step t ~frames:1;
  Msx.mem_read t chsns_marker

let () =
  check "HLE disk: the trap serves CHSNS" (run_probe (probe_machine ~real_rom:false) = 0x42);
  check "real ROM: no HLE trap" (run_probe (probe_machine ~real_rom:true) = 0xff);
  (match Msx.restore ~state:(Msx.serialize (probe_machine ~real_rom:true)) with
   | Error e -> failwith ("probe restore: " ^ e)
   | Ok r -> check "real ROM after restore: no HLE trap" (run_probe r = 0xff));
  print_endline "disk rom slot: ok"
