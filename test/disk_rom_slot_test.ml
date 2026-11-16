(* The real disk interface ROM (load_disk ~real_rom:true) sits in slot 3-3 in
   its own buffer, so a cartridge in slot 2 stays plugged while a disk boots,
   as on an NMS 8250 with a Konami SCC cart and a game disk. Before, the ROM
   took the cart buffer and slot 2 read as an empty bus. *)

let check name cond = if not cond then failwith name

let rom_size = 0x4000
let rom_marker_at = 0x0100
let rom_marker = 0x5a

(* A 16KB interface ROM with an "AB" header, given to load_disk via DISK_ROM. *)
let interface_rom =
  let b = Bytes.make rom_size '\xc9' in
  Bytes.blit_string "AB\x10\x40" 0 b 0 4;
  Bytes.set b rom_marker_at (Char.chr rom_marker);
  Bytes.to_string b

let () =
  let path = Filename.temp_file "disk_rom_slot" ".rom" in
  let oc = open_out_bin path in
  output_string oc interface_rom;
  close_out oc;
  Unix.putenv "DISK_ROM" path

let machine () = Msx.create ~machine:{ ram_kb = 64; vram_kb = 128; roms = [ ""; ""; "" ] }
let disk = String.make 512 '\000'

(* A 32KB plain cart: page 1 and page 2 markers, and known bytes where the
   FDC window would sit (0x7FF8-0x7FFF). *)
let plain_cart =
  let b = Bytes.make 0x8000 '\000' in
  Bytes.blit_string "AB" 0 b 0 2;
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
  Msx.load_disk ~real_rom:true t disk;
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
  Msx.load_disk ~real_rom:true k disk;
  check "the cart keeps its mapper" (Msx.cart_mapper k = Msx.Konami);
  view_slot2 k;
  Msx.mem_write k 0x6000 5;
  check "slot 2 bank switching still works" (Msx.mem_read k 0x6000 = 5);
  view_slot33 k;
  check "slot 3-3 page 1 is still the interface ROM" (Msx.mem_read k (0x4000 + rom_marker_at) = rom_marker);

  (* No cart: the cartridge slot is an empty bus, as before. *)
  let e = machine () in
  Msx.load_disk ~real_rom:true e disk;
  Msx.port_out e 0xa8 0xea;
  check "empty slot 2 page 0" (Msx.mem_read e 0x0000 = 0xff);
  check "empty slot 2 page 1" (Msx.mem_read e 0x4000 = 0xff);
  check "empty slot 2 page 2" (Msx.mem_read e 0x8000 = 0xff);
  check "no FDC window in empty slot 2" (Msx.mem_read e 0x7ffc = 0xff);

  (* Format 3 kept the interface ROM in the cart field with a flag of 1 where
     format 4 writes the ROM. Build that payload from the v4 one. *)
  let s4 = Msx.serialize e in
  let head = String.length "OCAML-MSX\000\004" + 16 in
  let p4 = String.sub s4 head (String.length s4 - head) in
  let be64 n = let b = Bytes.create 8 in Bytes.set_int64_be b 0 (Int64.of_int n); Bytes.to_string b in
  let rom_field = be64 rom_size ^ interface_rom in
  let find s sub =
    let n = String.length sub in
    let rec go i = if i + n > String.length s then -1 else if String.sub s i n = sub then i else go (i + 1) in
    go 0
  in
  let rom_at = find p4 rom_field in
  check "v4 payload holds the interface ROM once" (rom_at > 0 && find (String.sub p4 (rom_at + 1) (String.length p4 - rom_at - 1)) rom_field < 0);
  (* media list: RAM, three system ROMs, then the cart (empty here); each is
     a 64-bit length and the bytes. *)
  let skip at = at + 8 + Int64.to_int (String.get_int64_be p4 at) in
  let cart_at = skip (skip (skip (skip 0))) in
  check "v4 cart field is empty" (String.sub p4 cart_at 8 = be64 0);
  let p3 =
    String.sub p4 0 cart_at ^ rom_field
    ^ String.sub p4 (cart_at + 8) (rom_at - cart_at - 8)
    ^ be64 1
    ^ String.sub p4 (rom_at + String.length rom_field) (String.length p4 - rom_at - String.length rom_field)
  in
  let s3 = "OCAML-MSX\000\003" ^ Digest.string p3 ^ p3 in
  (match Msx.restore ~state:s3 with
   | Error err -> failwith ("v3 restore: " ^ err)
   | Ok r ->
     check "v3 real-ROM save reads as the v4 machine" (Msx.serialize r = s4);
     Msx.port_out r 0xa8 0xea;
     check "v3 save: slot 2 is empty" (Msx.mem_read r 0x4000 = 0xff));

  (* A v4 interface ROM must be 16KB. *)
  let after_rom = String.sub p4 (rom_at + String.length rom_field) (String.length p4 - rom_at - String.length rom_field) in
  let bad = String.sub p4 0 rom_at ^ be64 5 ^ "abcde" ^ after_rom in
  (match Msx.restore ~state:("OCAML-MSX\000\004" ^ Digest.string bad ^ bad) with
   | Error err -> check "bad ROM size named" (err = "invalid disk interface ROM size")
   | Ok _ -> failwith "a 5-byte interface ROM must be rejected");

  (* A v3 flag of 1 needs the 16KB interface ROM in the cart field; here
     the cart field is empty. *)
  let bad3 = String.sub p4 0 rom_at ^ be64 1 ^ after_rom in
  (match Msx.restore ~state:("OCAML-MSX\000\003" ^ Digest.string bad3 ^ bad3) with
   | Error err -> check "v3 flag without a 16KB cart named" (err = "invalid disk interface ROM size")
   | Ok _ -> failwith "a v3 real-ROM flag over an empty cart must be rejected");

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
  if real_rom then Msx.load_disk ~real_rom:true t disk
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
