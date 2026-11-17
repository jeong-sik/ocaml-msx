(* Panasoft SW-M001 PAC: a cartridge with 8190 bytes of battery-backed SRAM
   that games use to keep their saves. The model follows openMSX
   src/memory/MSXPac.cc and share/extensions/pac.xml.

   - The cart answers in page 1 (0x4000-0x7FFF) of its slot; [read] and
     [write] take the address inside that page.
   - 0x5FFE and 0x5FFF are registers. Writing 0x4D and 0x69 to them opens
     the SRAM; any other pair closes it. Power-up leaves both at 0xFF.
   - While open, 0x4000-0x5FFD is the SRAM, 0x5FFE-0x5FFF read the
     registers back and 0x6000-0x7FFF reads 0xFF. While closed, everything
     reads 0xFF and stores into the SRAM are dropped.
   - A new SRAM is 0xFF throughout.

   The save file is openMSX's: the 16-byte header "PAC2 BACKUP DATA", then
   the SRAM. *)

let sram_size = 0x1ffe
let reg_a = 0x1ffe
let reg_b = 0x1fff
let open_a = 0x4d
let open_b = 0x69
let page_mask = 0x3fff
let blank = '\xff'
let header = "PAC2 BACKUP DATA"
let file_size = String.length header + sram_size

type t = { sram : Bytes.t; mutable a : int; mutable b : int }

let create () = { sram = Bytes.make sram_size blank; a = 0xff; b = 0xff }
let is_open t = t.a = open_a && t.b = open_b

let read t addr =
  let off = addr land page_mask in
  if not (is_open t) then 0xff
  else if off < sram_size then Char.code (Bytes.get t.sram off)
  else if off = reg_a then t.a
  else if off = reg_b then t.b
  else 0xff

let write t addr v =
  let off = addr land page_mask in
  let v = v land 0xff in
  if off = reg_a then t.a <- v
  else if off = reg_b then t.b <- v
  else if off < sram_size && is_open t then Bytes.set t.sram off (Char.chr v)

let of_file s =
  if String.length s <> file_size then
    Error (Printf.sprintf "PAC file is %d bytes, expected %d" (String.length s) file_size)
  else if String.sub s 0 (String.length header) <> header then
    Error "PAC file does not start with \"PAC2 BACKUP DATA\""
  else
    Ok { (create ()) with sram = Bytes.of_string (String.sub s (String.length header) sram_size) }

let to_file t = header ^ Bytes.to_string t.sram

let write_state w t =
  State_codec.put_bytes w t.sram;
  State_codec.put_int w t.a;
  State_codec.put_int w t.b

let read_state r =
  let sram = State_codec.get_bytes r in
  if Bytes.length sram <> sram_size then State_codec.fail "invalid PAC SRAM size";
  let a = State_codec.get_int r ~min:0 ~max:255 in
  let b = State_codec.get_int r ~min:0 ~max:255 in
  { sram; a; b }
