(* Konami's SCC+ sound cartridge: an SCC-I chip and RAM, no ROM. Snatcher and
   SD Snatcher shipped with one. The model follows openMSX
   src/memory/MSXSCCPlusCart.cc.

   - RAM is 8KB blocks. A window's mapper register picks a block through its
     low 4 bits; a value the cartridge has no block for reads 0xFF and drops
     writes. Snatcher's cart answers 0-7, SD Snatcher's 8-15, the expanded
     cart (openMSX "scc+") all sixteen.
   - Four 8KB windows at 0x4000-0xBFFF. A write goes, in this order, to the
     mode register (0xBFFE-0xBFFF), to RAM when the window is in RAM mode, to
     the window's mapper register when [addr land 0x1800 = 0x1000], or to the
     SCC while its register window is open.
   - Mode register: bit 5 puts the SCC-I in Plus mode. Bit 4 makes all four
     windows RAM; otherwise bit 0 does window 0, bit 1 window 1, and bits 2
     and 5 together window 2. Window 3 holds the mode register and is never
     RAM by itself.
   - The SCC registers show at 0x9800-0x9FFF in Compatible mode while mapper
     register 2 has its low six bits set, and at 0xB800-0xBFFF in Plus mode
     while mapper register 3 has bit 7 set. *)

type kind = Snatcher | Sd_snatcher | Expanded

let block_size = 0x2000
let register_mask = 0x0f
let window_base = 0x4000
let window_end = 0xc000

let ram_size = function Snatcher | Sd_snatcher -> 0x10000 | Expanded -> 0x20000

(* The register value of the cartridge's first block. *)
let first_register = function Snatcher | Expanded -> 0 | Sd_snatcher -> 8

let block kind register =
  let b = (register land register_mask) - first_register kind in
  if b >= 0 && b < ram_size kind / block_size then Some b else None

let plus_mode mode = mode land 0x20 <> 0
let chip_mode mode : Scc.mode = if plus_mode mode then Plus else Compatible

let window a = (a - window_base) / block_size

let ram_window ~mode w =
  if mode land 0x10 <> 0 then true
  else
    match w with
    | 0 -> mode land 0x01 <> 0
    | 1 -> mode land 0x02 <> 0
    | 2 -> mode land 0x24 = 0x24
    | 3 -> false
    | _ -> invalid_arg "Scc_plus.ram_window: window out of range"

let scc_at ~mode ~banks a =
  if plus_mode mode then banks.(3) land 0x80 <> 0 && a >= 0xb800 && a < 0xc000
  else banks.(2) land 0x3f = 0x3f && a >= 0x9800 && a < 0xa000

let in_windows a = a >= window_base && a < window_end

let ram_offset kind ~banks a =
  Option.map (fun b -> (b * block_size) + (a land (block_size - 1))) (block kind banks.(window a))

let read kind ~ram ~banks ~mode scc a =
  if not (in_windows a) then 0xff
  else if scc_at ~mode ~banks a then Scc.read scc a
  else
    match ram_offset kind ~banks a with
    | Some off -> Char.code (Bytes.get ram off)
    | None -> 0xff

let write kind ~ram ~banks ~mode scc a v =
  let v = v land 0xff in
  if not (in_windows a) then mode
  else if a lor 1 = 0xbfff then begin
    Scc.set_mode scc (chip_mode v);
    v
  end
  else begin
    let w = window a in
    if ram_window ~mode w then
      (match ram_offset kind ~banks a with
       | Some off -> Bytes.set ram off (Char.chr v)
       | None -> ())
    else if a land 0x1800 = 0x1000 then banks.(w) <- v
    else if scc_at ~mode ~banks a then Scc.write scc a v;
    mode
  end
