(* V9938 sprite engine. Table addressing follows openMSX VDP.cc
   updateSpriteAttributeBase / updateSpritePatternBase: a table address is
   [base land (index_mask lor index)], where [base] holds the register bits
   with the unused low bits set to one. In mode 2 the attribute table sits at
   index 0x200 inside a 1KB window and the per-line colour table at index 0,
   so R#5 bits 0-2 must be 1 or the tables mirror, exactly as on hardware.
   Planar modes (GRAPHIC 6/7) permute the logical address in the caller's
   [read]. *)

type mode = Mode1 | Mode2

type hit = { x : int; pattern : int; attr : int }

type line = { hits : hit array; overflow : int option; scanned : int }

let dots = 32
let sprite_count = 32
let dot_mask = 0xffff_ffff
let screen_width = 256
let early_clock_shift = 32
let attribute_entry = 4
let colour_rows = 16

let per_line_limit = function Mode1 -> 4 | Mode2 -> 8

(* A Y value that ends the attribute table for this frame line. *)
let terminator = function Mode1 -> 208 | Mode2 -> 216

let size regs = if regs.(1) land 0x02 <> 0 then 16 else 8
let magnified regs = regs.(1) land 0x01 <> 0
let span regs = if magnified regs then size regs * 2 else size regs
let opaque_colour0 regs = regs.(8) land 0x20 <> 0
let ones_above bits = lnot 0 lsl bits

let attribute_base regs =
  ((regs.(11) land 0x03) lsl 15) lor (regs.(5) lsl 7) lor 0x7f

let pattern_base regs = ((regs.(6) land 0x3f) lsl 11) lor 0x7ff

let attribute_addr mode regs sprite field =
  let offset = (sprite * attribute_entry) + field in
  match mode with
  | Mode1 -> attribute_base regs land (ones_above 7 lor offset)
  | Mode2 -> attribute_base regs land (ones_above 10 lor 0x200 lor offset)

let colour_addr regs sprite row =
  attribute_base regs land (ones_above 10 lor ((sprite * colour_rows) + row))

(* "abcd…p" in the top 16 bits becomes "aabbccdd…pp" (openMSX
   SpriteChecker::doublePattern). *)
let double_pattern a =
  let a = (a lor (a lsr 8)) land 0xff00ff00 in
  let a = (a lor (a lsr 4)) land 0xf0f0f0f0 in
  let a = (a lor (a lsr 2)) land 0xcccccccc in
  let a = (a lor (a lsr 1)) land 0xaaaaaaaa in
  a lor (a lsr 1)

(* A 16×16 pattern is four 8×8 blocks: top-left, bottom-left, top-right,
   bottom-right. Row r of the left half is byte r, of the right half r+16. *)
let pattern_bits regs ~read number row =
  let at index = read (pattern_base regs land (ones_above 11 lor index)) in
  let index = (number * 8) + row in
  let left = at index lsl 24 in
  let bits =
    if size regs = 16 then left lor (at (index + 16) lsl 16) else left
  in
  if magnified regs then double_pattern bits else bits

let evaluate mode ~regs ~read ~display_line =
  let span = span regs in
  let number_mask = if size regs = 16 then 0xfc else 0xff in
  let check_line = display_line - 1 + regs.(23) in
  let limit = per_line_limit mode in
  let hits = ref [] and count = ref 0 and overflow = ref None in
  let field sprite k = read (attribute_addr mode regs sprite k) in
  let take sprite sprite_line =
    let row = if magnified regs then sprite_line / 2 else sprite_line in
    let attr =
      match mode with
      | Mode1 -> field sprite 3
      | Mode2 -> read (colour_addr regs sprite row)
    in
    let pattern =
      pattern_bits regs ~read (field sprite 2 land number_mask) row
    in
    let x = field sprite 1 in
    let x = if attr land 0x80 <> 0 then x - early_clock_shift else x in
    hits := { x; pattern; attr } :: !hits;
    incr count
  in
  let rec scan sprite =
    if sprite = sprite_count then sprite
    else
      let y = field sprite 0 in
      if y = terminator mode then sprite
      else begin
        let sprite_line = (check_line - y) land 0xff in
        (if sprite_line < span then
           if !count < limit then take sprite sprite_line
           else if Option.is_none !overflow then overflow := Some sprite);
        scan (sprite + 1)
      end
  in
  let scanned = scan 0 in
  { hits = Array.of_list (List.rev !hits); overflow = !overflow; scanned }

let leading_zeros32 v =
  let rec go n bit = if v land bit <> 0 then n else go (n + 1) (bit lsr 1) in
  go 0 0x8000_0000

let may_collide mode regs hit =
  (opaque_colour0 regs || hit.attr land 0x0f <> 0)
  &&
  match mode with
  | Mode1 -> true
  | Mode2 -> hit.attr land 0x60 = 0

let collision mode ~regs line =
  let span = span regs in
  let hits = line.hits in
  let leftmost = ref max_int in
  for i = Array.length hits - 1 downto 1 do
    let a = hits.(i) in
    if may_collide mode regs a then
      for j = i - 1 downto 0 do
        let b = hits.(j) in
        let dist = b.x - a.x in
        if may_collide mode regs b && -span < dist && dist < span then begin
          let shifted =
            if dist < 0 then (b.pattern lsl -dist) land dot_mask
            else b.pattern lsr dist
          in
          let overlap = a.pattern land shifted in
          (* Only dots inside the screen collide, never the left border. *)
          let overlap =
            if a.x < 0 then overlap land ((1 lsl (dots + a.x)) - 1) else overlap
          in
          if overlap <> 0 then
            leftmost := min !leftmost (a.x + leading_zeros32 overlap)
        end
      done
  done;
  if !leftmost < screen_width then Some !leftmost else None

let dot_set pattern b = (pattern lsr (dots - 1 - b)) land 1 = 1

let draw mode ~regs line put =
  let hits = line.hits in
  let n = Array.length hits in
  let skip colour = colour = 0 && not (opaque_colour0 regs) in
  let each_dot hit f =
    for b = 0 to dots - 1 do
      let x = hit.x + b in
      if x >= 0 && x < screen_width && dot_set hit.pattern b then f x
    done
  in
  match mode with
  | Mode1 ->
      for i = n - 1 downto 0 do
        let hit = hits.(i) in
        let colour = hit.attr land 0x0f in
        if not (skip colour) then each_dot hit (fun x -> put x colour)
      done
  | Mode2 ->
      let cc hit = hit.attr land 0x40 <> 0 in
      let rec first_owner i = if i < n && cc hits.(i) then first_owner (i + 1) else i in
      (* Following CC=1 lines OR their dot colour into the owner's. *)
      let rec merge j x colour =
        if j < n && cc hits.(j) then
          let other = hits.(j) in
          let s = x - other.x in
          let colour =
            if s >= 0 && s < dots && dot_set other.pattern s then
              colour lor (other.attr land 0x0f)
            else colour
          in
          merge (j + 1) x colour
        else colour
      in
      for i = n - 1 downto first_owner 0 do
        let hit = hits.(i) in
        let colour = hit.attr land 0x0f in
        if not (skip colour) then
          each_dot hit (fun x -> put x (merge (i + 1) x colour))
      done
