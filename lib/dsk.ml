(* A .dsk floppy image: the byte-level thing DSKIO reads from and writes to.

   The shape parsed here is the raw one: the sectors alone, 512 bytes each
   in logical order, so the file is exactly tracks*2*9*512 bytes and its
   first byte is the boot sector's first byte. The CPC-family headered
   shapes are refused with [Bad] until their own geometry handling lands.

   Sectors are addressed the way DSKIO does: logical sector numbers from 0,
   nine a head, two heads a track. The image stores them in that order, so
   the file offset is the number times 512 -- the physical interleave the
   drive firmware once walked is not in the file. *)

type t = {
  bytes : Bytes.t;
  mutable tracks : int;  (** cylinders, 2 heads each *)
  sector_bytes : int;
}

exception Bad of string

let bytes_per_sector = 512
let sectors_per_track = 9
let heads = 2

let sectors_per_cylinder = sectors_per_track * heads

(* Sector sizes a track header can name: 128 << the exponent nibble. *)
let named_sector_size code =
  if code > 0 && code <= 6 then Some (128 lsl (code - 1)) else None

let create ~tracks =
  if tracks <= 0 then raise (Bad "a disk needs a track");
  {
    bytes = Bytes.make (tracks * sectors_per_cylinder * bytes_per_sector) '\000';
    tracks;
    sector_bytes = bytes_per_sector;
  }

let total_sectors t = t.tracks * sectors_per_cylinder

(* Raw iff the file is exactly a whole number of cylinders. The extended
   formats open with a track header rather than a jump instruction, so a file
   whose first two bytes are a Z80 jump is not one of them. *)
let looks_raw len = len mod (sectors_per_cylinder * bytes_per_sector) = 0

let parse (data : string) : t =
  let len = String.length data in
  if len = 0 then raise (Bad "empty image");
  if looks_raw len then
    { bytes = Bytes.of_string data
    ; tracks = len / (sectors_per_cylinder * bytes_per_sector)
    ; sector_bytes = bytes_per_sector
    }
  else
    (* A headered image (CPC-family: the file opens with "MV - CPC" or
       "EXTENDED" and a 0x100 file header). Not parsed yet -- the header
       names its own geometry and the extended flavour interleaves per-sector
       ID blocks, so refusing beats guessing. MSX disk images in the wild are
       overwhelmingly raw. *)
    raise
      (Bad
         "headered (CPC-family) image: only raw sector-order .dsk is parsed")

let read_sector t logical =
  if logical < 0 || logical >= total_sectors t then None
  else begin
    let off = logical * t.sector_bytes in
    Some (Bytes.sub_string t.bytes off t.sector_bytes)
  end

let write_sector t logical (data : string) =
  if logical < 0 || logical >= total_sectors t then false
  else begin
    let off = logical * t.sector_bytes in
    let n = min (String.length data) t.sector_bytes in
    Bytes.blit_string data 0 t.bytes off n;
    true
  end
