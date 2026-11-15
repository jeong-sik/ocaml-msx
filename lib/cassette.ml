(* CAS playback, written from the behaviour of openMSX 21.0 CasImage.cc
   (MSX_CAS::convert) and CassettePlayer.cc; no code is taken from them.

   The wave is kept as a list of segments instead of one sample per byte:
   a 257 KB image would otherwise take 11 million samples. [level] computes
   the sample a segment would hold. *)

let baud = 3744
let samples_per_bit = 4
let sample_rate = baud * samples_per_bit
let bits_per_byte = 11 (* start bit 0, 8 data bits from bit 0 up, stop bits 1 1 *)
let samples_per_byte = bits_per_byte * samples_per_bit
let stop_bit_slot = 9

(* Before a file header block: 2 s of silence and 8000 1-bits; before the
   following data blocks: 1 s and 2000. *)
let long_silence = 2 * sample_rate
let short_silence = sample_rate
let long_pilot = 8000
let short_pilot = 2000

let block_header = "\x1F\xA6\xDE\xBA\xCC\x13\x7D\x74"
let id_length = 10
let ascii_id = String.make id_length '\xEA'
let binary_id = String.make id_length '\xD0'
let basic_id = String.make id_length '\xD3'
let end_of_file = '\x1A'

type file_type = Ascii | Binary | Basic | Unknown

type segment =
  | Silence of int  (** samples *)
  | Pilot of int  (** 1-bits *)
  | Data of int * int  (** first byte in the image, byte count *)

type tape = {
  cas : string;
  segments : segment array;
  starts : int array;  (** first sample of each segment *)
  total : int;
  first : file_type;
}

let segment_samples = function
  | Silence n -> n
  | Pilot n -> n * samples_per_bit
  | Data (_, n) -> n * samples_per_byte

(* The segments openMSX's converter writes, in order. *)
let convert cas =
  let n = String.length cas in
  let header_len = String.length block_header in
  let header_at p = p + header_len <= n && String.sub cas p header_len = block_header in
  let out = ref [] in
  let emit s = out := s :: !out in
  (* Bytes from [pos] up to the next block header, or to the end. Returns
     where it stopped and, when it stopped at a header, whether an
     end-of-file byte was written. *)
  let write_data pos =
    let rec scan p eof =
      if p + header_len > n then None
      else if header_at p then Some (p, eof)
      else scan (p + 1) (eof || cas.[p] = end_of_file)
    in
    match scan pos false with
    | Some (p, eof) ->
      if p > pos then emit (Data (pos, p - pos));
      (p, eof)
    | None ->
      if n > pos then emit (Data (pos, n - pos));
      (max pos n, false)
  in
  let lead silence pilot = emit (Silence silence); emit (Pilot pilot) in
  let classify p =
    if p + id_length > n then Unknown
    else
      let id = String.sub cas p id_length in
      if id = ascii_id then Ascii
      else if id = binary_id then Binary
      else if id = basic_id then Basic
      else Unknown
  in
  (* An ASCII file goes on block by block until a block holds 0x1A. Like
     openMSX, a short lead follows even when no block header is left. *)
  let rec ascii_blocks p =
    let p = p + header_len in
    lead short_silence short_pilot;
    let p, eof = write_data p in
    if eof || p + header_len > n then p else ascii_blocks p
  in
  let rec files pos found first =
    if pos + header_len > n then (found, first)
    else if not (header_at pos) then files (pos + 1) found first
    else begin
      let pos = pos + header_len in
      lead long_silence long_pilot;
      let kind = classify pos in
      let pos =
        match kind with
        | Ascii -> ascii_blocks (fst (write_data pos))
        | Binary | Basic ->
          let p, _ = write_data pos in
          lead short_silence short_pilot;
          fst (write_data (p + header_len))
        | Unknown -> fst (write_data pos)
      in
      files pos true (match first with None -> Some kind | some -> some)
    end
  in
  let found, first = files 0 false None in
  if not found then Error "not a CAS image: no block header"
  else
    let segments = Array.of_list (List.rev !out) in
    let starts = Array.make (Array.length segments) 0 in
    let total =
      Array.fold_left
        (fun (i, at) s -> starts.(i) <- at; (i + 1, at + segment_samples s))
        (0, 0) segments
      |> snd
    in
    Ok { cas; segments; starts; total; first = Option.value first ~default:Unknown }

let of_cas = convert
let cas t = t.cas
let first_file_type t = t.first
let samples t = t.total

(* A 0 bit is + + - -, a 1 bit + - + -. *)
let bit_level one s = if one then s mod 2 = 0 else s < 2

(* Last segment starting at or before sample [k]. *)
let segment_at t k =
  let rec search lo hi =
    if lo >= hi then lo
    else
      let mid = (lo + hi + 1) / 2 in
      if t.starts.(mid) <= k then search mid hi else search lo (mid - 1)
  in
  search 0 (Array.length t.starts - 1)

let level t k =
  if k < 0 || k >= t.total then true
  else
    let i = segment_at t k in
    let off = k - t.starts.(i) in
    match t.segments.(i) with
    | Silence _ -> true
    | Pilot _ -> bit_level true (off mod samples_per_bit)
    | Data (first, _) ->
      let byte = Char.code t.cas.[first + (off / samples_per_byte)] in
      let slot = off mod samples_per_byte / samples_per_bit in
      let one =
        if slot = 0 then false
        else if slot >= stop_bit_slot then true
        else (byte lsr (slot - 1)) land 1 = 1
      in
      bit_level one (off mod samples_per_bit)

(* openMSX measures time in units of 1/(3579545 * 960) s and finds a sample
   by dividing by MAIN_FREQ / 14976 rounded down (229458). *)
let units_per_cycle = 960
let main_freq = 3579545 * units_per_cycle
let units_per_sample = main_freq / sample_rate

type deck = {
  deck_tape : tape;
  mutable moved : int;  (** units the tape has moved *)
  mutable synced : int;  (** CPU cycle [moved] counts up to *)
}

let insert tape ~cycle = { deck_tape = tape; moved = 0; synced = cycle }
let tape d = d.deck_tape

let position d ~cycle ~motor =
  if motor then d.moved + ((cycle - d.synced) * units_per_cycle) else d.moved

let advance d ~cycle ~motor =
  d.moved <- position d ~cycle ~motor;
  d.synced <- cycle

let input d ~cycle ~motor =
  advance d ~cycle ~motor;
  level d.deck_tape (d.moved / units_per_sample)

let resume tape ~position ~cycle = { deck_tape = tape; moved = position; synced = cycle }
