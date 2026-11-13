(* A reader for openMSX's softwaredb.xml that keeps only what picks a
   mapper: per <dump>, the type and start of its <rom>/<megarom> and the
   SHA-1 in its <hash>. The rules follow openMSX RomDatabase.cc DBParser: a
   <rom> with no <type> is "Mirrored", a <megarom> has to name one, a
   "Mirrored" or "Normal" type with a <start> of the form 0xNNNN becomes
   that type with NNNN appended, a dump without a usable hash is dropped.
   Everything else (titles, remarks, <original>) is skipped over without
   being read. *)

type kind = Mapper of Msx.cart_mapper | Not_emulated of string

type t = { table : (string, kind) Hashtbl.t; entries : int; skipped : int }

let entries t = t.entries
let skipped t = t.skipped
let sha1_hex_len = 40

let plain_name : Msx.plain_layout -> string = function
  | Mirrored Header -> "plain"
  | Mirrored At_0000 -> "mirrored0000"
  | Mirrored At_4000 -> "mirrored4000"
  | Mirrored At_8000 -> "mirrored8000"
  | Mirrored At_c000 -> "mirroredc000"
  | Normal Header -> "normal"
  | Normal At_0000 -> "normal0000"
  | Normal At_4000 -> "normal4000"
  | Normal At_8000 -> "normal8000"
  | Normal At_c000 -> "normalc000"
  | Pages Page0 -> "page0"
  | Pages Page1 -> "page1"
  | Pages Page01 -> "page01"
  | Pages Page2 -> "page2"
  | Pages Page12 -> "page12"
  | Pages Page012 -> "page012"
  | Pages Page3 -> "page3"
  | Pages Page23 -> "page23"
  | Pages Page123 -> "page123"
  | Pages Page0123 -> "page0123"

let mapper_name : Msx.cart_mapper -> string = function
  | Plain layout -> plain_name layout
  | Ascii8 -> "ascii8"
  | Ascii16 -> "ascii16"
  | Konami -> "konami"
  | Konami_scc -> "konami-scc"
  | Ascii8_sram -> "koei"

let all_mappers : Msx.cart_mapper list =
  let starts : Msx.plain_start list = [ Header; At_0000; At_4000; At_8000; At_c000 ] in
  List.map (fun s -> Msx.Plain (Mirrored s)) starts
  @ List.map (fun s -> Msx.Plain (Normal s)) starts
  @ List.map
      (fun p -> Msx.Plain (Pages p))
      [ Page0; Page1; Page01; Page2; Page12; Page012; Page3; Page23; Page123; Page0123 ]
  @ [ Ascii8; Ascii16; Konami; Konami_scc; Ascii8_sram ]

let mapper_of_name name = List.find_opt (fun m -> mapper_name m = name) all_mappers

(* openMSX RomInfo.cc names and aliases, lowercased, for the types this
   emulator has. KoeiSRAM8 and KoeiSRAM32 both land on the one Koei mapper,
   whose 32KB of battery RAM covers either. *)
let kind_of_name name =
  match String.lowercase_ascii name with
  | "ascii8" | "4" -> Mapper Ascii8
  | "ascii16" | "5" -> Mapper Ascii16
  | "konami" | "konami4" | "3" -> Mapper Konami
  | "konamiscc" | "scc" | "konami5" | "2" -> Mapper Konami_scc
  | "koeisram8" | "koeisram32" -> Mapper Ascii8_sram
  | "mirrored" | "64kb" | "plain" -> Mapper (Plain (Mirrored Header))
  | "mirrored0000" -> Mapper (Plain (Mirrored At_0000))
  | "mirrored4000" -> Mapper (Plain (Mirrored At_4000))
  | "mirrored8000" -> Mapper (Plain (Mirrored At_8000))
  | "mirroredc000" -> Mapper (Plain (Mirrored At_c000))
  | "normal" -> Mapper (Plain (Normal Header))
  | "normal0000" | "0x0000" -> Mapper (Plain (Normal At_0000))
  | "normal4000" | "0x4000" -> Mapper (Plain (Normal At_4000))
  | "normal8000" | "0x8000" | "rombas" -> Mapper (Plain (Normal At_8000))
  | "normalc000" | "0xc000" -> Mapper (Plain (Normal At_c000))
  | "page0" -> Mapper (Plain (Pages Page0))
  | "page1" -> Mapper (Plain (Pages Page1))
  | "page01" -> Mapper (Plain (Pages Page01))
  | "page2" -> Mapper (Plain (Pages Page2))
  | "page12" -> Mapper (Plain (Pages Page12))
  | "page012" -> Mapper (Plain (Pages Page012))
  | "page3" -> Mapper (Plain (Pages Page3))
  | "page23" -> Mapper (Plain (Pages Page23))
  | "page123" -> Mapper (Plain (Pages Page123))
  | "page0123" -> Mapper (Plain (Pages Page0123))
  | _ -> Not_emulated name

exception Malformed of string

let is_hex c = match c with '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false

(* A SHA-1 as the database writes it: 40 hex digits, any case. *)
let hash_of_text s =
  if String.length s = sha1_hex_len && String.for_all is_hex s then
    Some (String.lowercase_ascii s)
  else None

(* Does [sub] start at [i] in [s]? *)
let at s i sub =
  let m = String.length sub in
  i + m <= String.length s && String.sub s i m = sub

(* The index just past the first [close] at or after [i]. *)
let past s i close what =
  let n = String.length s and m = String.length close in
  let rec go j =
    if j + m > n then raise (Malformed (Printf.sprintf "%s at %d never closes" what i))
    else if s.[j] = close.[0] && at s j close then j + m
    else go (j + 1)
  in
  go i

(* The index of the '>' that ends the tag at [i]. A '>' inside an
   attribute value would end it early, but attributes come after the name
   and only the name is read. *)
let tag_end s i =
  match String.index_from_opt s i '>' with
  | Some e -> e
  | None -> raise (Malformed (Printf.sprintf "tag at %d never closes" i))

let is_name_char c = not (List.mem c [ ' '; '\t'; '\n'; '\r'; '/'; '>' ])

(* The text from [i] up to the next tag, trimmed. *)
let text s i =
  let j = match String.index_from_opt s i '<' with Some j -> j | None -> String.length s in
  String.trim (String.sub s i (j - i))

(* The <rom> or <megarom> being read. A <type>, <start> or <hash> outside
   one is not read. *)
type rom = { mutable typ : string; mutable start : string; mutable hash : string option }

(* openMSX DBParser: "Mirrored" or "Normal" with a start of six characters
   from "0x" takes the four after "0x" as a suffix ("Normal" + "4000"). *)
let start_prefix = "0x"
let start_len = 6

let typed_name r =
  if (r.typ = "Mirrored" || r.typ = "Normal")
     && String.length r.start = start_len
     && String.starts_with ~prefix:start_prefix r.start
  then r.typ ^ String.sub r.start 2 (start_len - 2)
  else r.typ

let parse xml =
  let table = Hashtbl.create 8192 in
  let kept = ref 0 and dropped = ref 0 in
  let dump = ref None (* the dump's (kind, hash) once its rom closes *)
  and rom = ref None in
  let start_tag name i =
    match (name, !rom) with
    | "rom", None -> rom := Some { typ = "Mirrored"; start = ""; hash = None }
    | "megarom", None -> rom := Some { typ = ""; start = ""; hash = None }
    | "type", Some r -> r.typ <- text xml i
    | "start", Some r -> r.start <- text xml i
    | "hash", Some r ->
      (* openMSX skips a bad hash and keeps what the dump had. *)
      Option.iter (fun h -> r.hash <- Some h) (hash_of_text (text xml i))
    | _ -> ()
  in
  let end_tag name =
    match (name, !rom) with
    | ("rom" | "megarom"), Some r ->
      dump := Option.map (fun h -> (kind_of_name (typed_name r), h)) r.hash;
      rom := None
    | "dump", _ ->
      (match !dump with
       | Some (k, h) when not (Hashtbl.mem table h) ->
         Hashtbl.add table h k;
         incr kept
       | Some _ | None -> incr dropped);
      dump := None
    | _ -> ()
  in
  let n = String.length xml in
  let rec scan i =
    match String.index_from_opt xml i '<' with
    | None -> ()
    | Some i when i >= n - 1 -> ()
    | Some i ->
      if at xml i "<!--" then scan (past xml (i + 4) "-->" "comment")
      else if at xml i "<![CDATA[" then scan (past xml (i + 9) "]]>" "CDATA")
      else if at xml i "<?" then scan (past xml (i + 2) "?>" "declaration")
      else if at xml i "<!" then scan (tag_end xml (i + 2) + 1)
      else if at xml i "</" then begin
        let e = tag_end xml (i + 2) in
        end_tag (String.trim (String.sub xml (i + 2) (e - i - 2)));
        scan (e + 1)
      end
      else begin
        let e = tag_end xml (i + 1) in
        let j = ref (i + 1) in
        while !j < e && is_name_char xml.[!j] do incr j done;
        let name = String.sub xml (i + 1) (!j - i - 1) in
        start_tag name (e + 1);
        scan (e + 1)
      end
  in
  match scan 0 with
  | () -> Ok { table; entries = !kept; skipped = !dropped }
  | exception Malformed m -> Error m

let find t rom = Hashtbl.find_opt t.table (Sha1.hex rom)
