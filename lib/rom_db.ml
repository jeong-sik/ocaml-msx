(* A reader for openMSX's softwaredb.xml that keeps only what picks a
   mapper: per <dump>, the type of its <rom>/<megarom> and the SHA-1 in its
   <hash>. The rules follow openMSX RomDatabase.cc DBParser: a <rom> with
   no <type> is "Mirrored", a <megarom> has to name one, a dump without a
   usable hash is dropped. Everything else (titles, remarks, <original>)
   is skipped over without being read. *)

type kind = Mapper of Msx.cart_mapper | Not_emulated of string

type t = { table : (string, kind) Hashtbl.t; entries : int; skipped : int }

let entries t = t.entries
let skipped t = t.skipped
let sha1_hex_len = 40

let mapper_name : Msx.cart_mapper -> string = function
  | Flat -> "plain"
  | Ascii8 -> "ascii8"
  | Ascii16 -> "ascii16"
  | Konami -> "konami"
  | Konami_scc -> "konami-scc"
  | Ascii8_sram -> "koei"

let mapper_of_name = function
  | "plain" -> Some Msx.Flat
  | "ascii8" -> Some Msx.Ascii8
  | "ascii16" -> Some Msx.Ascii16
  | "konami" -> Some Msx.Konami
  | "konami-scc" -> Some Msx.Konami_scc
  | "koei" -> Some Msx.Ascii8_sram
  | _ -> None

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
  | "mirrored" | "mirrored0000" | "mirrored4000" | "mirrored8000" | "mirroredc000"
  | "normal" | "normal0000" | "normal4000" | "normal8000" | "normalc000"
  | "page0" | "page1" | "page2" | "page3" | "page01" | "page12" | "page23"
  | "page012" | "page123" | "page0123"
  | "64kb" | "plain" | "0x0000" | "0x4000" | "0x8000" | "0xc000" | "rombas" ->
    Mapper Flat
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

(* The <rom> or <megarom> being read. A <type> or <hash> outside one is
   not read. *)
type rom = { mutable typ : string; mutable hash : string option }

let parse xml =
  let table = Hashtbl.create 8192 in
  let kept = ref 0 and dropped = ref 0 in
  let dump = ref None (* the dump's (kind, hash) once its rom closes *)
  and rom = ref None in
  let start_tag name i =
    match (name, !rom) with
    | "rom", None -> rom := Some { typ = "Mirrored"; hash = None }
    | "megarom", None -> rom := Some { typ = ""; hash = None }
    | "type", Some r -> r.typ <- text xml i
    | "hash", Some r ->
      (* openMSX skips a bad hash and keeps what the dump had. *)
      Option.iter (fun h -> r.hash <- Some h) (hash_of_text (text xml i))
    | _ -> ()
  in
  let end_tag name =
    match (name, !rom) with
    | ("rom" | "megarom"), Some r ->
      dump := Option.map (fun h -> (kind_of_name r.typ, h)) r.hash;
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
