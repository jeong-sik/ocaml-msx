exception Invalid_state of string
let fail message = raise (Invalid_state message)
type writer = Buffer.t
type reader = { input : string; mutable pos : int }
let magic = "OCAML-MSX\000\001"
let writer () = Buffer.create 1024
let put_int w n =
  let b = Bytes.create 8 in
  Bytes.set_int64_be b 0 (Int64.of_int n);
  Buffer.add_bytes w b
let put_bool w b = Buffer.add_char w (if b then '\001' else '\000')
let put_bytes w b = put_int w (Bytes.length b); Buffer.add_bytes w b
let put_int_array w a = Array.iter (put_int w) a
let finish w =
  let payload = Buffer.contents w in
  magic ^ Digest.string payload ^ payload
let reader input =
  let start = String.length magic + 16 in
  if String.length input < start || String.sub input 0 (String.length magic) <> magic then
    fail "unsupported or truncated MSX state header";
  let payload = String.sub input start (String.length input - start) in
  if String.sub input (String.length magic) 16 <> Digest.string payload then
    fail "MSX state checksum mismatch";
  { input; pos = start }
let remaining r = String.length r.input - r.pos
let take r n =
  if n < 0 || n > remaining r then fail "truncated MSX state";
  let pos = r.pos in r.pos <- pos + n; pos
let get_int r ~min ~max =
  let p = take r 8 in
  let n = String.get_int64_be r.input p in
  if n < Int64.of_int min || n > Int64.of_int max then fail "MSX state integer out of range";
  Int64.to_int n
let get_bool r =
  match r.input.[take r 1] with
  | '\000' -> false | '\001' -> true | _ -> fail "invalid MSX state boolean"
let get_bytes r =
  let n = get_int r ~min:0 ~max:(remaining r) in
  let p = take r n in Bytes.of_string (String.sub r.input p n)
let fill_int_array r ~min ~max a =
  Array.iteri (fun i _ -> a.(i) <- get_int r ~min ~max) a
let end_of_input r = if remaining r <> 0 then fail "trailing data in MSX state"
