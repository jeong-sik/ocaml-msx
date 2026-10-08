exception Invalid_state of string
let fail message = raise (Invalid_state message)
type writer = Buffer.t
type reader = { input : string; mutable pos : int; version : int }
(* v1: before ocaml-msx #38 (no rtc_mode/rtc_regs/cart_is_disk_rom/disk_changed).
   v2: ocaml-msx #38..#45 layout. v3: appends the WD2793 drive block
   (ocaml-msx #46); see Msx.serialize. Readers accept 1-3; writers emit v3. *)
let magic_prefix = "OCAML-MSX\000"
let magic = magic_prefix ^ "\002"
let magic_v3 = magic_prefix ^ "\003"
let magic_v1 = magic_prefix ^ "\001"
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
  magic_v3 ^ Digest.string payload ^ payload
let reader input =
  let start = String.length magic + 16 in
  if String.length input < start then fail "unsupported or truncated MSX state header";
  let head = String.sub input 0 (String.length magic) in
  let version =
    if head = magic_v3 then 3
    else if head = magic then 2 else if head = magic_v1 then 1
    else if String.sub head 0 (String.length magic_prefix) = magic_prefix then
      fail (Printf.sprintf "MSX state saved as format %d; this build reads formats 1-3"
              (Char.code head.[String.length magic_prefix]))
    else fail "unsupported or truncated MSX state header" in
  let payload = String.sub input start (String.length input - start) in
  if String.sub input (String.length magic) 16 <> Digest.string payload then
    fail "MSX state checksum mismatch";
  { input; pos = start; version }
let version r = r.version
let reader_with_current_v1_layout input =
  let r = reader input in
  if r.version = 1 then { r with version = 2 } else r
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
