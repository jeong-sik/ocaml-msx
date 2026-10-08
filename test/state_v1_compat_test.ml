(* Regression for the v0.41.0 restart: every saved MSX checkpoint failed with
   "MSX state integer out of range" because ocaml-msx #38 inserted rtc_mode,
   rtc_regs, cart_is_disk_rom and fdc.disk_changed into the serialized layout
   without changing the envelope version, so the new reader consumed old
   payloads shifted. The fixture is a real v1 state written by the core at
   870e610 (fresh machine, 64KB RAM), stored as 2-byte run-length pairs. *)
let runs = [ (0x4f43,1);(0x414d,1);(0x4c2d,1);(0x4d53,1);(0x5800,1);(0x011c,1);(0x1cff,1);(0x1746,1);(0xd19c,1);(0x6e53,1);(0x75f0,1);(0x6515,1);(0xf2bd,1);(0xb900,1);(0x0000,2);(0x0100,1);(0x0000,32771);(0x0080,1);(0x0031,1);(0x00f0,1);(0x3ec0,1);(0xd3a8,1);(0x3e80,1);(0x32ff,1);(0xff21,1);(0x00c0,1);(0x34db,1);(0xa932,1);(0x01c0,1);(0xc30d,1);(0x0000,16375);(0x0040,1);(0x0000,8195);(0x0040,1);(0x0000,8216);(0x0100,1);(0x0000,3);(0x0200,1);(0x0000,3);(0x0300,1);(0x0000,3);(0x0300,1);(0x0000,3);(0x0300,1);(0x0000,3);(0x0300,1);(0x0000,3);(0x0300,1);(0x0000,119);(0x8000,1);(0x0000,27);(0x3f00,1);(0x0000,90);(0x00ff,1);(0xff00,1);(0x0000,204);(0x0200,1);(0x0000,65547);(0x0006,1);(0x1100,1);(0x0000,2);(0x0007,1);(0x3300,1);(0x0000,2);(0x0001,1);(0x1700,1);(0x0000,2);(0x0003,1);(0x2700,1);(0x0000,2);(0x0001,1);(0x5100,1);(0x0000,2);(0x0006,1);(0x2700,1);(0x0000,2);(0x0001,1);(0x7100,1);(0x0000,2);(0x0003,1);(0x7300,1);(0x0000,2);(0x0006,1);(0x6100,1);(0x0000,2);(0x0006,1);(0x6400,1);(0x0000,2);(0x0004,1);(0x1100,1);(0x0000,2);(0x0002,1);(0x6500,1);(0x0000,2);(0x0005,1);(0x5500,1);(0x0000,2);(0x0007,1);(0x7700,1);(0x0000,56);(0x0001,1);(0x0000,3) ]
let v1_state =
  let b = Buffer.create 270000 in
  List.iter (fun (u, n) ->
    for _ = 1 to n do
      Buffer.add_char b (Char.chr (u lsr 8)); Buffer.add_char b (Char.chr (u land 255))
    done) runs;
  Buffer.contents b
let check name cond = if not cond then failwith name
let v1_envelope_with_current_layout state =
  let magic_v2 = "OCAML-MSX\000\002" in
  let payload_offset = String.length magic_v2 + 16 in
  let payload = String.sub state payload_offset (String.length state - payload_offset) in
  "OCAML-MSX\000\001" ^ Digest.string payload ^ payload
let () =
  let current = Msx.create ~machine:{ram_kb = 64; vram_kb = 128; roms = [""; ""; ""]} in
  Msx.load_disk current (String.make 512 '\000');
  let current_v3 = Msx.serialize current in
  let v3_as_v1 = v1_envelope_with_current_layout current_v3 in
  check "re-enveloped fixture has a v1 envelope"
    (String.sub v3_as_v1 0 11 = "OCAML-MSX\000\001");
  (* Since v3 (ocaml-msx #46) a v1 envelope can legally carry only the frozen
     v1 layout; a current payload under a v1 envelope is corruption, and both
     decode branches of restore_v1 must reject it loudly instead of silently
     decoding a shifted machine. *)
  check "v1 envelope carrying a current payload is rejected"
    (match Msx.restore ~state:v3_as_v1 with Error _ -> true | Ok _ -> false);
  check "fixture is a v1 envelope" (String.sub v1_state 0 11 = "OCAML-MSX\000\001");
  match Msx.restore ~state:v1_state with
  | Error e -> failwith ("v1 checkpoint must restore: " ^ e)
  | Ok t ->
    let f0 = Msx.frame_number t in
    Msx.step t ~frames:3;
    check "restored v1 machine keeps running" (Msx.frame_number t = f0 + 3);
    let s2 = Msx.serialize t in
    check "re-serialized as current version" (String.sub s2 0 11 = "OCAML-MSX\000\003");
    (match Msx.restore ~state:s2 with
     | Ok t2 -> check "v2 round trip is stable" (Msx.serialize t2 = s2)
     | Error e -> failwith ("v2 round trip: " ^ e));
    let tampered = Bytes.of_string v1_state in
    Bytes.set tampered 11 '\xff';
    check "corrupt v1 still rejected"
      (match Msx.restore ~state:(Bytes.to_string tampered) with Error _ -> true | Ok _ -> false);
    let future = Bytes.of_string s2 in
    Bytes.set future 10 '\x09';
    check "unknown format number is named"
      (match Msx.restore ~state:(Bytes.to_string future) with
       | Error e -> e = "MSX state saved as format 9; this build reads formats 1-3"
       | Ok _ -> false);
    print_endline "state v1 compat: ok"
