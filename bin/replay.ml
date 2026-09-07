(* Replay a keeper session (RFC-0439 §3.6). The core is deterministic — no
   clock, no randomness — so the same cartridge and the same ledger reproduce
   the same frames. This reads the ledger masc's Msx_lane writes,
   (frame, who, key, edge) one JSON object a line, drives a fresh machine
   through it, and dumps a PPM every [--every] frames so the run can be watched
   as a flip-book or assembled into a GIF.

   The ledger's frame numbers are absolute from power-on and include the boot
   pre-step, so this boots the same [boot_frames] and then advances to each
   entry's frame in order. *)

let roms_dir = ref ""
let cart = ref ""
let ledger = ref ""
let out_dir = ref "/tmp/msx-replay"
let every = ref 5
let tail = ref 120
let boot_frames = 45 (* matches masc Msx_lane.boot_frames *)

let read_file p =
  let ic = open_in_bin p in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* One field out of the fixed ledger shape: name is frame, key, or edge. *)
let field line name =
  let key = "\"" ^ name ^ "\":" in
  match
    let klen = String.length key and llen = String.length line in
    let rec find i =
      if i + klen > llen then None
      else if String.sub line i klen = key then Some (i + klen)
      else find (i + 1)
    in
    find 0
  with
  | None -> None
  | Some start ->
    let llen = String.length line in
    let start = if start < llen && line.[start] = '"' then start + 1 else start in
    let stop = ref start in
    while
      !stop < llen
      &&
      let c = line.[!stop] in
      c <> '"' && c <> ',' && c <> '}'
    do
      incr stop
    done;
    Some (String.sub line start (!stop - start))

type entry = { at : int; key : string; down : bool }

let parse_ledger path =
  read_file path
  |> String.split_on_char '\n'
  |> List.filter_map (fun line ->
       if String.trim line = "" then None
       else
         match field line "frame", field line "key", field line "edge" with
         | Some f, Some k, Some e -> (
           match int_of_string_opt f with
           | Some at -> Some { at; key = k; down = e = "down" }
           | None -> None)
         | _ -> None)
  |> List.sort (fun a b -> compare a.at b.at)

let key_of_string s : Msx.key option =
  match String.lowercase_ascii s with
  | "up" -> Some Msx.Up
  | "down" -> Some Msx.Down
  | "left" -> Some Msx.Left
  | "right" -> Some Msx.Right
  | "space" -> Some Msx.Space
  | "esc" | "escape" -> Some Msx.Esc
  | "return" | "enter" -> Some Msx.Return
  | "trigger_a" -> Some Msx.Trigger_a
  | "trigger_b" -> Some Msx.Trigger_b
  | "f1" -> Some (Msx.Function 1)
  | "f2" -> Some (Msx.Function 2)
  | "f3" -> Some (Msx.Function 3)
  | "f4" -> Some (Msx.Function 4)
  | "f5" -> Some (Msx.Function 5)
  | k when String.length k = 1 -> Some (Msx.Char k.[0])
  | _ -> None

let write_ppm t path =
  let w, h = Msx.frame_dims t in
  let rgb = Msx.frame_rgb t in
  let oc = open_out_bin path in
  Printf.fprintf oc "P6\n%d %d\n255\n" w h;
  output_string oc rgb;
  close_out oc

let () =
  Arg.parse
    [ ("--roms", Arg.Set_string roms_dir, "DIR  C-BIOS roms directory");
      ("--cart", Arg.Set_string cart, "PATH  cartridge ROM the ledger was recorded on");
      ("--ledger", Arg.Set_string ledger, "FILE  the .masc/msx/ledger.jsonl to replay");
      ("--out-dir", Arg.Set_string out_dir, "DIR  where frame PPMs are written");
      ("--every", Arg.Set_int every, "N  dump one frame every N (default 5)");
      ("--tail", Arg.Set_int tail, "N  frames to run past the last input (default 120)") ]
    (fun _ -> ())
    "replay — reproduce a keeper's MSX session from its ledger";
  if !ledger = "" then (prerr_endline "replay: --ledger is required"; exit 2);
  let roms =
    if !roms_dir = "" then [ ""; ""; "" ]
    else
      List.map
        (fun f ->
          let p = Filename.concat !roms_dir f in
          if Sys.file_exists p then read_file p else "")
        [ "cbios_main_msx2.rom"; "cbios_logo_msx2.rom"; "cbios_sub.rom" ]
  in
  let t = Msx.create ~machine:{ ram_kb = 512; vram_kb = 128; roms } in
  if !cart <> "" then Msx.load_cartridge t (read_file !cart);
  (try Unix.mkdir !out_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let entries = parse_ledger !ledger in
  let last = List.fold_left (fun m e -> max m e.at) boot_frames entries in
  let total = last + !tail in
  Msx.step t ~frames:boot_frames;
  let frame = ref boot_frames in
  let dumped = ref 0 in
  let dump () =
    if !frame mod !every = 0 then begin
      write_ppm t (Printf.sprintf "%s/frame-%06d.ppm" !out_dir !frame);
      incr dumped
    end
  in
  dump ();
  let apply_at f =
    List.iter
      (fun e ->
        if e.at = f then
          match key_of_string e.key with
          | Some k -> ignore (Msx.set_key t k ~pressed:e.down : bool)
          | None -> ())
      entries
  in
  while !frame < total do
    incr frame;
    apply_at !frame;
    Msx.step t ~frames:1;
    dump ()
  done;
  Printf.printf
    "replayed %d entries over %d frames (boot %d + play %d + tail %d); %d frames in %s\n"
    (List.length entries) total boot_frames (last - boot_frames) !tail !dumped !out_dir
