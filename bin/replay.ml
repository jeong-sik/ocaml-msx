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
let disk = ref ""
let restore_state = ref ""
let save_state = ref ""
let change_disk = ref ""
let ledger = ref ""
let trace_disk = ref false
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

type entry = { at : int; key : string; down : bool }

let parse_ledger path =
  read_file path
  |> String.split_on_char '\n'
  |> List.mapi (fun index line ->
       let invalid reason = invalid_arg
         (Printf.sprintf "ledger line %d: %s" (index + 1) reason) in
       if String.trim line = "" then None
       else
         let json = try Yojson.Safe.from_string line with
           | Yojson.Json_error _ -> invalid "invalid JSON" in
         match json with
         | `Assoc fields ->
             let names = List.map fst fields in
             if List.length names <> List.length (List.sort_uniq String.compare names) then
               invalid "duplicate field";
             let at = match List.assoc_opt "frame" fields with
               | Some (`Int n) when n >= 0 -> n
               | _ -> invalid "frame must be a nonnegative integer" in
             let key = match List.assoc_opt "key" fields with
               | Some (`String key) when key <> "" -> key
               | _ -> invalid "key must be a nonempty string" in
             let down = match List.assoc_opt "edge" fields with
               | Some (`String "down") -> true
               | Some (`String "up") -> false
               | _ -> invalid "edge must be down or up" in
             Some { at; key; down }
         | _ -> invalid "expected a JSON object")
  |> List.filter_map Fun.id
  |> List.stable_sort (fun a b -> compare a.at b.at)

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
      ("--disk", Arg.Set_string disk, "PATH  disk image to warm-boot");
      ("--change-disk", Arg.Set_string change_disk, "PATH  replace disk in restored machine without rebooting");
      ("--restore-state", Arg.Set_string restore_state, "FILE  resume a saved machine instead of booting");
      ("--save-state", Arg.Set_string save_state, "FILE  atomically save the final machine state");
      ("--trace-disk", Arg.Set trace_disk, "log disk BIOS and BDOS calls during replay");
      ("--ledger", Arg.Set_string ledger, "FILE  the .masc/msx/ledger.jsonl to replay");
      ("--out-dir", Arg.Set_string out_dir, "DIR  where frame PPMs are written");
      ("--every", Arg.Set_int every, "N  dump one frame every N (default 5)");
      ("--tail", Arg.Set_int tail, "N  frames to run past the last input (default 120)") ]
    (fun _ -> ())
    "replay — reproduce a keeper's MSX session from its ledger";
  if !ledger = "" then (prerr_endline "replay: --ledger is required"; exit 2);
  if !every < 1 || !tail < 0 then (prerr_endline "replay: --every must be positive and --tail nonnegative"; exit 2);
  if (!restore_state <> "" && (!cart <> "" || !disk <> "" || !roms_dir <> ""))
     || (!cart <> "" && !disk <> "") then
    (prerr_endline "replay: choose cartridge, disk, or saved state"; exit 2);
  if !change_disk <> "" && !restore_state = "" then
    (prerr_endline "replay: --change-disk requires --restore-state"; exit 2);
  let entries =
    try parse_ledger !ledger with
    | Invalid_argument message | Sys_error message ->
        prerr_endline ("replay: " ^ message); exit 2
  in
  let roms =
    if !roms_dir = "" then [ ""; ""; "" ]
    else
      List.map
        (fun f ->
          let p = Filename.concat !roms_dir f in
          if Sys.file_exists p then read_file p else "")
        [ "cbios_main_msx2.rom"; "cbios_logo_msx2.rom"; "cbios_sub.rom" ]
  in
  let t =
    if !restore_state <> "" then
      match Msx.restore ~state:(read_file !restore_state) with
      | Ok t -> t | Error e -> prerr_endline e; exit 2
    else begin
      let t = Msx.create ~machine:{ ram_kb = 512; vram_kb = 128; roms } in
      if !cart <> "" then Msx.load_cartridge t (read_file !cart);
      if !disk <> "" then begin
        Msx.load_disk ~interface_rom:false t (read_file !disk);
        Msx.step t ~frames:720;
        match Msx.boot_disk t with
        | Ok () -> () | Error e -> prerr_endline e; exit 2
      end;
      Msx.step t ~frames:boot_frames;
      t
    end
  in
  if !change_disk <> "" then begin
    match Msx.change_disk t (read_file !change_disk) with
    | Ok () -> () | Error message -> prerr_endline message; exit 2
  end;
  (try Unix.mkdir !out_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  Msx.set_disk_call_log !trace_disk;
  let initial_frame = Msx.frame_number t in
  List.iter (fun e ->
    if e.at < initial_frame then failwith "ledger entry predates initial machine state";
    match key_of_string e.key with
    | None -> failwith ("unknown ledger key: " ^ e.key)
    | Some _ -> ()) entries;
  let last = List.fold_left (fun m e -> max m e.at) initial_frame entries in
  if !tail > max_int - last then
    (prerr_endline "replay: final frame exceeds supported integer range"; exit 2);
  let total = last + !tail in
  let frame = ref initial_frame in
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
          | Some k -> if not (Msx.set_key t k ~pressed:e.down) then failwith ("unmapped ledger key: " ^ e.key)
          | None -> ())
      entries
  in
  while !frame < total do
    apply_at !frame;
    Msx.step t ~frames:1;
    incr frame;
    dump ()
  done;
  (* Apply an edge at the final boundary even when --tail is zero. *)
  apply_at !frame;
  write_ppm t (Filename.concat !out_dir "final.ppm");
  if !save_state <> "" then begin
    let tmp, oc = Filename.open_temp_file ~temp_dir:(Filename.dirname !save_state) ".msx-state-" ".tmp" in
    Fun.protect ~finally:(fun () -> close_out_noerr oc; if Sys.file_exists tmp then Sys.remove tmp)
      (fun () -> output_string oc (Msx.serialize t); close_out oc; Sys.rename tmp !save_state)
  end;
  if !trace_disk then begin
    List.iter (fun (pc, a, bc, de, hl, f) ->
      Printf.printf "disk @%04x a=%02x bc=%04x de=%04x hl=%04x f=%02x\n"
        pc a bc de hl f) (Msx.disk_call_entries ());
    Array.iteri (fun function_number count ->
      if count > 0 then Printf.printf "bdos %02x: %d\n" function_number count)
      (Msx.bdos_counts ())
  end;
  Printf.printf "final frame=%d pc=%04x mode=%s\n"
    (Msx.frame_number t) (Msx.dump_pc t) (Msx.display_mode_to_string (Msx.display_mode t));
  Printf.printf
    "replayed %d entries over %d frames (boot %d + play %d + tail %d); %d frames in %s\n"
    (List.length entries) total initial_frame (last - initial_frame) !tail !dumped !out_dir
