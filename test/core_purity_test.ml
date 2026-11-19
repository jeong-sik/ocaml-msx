(* Core contract (ROADMAP 「코어 계약」): the emulator core reads no time,
   files, randomness or environment of its own; callers pass everything in.
   This scans the runtime core sources for the APIs that would break it.
   lib/identity/gen runs at build time and is not part of the core. *)

let forbidden =
  [ "Sys.getenv"; "Sys.time"; "Sys.file_exists"; "Sys.readdir"; "Unix.";
    "Random."; "open_in"; "open_out"; "In_channel."; "Out_channel." ]

let contains hay needle =
  let n = String.length needle and m = String.length hay in
  let rec at i = i + n <= m && (String.sub hay i n = needle || at (i + 1)) in
  at 0

let () =
  let dir = "../lib" in
  let sources =
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".ml")
    |> List.sort compare
  in
  if sources = [] then failwith "core_purity_test: no core sources found";
  let hits =
    List.concat_map
      (fun file ->
        let lines =
          In_channel.with_open_bin (Filename.concat dir file) In_channel.input_lines
        in
        List.concat
          (List.mapi
             (fun i line ->
               List.filter_map
                 (fun api ->
                   if contains line api then
                     Some (Printf.sprintf "lib/%s:%d uses %s" file (i + 1) api)
                   else None)
                 forbidden)
             lines))
      sources
  in
  List.iter prerr_endline hits;
  if hits <> [] then exit 1;
  Printf.printf "core purity: %d sources, no environment/file/clock/random reads\n"
    (List.length sources)
