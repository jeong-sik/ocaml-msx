(* The core's identity is the digest of the sources it was built from. What
   this pins: the baked value is the digest of the lib/ sources this build
   actually has (so it cannot go stale the way a hand-written constant
   would), and the digest moves when any byte, name or file boundary moves. *)

let failed = ref 0

let check_true name cond =
  if not cond then begin
    incr failed;
    Printf.eprintf "FAIL %s\n%!" name
  end

let check_s name got want =
  if got <> want then begin
    incr failed;
    Printf.eprintf "FAIL %s:\n  got=%S\n  want=%S\n%!" name got want
  end

let lib_dir = Filename.concat Filename.parent_dir_name "lib"

let lib_sources () =
  Sys.readdir lib_dir |> Array.to_list
  |> List.filter (fun f ->
         (String.equal f "dune"
          || Filename.check_suffix f ".ml" || Filename.check_suffix f ".mli")
         && not (Sys.is_directory (Filename.concat lib_dir f)))
  |> List.map (Filename.concat lib_dir)

let is_lower_hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')

let test_baked_value_is_this_builds_sources () =
  let paths = lib_sources () in
  check_true "lib/ has sources to digest" (paths <> []);
  check_s "baked digest = digest of the lib/ sources on disk"
    Msx_core_identity.source_digest (Core_digest.of_paths paths);
  check_true "every lib/ source went in"
    (Msx_core_identity.source_files = List.length paths);
  check_true "32 lowercase hex characters"
    (String.length Msx_core_identity.source_digest = 32
    && String.for_all is_lower_hex Msx_core_identity.source_digest)

let test_digest_moves_with_the_sources () =
  let base = [ ("a.ml", "let x = 1\n"); ("b.ml", "let y = 2\n") ] in
  let d = Core_digest.of_files base in
  check_s "input order does not matter" d (Core_digest.of_files (List.rev base));
  check_true "one changed byte changes it"
    (d <> Core_digest.of_files [ ("a.ml", "let x = 2\n"); ("b.ml", "let y = 2\n") ]);
  check_true "a renamed file changes it"
    (d <> Core_digest.of_files [ ("c.ml", "let x = 1\n"); ("b.ml", "let y = 2\n") ]);
  check_true "bytes moved across a file boundary change it"
    (d <> Core_digest.of_files [ ("a.ml", "let x = 1\nl"); ("b.ml", "et y = 2\n") ]);
  check_true "an added file changes it"
    (d <> Core_digest.of_files (("c.ml", "") :: base))

let () =
  test_baked_value_is_this_builds_sources ();
  test_digest_moves_with_the_sources ();
  if !failed = 0 then print_endline "core identity: all passed"
  else begin
    Printf.eprintf "core identity: %d failures\n%!" !failed;
    exit 1
  end
