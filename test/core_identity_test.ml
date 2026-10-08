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

let git_output root args =
  let command = Printf.sprintf "git -C %s %s" (Filename.quote root) args in
  match Unix.open_process_in command with
  | exception (Unix.Unix_error _ | Sys_error _) -> None
  | channel ->
    let output =
      match In_channel.input_all channel with
      | text -> Some (String.trim text)
      | exception Sys_error _ -> None in
    let status =
      match Unix.close_process_in channel with
      | status -> Some status
      | exception (Unix.Unix_error _ | Sys_error _) -> None in
    (match status, output with
     | Some (Unix.WEXITED 0), Some text -> Some text
     | Some (Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _), _
     | None, _ -> None)

let expected_source_commit () =
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | None -> None
  | Some root ->
    (match git_output root "status --porcelain --untracked-files=all",
           git_output root "rev-parse --verify HEAD" with
     | Some "", Some sha when String.length sha = 40 && String.for_all is_lower_hex sha ->
       Some sha
     | _ -> None)

let option_to_string = function None -> "None" | Some value -> "Some " ^ value

let test_source_commit_is_this_builds_git_revision () =
  check_s "baked source commit = clean git source root"
    (option_to_string Msx_core_identity.source_commit)
    (option_to_string (expected_source_commit ()));
  check_true "source commit is full lowercase SHA when present"
    (match Msx_core_identity.source_commit with
     | None -> true
     | Some sha -> String.length sha = 40 && String.for_all is_lower_hex sha)

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
  test_source_commit_is_this_builds_git_revision ();
  if !failed = 0 then print_endline "core identity: all passed"
  else begin
    Printf.eprintf "core identity: %d failures\n%!" !failed;
    exit 1
  end
