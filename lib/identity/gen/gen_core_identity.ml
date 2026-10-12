(* Writes msx_core_identity.ml from the source files named on the command
   line and the clean Git checkout that Dune built. Run by Dune, never by hand. *)

let read_git root args =
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
;;

let is_full_sha sha =
  String.length sha = 40
  && String.for_all
       (function '0'..'9' | 'a'..'f' -> true | _ -> false)
       sha
;;

let realpath path =
  match Unix.realpath path with
  | path -> Some path
  | exception Unix.Unix_error _ -> None
;;

let source_commit project_path =
  (* Dune expands project_root relative to the action's working directory,
     which the rule sets to workspace_root. Both are build-context paths;
     only DUNE_SOURCEROOT identifies the corresponding source workspace. *)
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | None -> None
  | Some workspace ->
    let root = Filename.concat workspace project_path in
    (* A vendored/archive project must not inherit a parent repository's
       identity. A worktree's .git file, as well as a .git directory, owns
       a checkout. Canonical paths also handle symlinked workspace roots. *)
    if not (Sys.file_exists (Filename.concat root ".git")) then None
    else
      (match realpath root, read_git root "rev-parse --show-toplevel" with
       | Some root, Some top when realpath top = Some root ->
         (match read_git root "status --porcelain --untracked-files=all --ignore-submodules=none",
                read_git root "rev-parse --verify HEAD" with
          | Some "", Some sha when is_full_sha sha -> Some sha
          | _ -> None)
       | _ -> None)
;;

let () =
  match List.tl (Array.to_list Sys.argv) with
  | [] | [_] ->
    prerr_endline "gen_core_identity: expected project path and source files";
    exit 2
  | project_path :: paths ->
    let source_commit =
      match source_commit project_path with
      | Some sha -> "Some " ^ Printf.sprintf "%S" sha
      | None -> "None" in
    Printf.printf
      "(* Generated at build time by lib/identity/gen/gen_core_identity.exe. *)\n\n       let source_digest = %S\n\n       let source_files = %d\n\n       let source_commit = %s\n"
      (Core_digest.of_paths paths) (List.length paths) source_commit
;;
