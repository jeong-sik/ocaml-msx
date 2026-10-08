(* Writes msx_core_identity.ml: the digest of the source files named on the
   command line. Run by the rule in lib/identity/dune, never by hand. *)

let () =
  match List.tl (Array.to_list Sys.argv) with
  | [] ->
    prerr_endline "gen_core_identity: no source files given";
    exit 2
  | paths ->
    Printf.printf
      "(* Generated at build time by lib/identity/gen/gen_core_identity.exe. *)\n\
       let source_digest = %S\n\
       let source_files = %d\n"
      (Core_digest.of_paths paths) (List.length paths)
