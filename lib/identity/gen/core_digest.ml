(* The digest behind Msx_core_identity.source_digest. Shared by the build
   rule that writes the value and by the test that recomputes it, so both
   read the same bytes the same way.

   Files are taken by base name, sorted, so the digest is the same whether
   the core is built from a git checkout, an opam archive or a vendored
   copy under another project: none of them moves a file within lib/. Each
   file contributes its name, its length and its bytes, so moving bytes
   from one file to the next, or renaming a file, changes the digest. *)

let of_files (files : (string * string) list) =
  let buf = Buffer.create 4096 in
  files
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  |> List.iter (fun (name, contents) ->
         Buffer.add_string buf name;
         Buffer.add_char buf '\000';
         Buffer.add_string buf (string_of_int (String.length contents));
         Buffer.add_char buf '\000';
         Buffer.add_string buf contents);
  Digest.to_hex (Digest.string (Buffer.contents buf))

let read path = In_channel.with_open_bin path In_channel.input_all

let of_paths paths =
  of_files (List.map (fun p -> (Filename.basename p, read p)) paths)
