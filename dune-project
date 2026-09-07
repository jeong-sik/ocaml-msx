(lang dune 3.0)
(name ocaml-msx)
(generate_opam_files true)
(package
 (name ocaml-msx)
 (synopsis "MSX2 emulator core - pure, deterministic, no IO")
 (description "Z80, V9938 and the MSX2 machine wiring. Pure library: time and IO live in the client.")
 (depends ocaml dune))
