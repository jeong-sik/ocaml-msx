(** The identity of this build of the core.

    A client that links ocaml-msx cannot otherwise tell which core it got:
    a project built without its vendored copy links whatever opam installed,
    and the two can differ by a fix the client depends on. This module lets
    the client say which one it has.

    The identity is a digest of sources, not a commit. An opam or archive
    install has no git history to ask, and a digest does not change when the
    same sources land under a different commit (a squash merge, a rebase). *)

val source_digest : string
(** Lowercase hex MD5 over every [.ml] and [.mli] file in the core's [lib/]
    directory, plus that directory's [dune] file, each taken with its base
    name and length, in name order. Computed by the build, never written by
    hand. *)

val source_files : int
(** How many files went into {!source_digest}. *)
