(** The identity of this build of the core.

    A client that links ocaml-msx cannot otherwise tell which core it got:
    a project built without its vendored copy links whatever opam installed,
    and the two can differ by a fix the client depends on. This module lets
    the client say which one it has.

    The source digest identifies the linked library inputs. [source_commit]
    separately reports the full commit that built this identity when Dune sees
    a clean Git checkout; archive and dirty-tree builds report [None]. *)

val source_digest : string
(** Lowercase hex MD5 over every [.ml] and [.mli] file in the core's [lib/]
    directory, plus that directory's [dune] file, each taken with its base
    name and length, in name order. Computed by the build, never written by
    hand. *)

val source_files : int
(** How many files went into {!source_digest}. *)

val source_commit : string option
(** The full Git commit of the clean source checkout used for this build.
    [None] when the build has no Git checkout or the checkout was dirty. *)
