(** The openMSX software database ([share/softwaredb.xml]) read as a map
    from a ROM file's SHA-1 to its mapper.

    openMSX picks a cartridge's mapper from this database first and only
    guesses from the code when the ROM is not in it (RomFactory.cc). The
    guess alone gets many commercial MegaROMs wrong, so a caller that has
    the file can hand its text to {!parse}. The database belongs to the
    openMSX and BlueMSX teams; this module only reads a copy the user
    already has, and the core never opens files itself. *)

type kind =
  | Mapper of Msx.cart_mapper  (** a type this emulator models *)
  | Not_emulated of string  (** a type it does not; the database's name *)

type t

val parse : string -> (t, string) result
(** [parse xml] reads the text of a softwaredb.xml. Each [<dump>] holds a
    [<rom>] (plain unless its [<type>] says otherwise) or a [<megarom>],
    and the SHA-1 in its [<hash>]. A dump without a valid hash is dropped
    and counted in {!skipped}, as openMSX does; a repeated hash keeps the
    first dump. [Error] when a comment, tag or CDATA section never
    closes. *)

val find : t -> string -> kind option
(** [find db rom] looks up the SHA-1 of [rom]'s bytes. [None] when the
    database does not list it. *)

val entries : t -> int
(** Dumps kept. *)

val skipped : t -> int
(** Dumps dropped: no valid hash, or a hash already kept. *)

val kind_of_name : string -> kind
(** A database type name, matched without regard to case as openMSX
    RomInfo::nameToRomType does, aliases included. Every plain type
    (Mirrored, Normal, Page...) is [Mapper Flat]: where a plain ROM sits
    is {!Msx.load_cartridge}'s business. *)

val mapper_name : Msx.cart_mapper -> string
(** The name [boot.exe --cart-mapper] takes for a mapper. *)

val mapper_of_name : string -> Msx.cart_mapper option
(** The inverse of {!mapper_name}. *)
