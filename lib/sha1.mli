(** SHA-1 (FIPS 180-4). The openMSX software database names a ROM by the
    SHA-1 of its file, so looking a cartridge up there needs it. *)

val hex : string -> string
(** [hex s] is the SHA-1 of [s] as 40 lowercase hex digits. *)
