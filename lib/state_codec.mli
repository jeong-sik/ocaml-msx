(** Versioned binary state primitives. No closures or runtime values are decoded.
    Malformed input raises [Invalid_state], never partially publishes a machine. *)
exception Invalid_state of string

type writer
type reader

val writer : unit -> writer
val finish : writer -> string
val reader : string -> reader
val end_of_input : reader -> unit
val remaining : reader -> int
val fail : string -> 'a

val put_int : writer -> int -> unit
val get_int : reader -> min:int -> max:int -> int
val put_bool : writer -> bool -> unit
val get_bool : reader -> bool
val put_bytes : writer -> bytes -> unit
val get_bytes : reader -> bytes
val put_int_array : writer -> int array -> unit
val fill_int_array : reader -> min:int -> max:int -> int array -> unit
