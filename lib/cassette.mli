(** Cassette tape for CAS images, played as a signal the way openMSX 21.0
    CasImage and CassettePlayer do: the BIOS reads PSG R#14 bit 7 and
    measures the pulses itself, so a tape loads through the same code paths
    as on a real machine.

    A CAS image becomes a 3744 baud wave of 4 samples a bit (14976 Hz). The
    deck moves the tape only while the motor (PPI port C bit 4 = 0) is on,
    counting CPU cycles; it reads no host clock. *)

type tape

type file_type = Ascii | Binary | Basic | Unknown
(** What the first file on the tape is: [RUN"CAS:"], [BLOAD"CAS:",R],
    [CLOAD], or none of the three. *)

val of_cas : string -> (tape, string) result
(** Error when the image holds no CAS block header. *)

val cas : tape -> string
(** The image the tape was made from. *)

val first_file_type : tape -> file_type

val sample_rate : int
(** Samples per second of the wave: 14976. *)

val samples : tape -> int
(** Length of the wave in samples. *)

val level : tape -> int -> bool
(** [level tape k] is the cassette input for sample [k]: [true] for silence
    and the positive half of each pulse, [false] for the negative half.
    Before the first and after the last sample it is [true]. *)

type deck

val insert : tape -> cycle:int -> deck
(** A deck holding [tape] rewound to its start at CPU cycle [cycle]. *)

val input : deck -> cycle:int -> motor:bool -> bool
(** The input bit at CPU cycle [cycle]. [motor] is the motor state since the
    last call; the tape moved only if it was on. *)

val advance : deck -> cycle:int -> motor:bool -> unit
(** Move the tape to CPU cycle [cycle]; call it before the motor changes. *)

val position : deck -> cycle:int -> motor:bool -> int
(** How far the tape has moved by CPU cycle [cycle], in units of
    1/(3579545 * 960) s, without changing the deck. *)

val tape : deck -> tape

val resume : tape -> position:int -> cycle:int -> deck
(** A deck at [position] (as from [position]) at CPU cycle [cycle]. *)
