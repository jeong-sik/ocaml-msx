(** YM2413 (OPLL), the FM chip of MSX-MUSIC and the FM-PAC: a port of
    emu2413 v1.6.3 (MIT, Mitsutaka Okazaki). It departs from emu2413 in one
    place: the outputs of the inactive mode (melody channels 6-8 or the five
    drums) read 0.

    The chip makes one sample per 72 master clocks (49716 Hz at 3.58 MHz).
    [step] makes one; the caller decides when, and resamples. A register
    write takes effect in the next [step]. *)

type t

val create : unit -> t
(** A chip after reset: every register 0, the instrument ROM loaded. *)

val reset : t -> unit
(** Clear the registers and the slots. The user instrument is register 0-7,
    so it clears too. *)

val write_address : t -> int -> unit
(** Port 0x7C: latch the register number. *)

val write_data : t -> int -> unit
(** Port 0x7D: write the latched register. Registers 0x40 and above are
    ignored; 0x19-0x1F, 0x29-0x2F and 0x39-0x3F mirror nine below. *)

val write_register : t -> int -> int -> unit
(** [write_register t reg v] is [write_address t reg; write_data t v]
    without touching the latch. *)

val register : t -> int -> int
(** The value last written to register [r] (0x00-0x3F, after mirroring). *)

val step : t -> unit
(** Advance one chip sample. *)

val outputs : int
(** 14: the nine melody channels, then bass drum, hi-hat, snare drum, tom-tom
    and top cymbal. *)

val output : t -> int -> int
(** The last sample of output [i]. A melody channel ranges -2048..2048, a
    rhythm output -4096..4094. Melody channels 6-8 read 0 in rhythm mode, and
    the rhythm outputs read 0 outside it. *)

val mix : t -> int
(** The sum of the fourteen outputs, wrapped to 16 bits the way emu2413's
    [OPLL_calc] wraps it. *)
