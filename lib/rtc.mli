(** Ricoh RP5C01 real-time clock: four blocks of thirteen 4-bit registers
    plus the mode, test and reset registers. Block 0 holds the time, block 1
    the alarm, the 12/24-hour flag and the leap-year counter; blocks 2 and 3
    are battery-backed RAM.

    The clock reads no host time. It counts a 16384 Hz timebase from the CPU
    cycle given with each access, so a run stays a pure function of its
    inputs; [set_time] chooses the date it starts from. The behaviour follows
    openMSX 21.0 RP5C01 in its default EmuTime mode. *)

type t

type time
(** A date and time the clock can show: years 1980 to 2079. *)

val time :
  year:int -> month:int -> day:int -> hour:int -> minute:int -> second:int ->
  (time, string) result
(** [month] and [day] count from 1. The error names the field that is out of
    range. *)

val create : unit -> t
(** Power-on state: timer on, test register 0, every time register 0 (the
    date 00-00-00), the alarm block and both RAM blocks reading 0xF. *)

val set_time : t -> cycle:int -> time -> unit
(** Show [time] from CPU cycle [cycle] on; the fraction of a second starts
    at 0. The weekday register follows from the date (0 = Sunday). *)

val read : t -> cycle:int -> int -> int
(** [read t ~cycle reg] is register [reg land 0xF] at CPU cycle [cycle], a
    nibble. Reading the time or alarm block first counts the ticks since the
    last access into the registers. *)

val write : t -> cycle:int -> int -> int -> unit
(** [write t ~cycle reg v] stores the low nibble of [v]. *)

val write_registers : State_codec.writer -> t -> unit
val read_registers : State_codec.reader -> t -> unit
(** The mode register and the 52 register nibbles (state format 2 on). *)

val write_clock : State_codec.writer -> t -> unit
val read_clock : State_codec.reader -> t -> cycle:int -> unit
(** The test register, the fraction of a second and the last tick counted
    into the registers (state format 4 on). [cycle] is the restored CPU time;
    a saved tick after it is rejected. *)

val start_clock : t -> cycle:int -> unit
(** For states saved before format 4, whose clock never ran: count ticks
    from [cycle] on. *)
