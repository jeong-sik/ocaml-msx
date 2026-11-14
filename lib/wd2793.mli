(** The NMS 8250 disk interface: a WD2793 controller behind the Philips
    register window (0x7FF8-0x7FFF of the disk ROM's slot), drive A and an
    empty drive B. It keeps time like openMSX 21.0 (WD2793, RealDrive,
    SectorBasedDisk, PhilipsFDC): the disk turns at 300 rpm while the motor
    is on, a track holds 6250 bytes in the standard 9-sector layout, a data
    byte passes the head every 32 µs and a head step takes 6 to 30 ms.

    Times are master ticks: 960 per Z80 cycle (openMSX MAIN_FREQ = 3579545
    × 960), so a Z80 cycle count [c] is time [c * ticks_per_cycle]. Every
    access passes its time; times never go back. The controller acts on
    its own between accesses (a seek steps, a sector arrives under the
    head), and it catches up to the access time before it answers.

    The disk image is drive A's: plain 512-byte sectors in logical order,
    passed on every access because the machine owns it. An empty image is
    an empty drive. Writes to the disk land in the image. *)

type t

val ticks_per_cycle : int

val create : unit -> t
(** At power-on (time 0): drive A selected with the motor off, side 0,
    and the controller busy with a RESTORE at the slowest step rate
    (openMSX WD2793::reset). With the motor off the drive never reports
    track 0, so that RESTORE steps until the track register counts down
    to 0, 7.65 s, unless a command stops it first. *)

val read : t -> disk:Bytes.t -> time:int -> int -> int
(** [read t ~disk ~time r] reads window register [r] (0-7, address
    0x7FF8 + r):
    - 0 status. Reading it clears INTRQ unless that is still to come.
    - 1 track, 2 sector.
    - 3 data. While a read command holds DRQ this takes the next byte.
    - 4 the last value written to 4 (bit 0 = side).
    - 5 the last value written to 5, with bit 2 = 0 when drive A's disk
      changed since the last read of 5 (that read takes the news).
    - 6 0xFF.
    - 7 0xFF with bit 6 = 0 while INTRQ and bit 7 = 0 while DRQ. *)

val write : t -> disk:Bytes.t -> time:int -> int -> int -> unit
(** [write t ~disk ~time r v]: 0 command, 1 track, 2 sector, 3 data,
    4 side (bit 0), 5 drive (bits 0-1: 0 or 2 = A, 1 = B, 3 = none; bit 7
    = motor). 6 and 7 take nothing. *)

val image_written : t -> unit
(** Something other than the controller wrote drive A's image (the disk
    HLE): the controller drops the track it holds, so its next access
    reads the new bytes. Software sees no disk change. *)

val disk_changed : t -> unit
(** A new image went into drive A: the controller drops the track it
    holds, and the next read of register 5 reports the change. *)

val write_state : State_codec.writer -> t -> unit
val read_state : State_codec.reader -> t -> unit
