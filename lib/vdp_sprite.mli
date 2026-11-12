(** V9938 sprite engine: which sprites a display line shows, whether they
    collide, and what colour each pixel takes. Pure over the VDP registers and
    a logical-address VRAM reader; the VDP owns timing and status bits.

    Reference: openMSX [SpriteChecker.cc] (line evaluation, 5th/9th sprite,
    collision) and [SpriteConverter.hh] (drawing, CC colour merge). *)

(** Sprite mode 1 serves GRAPHIC 1/2 and MULTICOLOR; sprite mode 2 serves
    GRAPHIC 3-7. TEXT modes show no sprites, so the VDP passes no mode. *)
type mode = Mode1 | Mode2

type hit = {
  x : int;  (** left edge on the 256-dot grid, already shifted 32 left by EC *)
  pattern : int;  (** 32 dots, bit 31 leftmost; MAG already doubled *)
  attr : int;
      (** mode 1: the SAT colour byte. mode 2: this line's colour-table byte
          (bit7 EC, bit6 CC, bit5 IC, bits 0-3 colour). *)
}

type line = {
  hits : hit array;  (** shown sprites in SAT order: index 0 has priority *)
  overflow : int option;
      (** the first sprite that did not fit the 4 (mode 1) / 8 (mode 2)
          per-line limit *)
  scanned : int;  (** sprites before the Y terminator, 32 when none *)
}

val evaluate :
  mode -> regs:int array -> read:(int -> int) -> display_line:int -> line
(** Sprites on visible line [display_line] (0 = first line under the top
    border). The VDP checks line [display_line - 1 + R#23] and shows the
    result one line lower, so a sprite with Y = 0 starts on line 1 and
    Y = 255 on line 0. [read] maps a logical VRAM address to its byte. *)

val collision : mode -> regs:int array -> line -> int option
(** Leftmost on-screen dot where two shown sprites overlap. Colour-0
    sprites collide only while R#8 TP is set; in mode 2 a line with CC or IC
    set never collides. *)

val draw : mode -> regs:int array -> line -> (int -> int -> unit) -> unit
(** [draw mode ~regs line put] calls [put x colour] for every shown sprite
    dot with 0 <= x < 256, lowest priority first, so later calls overwrite.
    Colour 0 is skipped while R#8 TP is clear. In mode 2, CC=1 lines show
    only behind an earlier CC=0 sprite and OR their colour into it. *)
