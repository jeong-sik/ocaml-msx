(** AY-3-8910 PSG — 톤 3개, 노이즈, 엔벨로프. 동작 정본은 openMSX
    src/sound/AY8910.cc 다. 칩 샘플 하나는 CPU 16 사이클이고, 44.1 kHz 로
    바꾸는 일은 {!Mixer} 가 한다. 결정론: 정수 연산만 쓴다. *)

type t

val create : unit -> t
(** 전원 직후: 레지스터 0, 노이즈 LFSR 1, 엔벨로프 모양 0. *)

val reg : t -> int -> int
(** R#0-R#15 에 마지막으로 쓴 값. *)

val write : t -> int -> int -> unit
(** [write t r v] 는 R#r 에 v 를 쓴다(0 <= r < 16). 주기·엔벨로프 모양이
    바로 반영된다. R#14·R#15 는 I/O 포트라 소리에 영향이 없다. *)

val chip_sample : t -> int
(** 지금 칩이 내는 레벨을 돌려주고 칩을 한 칩 샘플(CPU 16 사이클)
    진행한다. 0(무음)부터 24573(세 채널이 볼륨 15)까지. *)

val write_regs : State_codec.writer -> t -> unit
(** 레지스터 16개 — 세이브 v1 부터 있던 자리. *)

val read_regs : State_codec.reader -> t -> unit
(** 레지스터를 읽고 주기·엔벨로프 모양을 다시 계산한다. 카운터는
    [read_state] 가 덮어쓰지 않으면 전원 직후 값이다. *)

val write_state : State_codec.writer -> t -> unit
(** 카운터·LFSR·엔벨로프 위치 (세이브 v8). *)

val read_state : State_codec.reader -> t -> unit
(** [write_state] 가 쓴 값을 읽는다. 카운터 상한이 주기에 달려 있어
    [read_regs] 뒤에 부른다. 범위를 벗어나면 [State_codec.Invalid_state]. *)
