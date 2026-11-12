(** Konami SCC 음원(SCC 메가ROM 카트 안의 칩) — 32바이트 파형 5채널.
    동작 정본은 openMSX src/sound/SCC.cc 의 Real 모드다. *)

type t

val create : unit -> t
(** 전원 직후: 파형 0xFF, 볼륨 15, 주기 0, 채널 모두 꺼짐. *)

val power_up : t -> unit
(** 전원 직후 상태로 되돌린다. *)

val write : t -> int -> int -> unit
(** [write t a v]: 레지스터 창 [a land 0xFF] 에 쓴다(CPU 0x9800-0x9FFF). *)

val read : t -> int -> int
(** 레지스터 창 읽기. 0x80 아래는 파형, 위는 0xFF. 0xE0 이상을 읽으면
    변형 레지스터에 0xFF 를 쓴 것과 같다. *)

val tick : t -> int
(** 믹서 틱(CPU 16 사이클) 하나를 진행하고 그 틱의 레벨을 돌려준다.
    샘플은 32 사이클마다 새로 나오니 두 틱이 같은 샘플을 낸다. 다섯
    채널의 합이고 범위는 -600..595 다. *)

val write_state : State_codec.writer -> t -> unit
(** 레지스터·카운터·변형 타이머 (세이브 v9). *)

val read_state : State_codec.reader -> t -> unit
