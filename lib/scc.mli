(** Konami SCC 음원 — 32바이트 파형 5채널. 동작 정본은 openMSX
    src/sound/SCC.cc 다. SCC 메가ROM 카트의 칩은 Real 모드, SCC+ 사운드
    카트의 SCC-I 는 Compatible·Plus 모드로 돈다. *)

type t

(** 레지스터 배치. Real: 파형 1-4(0x00), 주기·볼륨(0x80), 변형(0xE0).
    Compatible: Real 과 같고 변형만 0xC0, 0xA0 에서 파형 5 를 읽는다.
    Plus: 파형 1-5(0x00), 주기·볼륨(0xA0), 변형(0xC0) — 채널 5 가 자기
    파형을 가진다. *)
type mode = Real | Compatible | Plus

val create : unit -> t
(** 전원 직후: Real 모드, 파형 0xFF, 볼륨 15, 주기 0, 채널 모두 꺼짐. *)

val set_mode : t -> mode -> unit
(** 칩 모드를 바꾼다. 꽂힌 카트가 정한다. *)

val power_up : t -> unit
(** 전원 직후 상태로 되돌린다. SCC-I(Compatible·Plus)는 Compatible 로. *)

val write : t -> int -> int -> unit
(** [write t a v]: 레지스터 창 [a land 0xFF] 에 쓴다(CPU 0x9800-0x9FFF,
    SCC+ 모드면 0xB800-0xBFFF). *)

val read : t -> int -> int
(** 레지스터 창 읽기. 파형 자리 밖은 0xFF. 변형 레지스터 자리를 읽으면
    거기 0xFF 를 쓴 것과 같다. *)

val tick : t -> int
(** 믹서 틱(CPU 16 사이클) 하나를 진행하고 그 틱의 레벨을 돌려준다.
    샘플은 32 사이클마다 새로 나오니 두 틱이 같은 샘플을 낸다. 다섯
    채널의 합이고 범위는 -600..595 다. *)

val write_state : State_codec.writer -> t -> unit
(** 레지스터·카운터·변형 타이머 (세이브 v9). 칩 모드는 넣지 않는다 — 꽂힌
    카트에서 다시 정한다({!set_mode}). *)

val read_state : State_codec.reader -> t -> unit
