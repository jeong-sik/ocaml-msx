(** 칩 틱(CPU 16 사이클) 레벨을 44.1 kHz 모노 16비트 PCM 으로 바꾼다.
    출력 샘플은 직전 샘플 뒤 틱 레벨의 평균을 2로 나눈 값(내림)이다. *)

type t

val sample_rate : int
(** 출력 샘플률, 44100. *)

val cycles_per_tick : int
(** 틱 하나의 CPU 사이클, 16. *)

val create : unit -> t

val ticks : t -> cycles:int -> int
(** CPU 사이클을 받아 그동안 지난 온전한 틱 수를 돌려준다. 16 사이클에
    못 미친 나머지는 다음 호출로 넘어간다. *)

val push : t -> int -> unit
(** 틱 하나의 레벨을 더한다. 1/44100 초가 차면 출력 샘플을 버퍼에
    붙인다. *)

val clear_output : t -> unit
(** 출력 버퍼를 비운다. *)

val output : t -> string
(** 마지막 [clear_output] 뒤에 생긴 샘플. 부호 있는 16비트 리틀엔디언. *)

val write_state : State_codec.writer -> t -> unit
(** 남은 사이클·위상·누적값 (세이브 v8, PSG 상태 바로 뒤). *)

val read_state : State_codec.reader -> t -> unit
