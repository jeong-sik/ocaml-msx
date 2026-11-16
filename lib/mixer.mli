(** 칩 틱(CPU 16 사이클) 레벨을 44.1 kHz 모노 16비트 PCM 으로 바꾼다.
    openMSX 소리 경로처럼 두 단계를 거친다.
    - 대역 제한: Kaiser 창 sinc 저역 필터(차단 17640 Hz, 반폭 24 출력
      주기)가 출력 시각 둘레의 틱 레벨을 읽는다.
    - DC 제거: [y(n) = x(n) - x(n-1) + (511/512) y(n-1)]. 오래 머무는
      레벨은 무음으로 줄어든다.
    끝에 2로 나누고(내림) 16비트 범위로 자른다. 출력은 칩보다 121.5틱
    (출력 약 24샘플) 늦다. *)

type t

val sample_rate : int
(** 출력 샘플률, 44100. *)

val cycles_per_tick : int
(** 틱 하나의 CPU 사이클, 16. *)

val taps : int
(** 출력 샘플 하나가 읽는 틱 수, 244. *)

val create : unit -> t

val ticks : t -> cycles:int -> int
(** CPU 사이클을 받아 그동안 지난 온전한 틱 수를 돌려준다. 16 사이클에
    못 미친 나머지는 다음 호출로 넘어간다. *)

val push : t -> int -> unit
(** 틱 하나의 레벨을 넣는다. 1/44100 초가 차면 출력 샘플을 버퍼에
    붙인다. *)

val clear_output : t -> unit
(** 출력 버퍼를 비운다. *)

val output : t -> string
(** 마지막 [clear_output] 뒤에 생긴 샘플. 부호 있는 16비트 리틀엔디언. *)

val write_state : State_codec.writer -> t -> unit
(** 남은 사이클·위상, 최근 [taps] 틱 레벨(오래된 것부터), 대역 제한 값과
    DC 필터 출력 (세이브 v10, PSG 상태 바로 뒤). *)

val read_state : State_codec.reader -> t -> unit
(** v8·v9 는 상자 평균의 남은 사이클·위상·누적값을 담았다. 필터 이력이
    없으니 무음에서 다시 시작한다. *)
