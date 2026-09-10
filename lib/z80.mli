(** Z80 CPU 코어 — 해석형, T-state 카운터 포함.

    메모리는 코어 밖에 있다: 생성 시 read/write 콜백을 받는다. MSX 의 슬롯
    배선(P1)도 이 경로로 들어온다. 결정론: 같은 상태 + 같은 메모리 + 같은
    입력 = 같은 실행 — [t_states] 누적으로 zexall 사이클 기준값과 대조한다.

    인터럽트는 아직 없다 (IFF/IM 필드만 유지). P1 에서 NMI/INT 주입이
    붙는 자리다. 인터럽트 없이 zexall 은 완주한다. *)

type t

val create :
  read:(int -> int) ->
  write:(int -> int -> unit) ->
  port_in:(int -> int) ->
  port_out:(int -> int -> unit) ->
  t

val step : t -> int
(** 한 명령을 실행하고 그 명령이 쓴 T-state 를 반환. HALT 상태면 아무
    것도 하지 않고 4 를 반환한다. *)

val set_pc : t -> int -> unit
(** 하네스가 TPA 시작(0x100)을 강제하는 자리. 이미지 헤더의 JP 를 믿지
    않는다 — CP/M 계약은 로드 주소가 시작 주소다. *)

val interrupt : t -> bool
(** INT 라인이 활성일 때 호출. IM1 이면 RST 38h 로 점프하고 IFF 를
    끈다. [false] 는 인터럽트를 받지 않았음 (IFF1 꺼짐·EI 직후 지연) —
    호출자는 라인을 유지해 다시 시도할 수 있다. HALT 중이면 깨운다.
    NMI 는 아직 없다. *)

val halted : t -> bool

val t_states : t -> int
(** 생성 이후 누적 T-state. *)

val dump_pc : t -> int
(** 현재 PC. 하네스 판정용. *)

val dump_a : t -> int

(** 유닛 벡터/디버그용 레지스터 세터. 에뮬 코어의 정당한 디버그 표면 —
    하네스가 시드를 심고 플래그를 읽는다. *)
val set_af : t -> int -> unit
val set_bc : t -> int -> unit
val set_de : t -> int -> unit
val set_hl : t -> int -> unit
val set_sp : t -> int -> unit
val dump_bc : t -> int
val dump_de : t -> int
val dump_hl : t -> int
val dump_f : t -> int
val dump_iff1 : t -> bool
val dump_ix : t -> int
val dump_iy : t -> int
val set_ix : t -> int -> unit
val set_iy : t -> int -> unit
val dump_sp : t -> int

(** Internal state codec; read only into a fresh unpublished CPU. *)
val write_state : State_codec.writer -> t -> unit

val read_state : State_codec.reader -> t -> unit
