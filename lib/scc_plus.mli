(** Konami SCC+ 사운드 카트 — SCC-I 칩과 RAM, ROM 없음. 동작 정본은 openMSX
    src/memory/MSXSCCPlusCart.cc 다. 상태(RAM, 매퍼 레지스터 4개, 모드
    레지스터)는 꽂은 쪽이 들고, 여기 함수가 CPU 접근을 풀어 준다. *)

(** 카트 종류. 매퍼 레지스터 하위 4비트 중 RAM 블록이 있는 값:
    [Snatcher] 0-7(64KB), [Sd_snatcher] 8-15(64KB), [Expanded] 0-15(128KB). *)
type kind = Snatcher | Sd_snatcher | Expanded

val ram_size : kind -> int
(** RAM 바이트 수. *)

val chip_mode : int -> Scc.mode
(** 모드 레지스터가 정하는 SCC-I 모드: bit 5 가 1 이면 [Plus], 아니면
    [Compatible]. *)

val read : kind -> ram:Bytes.t -> banks:int array -> mode:int -> Scc.t -> int -> int
(** 슬롯 주소 [a] 읽기. 0x4000-0xBFFF 밖과 블록 없는 레지스터 값은 0xFF.
    SCC 창이 열려 있으면 SCC 를 읽는다. *)

val write : kind -> ram:Bytes.t -> banks:int array -> mode:int -> Scc.t -> int -> int -> int
(** 슬롯 주소 [a] 에 쓰고 새 모드 레지스터 값을 돌려준다. 모드 레지스터를
    쓰면 SCC-I 모드도 바꾼다. RAM·매퍼 레지스터·SCC 는 그 자리에서 바뀐다. *)
