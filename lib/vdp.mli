(** V9938 — TMS9918 호환 표면(텍스트/G1/G2) + SCREEN5(G4) 비트맵 렌더,
    블록 명령(LMMV/HMMV/HMMM/YMMM/LMMM) 과 CPU 동기 전송(LMMC/HMMC),
    레지스터·VRAM 래치, 팔레트, VBlank·라인 인터럽트.
    SCREEN6-8·스프라이트·POINT/SRCH/LINE 은 아직 없다. *)

type t

val create : unit -> t

val io_write : t -> port:int -> int -> unit
(** 0x98 VRAM 데이터 쓰기 / 0x99 주소·레지스터 래치 / 0x9A 팔레트
    인덱스 / 0x9B 팔레트 데이터. *)

val io_read : t -> port:int -> int
(** 0x98 VRAM 읽기(버퍼) / 0x99 status0 — 읽으면 VBlank 비트와
    인터럽트 요청이 지워진다. *)

val advance : t -> cycles:int -> bool
(** 경과 T-state 만큼 스캔라인을 진행한다. VBlank 시작(라인 192)에서
    반환값이 [true] 가 되고 인터럽트 요청이 걸린다 — 호출자는 Z80 에
    INT 를 걸고, 스크린 표시 기간 동안엔 이 신호가 유지된다. *)

val int_active : t -> bool
(** 인터럽트 라인 상태 — IE 가 켜져 있고 status0 비트7 이 서 있는 동안. *)

val frame_rgb : t -> string
(** 현재 VRAM 을 256×192 RGB (row-major, 채널 R,G,B) 로 렌더.
    BLANK(R1 bit6) 꺼짐이면 검은 화면. *)

val vram : t -> Bytes.t
(** VRAM 원본 — 하네스 검사용. *)

val palette_rgb : t -> int -> int * int * int
(** 팔레트 색 인덱스의 RGB — 렌더 판정 하네스용. *)

val tx_state : t -> bool * int * int * int * int
(** (전송 중, transfer 횟수, 남은 줄, 줄 내 남은 바이트, 현재 Y). *)

val cmd_history : t -> (int * int * int * int) list
(** 최근 명령 발행 (CMR, DX, DY, NY) 32개 — 시간 순. *)

val regs : t -> int array
(** 레지스터 0-46 — 하네스 검사용. *)

val write_log : t -> (int * int * int) list
(** 최근 4096 포트 쓰기 (port, 래치주소, 값) — 부트 디버깅용. *)
