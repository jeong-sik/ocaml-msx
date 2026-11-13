(** V9938 — TMS9918 호환 표면(TEXT1/TEXT2/MULTICOLOR/G1/G2/G3) + SCREEN5-8
    비트맵 렌더,
    레지스터·VRAM 래치, 팔레트, VBlank·라인 인터럽트.
    명령 엔진은 SCREEN5-8 에서만 돈다: STOP, POINT, PSET, SRCH, LINE, 블록
    명령(LMMV/HMMV/HMMM/YMMM/LMMM), CPU 동기 전송(LMMC/HMMC 는 R#44 쓰기,
    LMCM 은 S#7 읽기). 전송이 아닌 명령은 CMR 쓰기 안에서 끝난다. 명령은
    멈춘 자리를 레지스터에 남긴다: 블록 명령과 전송은 끝낸 줄마다 DY(원본을
    읽으면 SY 도)를 한 줄 옮기고 NY 를 하나 줄이며, LINE 은 DY 를 마지막
    자리에 둔다. 그래서 CPU 가 일부 레지스터만 다시 쓰고 CMR 을 쓰면 다음
    명령이 그 자리에서 이어진다.
    스프라이트 모드 1·2 는 {!Vdp_sprite} 가 그리고, [advance] 가 표시 라인마다
    S#0 의 5S/9S·충돌 비트와 S#3-S#6 충돌 좌표를 갱신한다. *)

type t

val create : unit -> t

val io_write : t -> port:int -> int -> unit
(** 0x98 VRAM 데이터 쓰기 / 0x99 주소·레지스터 래치 / 0x9A 팔레트
    인덱스 / 0x9B 팔레트 데이터. *)

val io_read : t -> port:int -> int
(** 0x98 VRAM 읽기(버퍼) / 0x99 R#15 가 고른 상태 레지스터. S#0 을 읽으면
    VBlank 비트와 인터럽트 요청이 지워진다. S#2 는 CE·TR·BD, S#7 은 COL
    (POINT·LMCM 의 점; 읽으면 LMCM 이 다음 점을 꺼낸다), S#8/S#9 는 ASX
    (SRCH 가 멈춘 X), S#9 를 읽으면 BD 가 지워진다. S#2 의 HR 은 줄
    경계(오른쪽 테두리)부터 52 사이클(텍스트 모드 68) 동안 1, VR 은
    VBlank 부터 첫 표시 줄 바로 윗줄의 48 사이클째까지 1, EO 는 프레임마다
    뒤집힌다. *)

val advance : t -> cycles:int -> bool
(** 경과 T-state 만큼 스캔라인을 진행한다. 표시 라인을 벗어날 때마다 그
    순간의 레지스터·팔레트·VRAM 으로 그 라인을 그린다. 라인
    (R#19 - R#23) mod 256 을 벗어날 때 IE1 이면 라인 인터럽트, VBlank
    시작(라인 192/212)에서 프레임을 확정하고 수직 인터럽트를 건다. 둘 중
    하나가 걸리면 [true]. 한 프레임은 262 줄이고, 줄 카운터가 0 으로
    돌아갈 때 R#9 bit 1 이 켜져 있으면 그다음 프레임부터 313 줄(PAL)이다.
    화면 모드(R#0 M3-M5, R#1 M1-M2)·화면 끔(R#1 BL)·스프라이트 끔(R#8 SPD)
    비트는 쓴 즉시가 아니라 다음 줄 시작(줄 경계 뒤 39 사이클, 텍스트 모드
    43)에서 적용된다(openMSX syncAtNextLine). 그 뒤로 그리기·스프라이트
    검사·명령 엔진·CPU 의 planar 주소가 새 모드를 따른다. *)

val int_active : t -> bool
(** 인터럽트 라인 상태 — IE 가 켜져 있고 status0 비트7 이 서 있는 동안. *)

val frame_dims : t -> int * int
(** Size of the frame the last VBlank published: native width (TEXT2 and
    SCREEN6/7: 512, the other modes 256) and R#9 LN height (192/212) in
    every mode. A fresh VDP shows a black 256×192 frame. *)

val frame_rgb : t -> string
(** The frame the last VBlank published, {!frame_dims} RGB (row-major,
    R,G,B). Each line holds what was scanned out for it, so changes made
    mid-frame (scroll, palette, mode) show from the line they reached.
    A line drawn at the other width is doubled or halved to the frame's.
    The frame is the display area at its neutral place: R#18 set-adjust
    moves the display inside it (horizontally per line, vertically as
    latched at the line counter reset) and the uncovered edge shows the
    border colour.
    BLANK(R1 bit6) 이 꺼진 라인은 검다. *)

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

val status0 : t -> int
(** S#0 현재값 — 인터럽트 디버깅용. *)

val line_now : t -> int * int
(** (현재 라인, 라인 내 사이클) — 인터럽트 미발사 진단용. *)

val write_log : t -> (int * int * int) list
(** 최근 4096 포트 쓰기 (port, 래치주소, 값) — 부트 디버깅용. *)

(** V9938 화면 모드 — R#0 bits1-3 (M3 M4 M5) 와 R#1 bits3-4 (M2 M1). 표에
    없는 조합은 5비트 코드(M1=bit0 … M5=bit4)를 든 [Undefined]. *)
type display_mode =
  | Text1
  | Text2
  | Multicolor
  | Graphic1
  | Graphic2
  | Graphic3
  | Graphic4
  | Graphic5
  | Graphic6
  | Graphic7
  | Undefined of int

val display_mode : t -> display_mode
val display_mode_to_string : display_mode -> string

val vram_read : t -> int -> int
(** VRAM 한 바이트 (주소는 17비트로 감싼다) — 관측자용. *)

(** Internal state codec; read only into a fresh unpublished VDP.
    Debug write history and global diagnostic counters are not execution state. *)
val write_state : State_codec.writer -> t -> unit

val read_state : State_codec.reader -> t -> unit

val write_appended : State_codec.writer -> t -> unit
(** State added after format v9, written after the sound chips: R#13
    blinking (v10) and the latched R#18 vertical adjust (v11). *)

val read_appended : State_codec.reader -> t -> unit
(** Reads what the state's format holds of {!write_appended}; call it
    after {!read_state}. *)
