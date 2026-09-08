(** ocaml-msx 코어 계약.

    코어는 순수하다: 시간·파일·난수를 스스로 읽지 않는다. 프레임은
    {!step} 호출로만 진행한다. P0 의 Z80 과 P1 의 V9938(TMS 호환)·PPI·
    RAM 매퍼를 배선한 MSX2 머신이다.

    지금 시점의 경계: 스프라이트 렌더·command engine·사운드 출력·
    savestate 은 없다 (P2/P3). 인터럽트는 VBlank INT 하나만 나간다. *)

type key =
  | Up
  | Down
  | Left
  | Right
  | Space
  | Trigger_a
  | Trigger_b
  | Esc
  | Return
  | Function of int
  | Char of char

type machine = {
  ram_kb : int;  (** 64 / 128 / 256 / 512 *)
  vram_kb : int;  (** 128 (MSX2). 16 만 에뮬레이트됨 *)
  roms : string list;
      (** C-BIOS 순서: [main(32KB); logo(16KB); sub(16KB)] — 없는 것은
          빈 문자열. 카트리지는 {!load_cartridge}. *)
}

type t

(** MegaROM cartridge mapper. [Flat] is a plain 16/32KB cart; the others bank
    8KB/16KB windows the way the named hardware does (openMSX RomKonami /
    RomKonamiSCC / RomAscii8 / RomAscii16). SCC sound is not modelled. *)
type cart_mapper = Flat | Konami | Konami_scc | Ascii8 | Ascii16

val create : machine:machine -> t

val name : t -> string

val load_cartridge : ?mapper:cart_mapper -> t -> string -> unit
(** Plug in a cartridge. Without [mapper] the type is guessed from the ROM
    ({!guess_mapper}); pass it to override a wrong guess. The bank registers
    reset linear so the ROM boots from segment 0. *)

val cart_mapper : t -> cart_mapper
(** The mapper the plugged-in cart is using ([Flat] when none/plain). *)

val guess_mapper : string -> cart_mapper
(** Guess a ROM's mapper by counting its bank-register writes. [Flat] for a ROM
    of 32KB or less. A heuristic — the caller can override with
    [load_cartridge ~mapper]. *)

val palette_entries : t -> (int * int * int) array
(** 팔레트 레지스터 16색의 RGB (채널 확대 후). 부트 판정 재료. *)

val mem_read : t -> int -> int
(** Read the byte the Z80 sees at a 16-bit address (slots, mapper, and all).
    For tests and debugging. *)

val mem_write : t -> int -> int -> unit
(** Write a byte at a 16-bit address as the Z80 would — into RAM, or as a bank
    select in a MegaROM's cart window. For tests and debugging. *)

val load_disk : t -> string -> unit
(** Plug a floppy image (raw .dsk, 512 bytes a sector) into drive A. A disk
    interface ROM rides in the cartridge slot; C-BIOS finds its "AB" header and
    calls INIT, whose BIOS entries are HLE traps the step loop services against
    the image. *)

val set_disk_call_log : bool -> unit
(** Record each disk BIOS entry the running code reaches — for learning the
    convention a given .dsk expects. Off by default. *)

val disk_call_entries : unit -> (int * int * int * int * int * int) list
(** [(pc, a, bc, de, hl, f)] per disk BIOS entry, in order. *)

val set_key : t -> key -> pressed:bool -> bool
(** 논리 키를 누르거나 뗀다. 키보드 매트릭스 키와 조이스틱 1 버튼
    (Trigger_a/b) 은 true. 자리가 없는 키(표 밖 글자, F6 이상) 는 아무것도
    바꾸지 않고 false. 커서 키는 키보드 행 8 이고 조이스틱 방향은 배선하지
    않는다 — BIOS GTSTCK(0)/GTTRIG(0) 경로. *)

val port_in : t -> int -> int
val port_out : t -> int -> int -> unit
(** Z80 이 보는 I/O 포트를 직접 읽고 쓴다 — 키보드·PSG 계약 테스트용. *)

val step : t -> frames:int -> unit
(** [frames] 프레임만큼 진행. 한 프레임 = 262 라인 × 228 사이클. *)

val dump_pc : t -> int
(** 현재 PC — 부트 하네스 판정용. *)

val screen_text : t -> string
(** name table 을 40×24 (또는 32×24) 문자 그리드로 — 부트 판정용. *)

val set_watch_enter : int -> int -> unit
(** PC 가 [lo,hi) 에 처음 들어가면 직전 40스텝을 stderr 로. *)

val set_trace_from : int -> int -> unit
(** PC 가 [pc] 에 도달하면 그 시점부터 [n] 스텝을 stderr 로 트레이스. *)

val set_pc_hist : bool -> unit
val pc_histogram : unit -> int array
(** PC 상위 바이트별 실행 횟수 (256 버킷). *)

val set_ldirvm_log : bool -> unit
(** LDIRVM(0x005C) 호출 추적 on/off — 부트 디버깅용. *)

val ldirvm_log_calls : unit -> (int * int * int) list
(** (HL=src, DE=dst, BC=len) — set_ldirvm_log true 후 step 에 수집. *)

val set_watch_mem : int list -> unit
(** RAM 쓰기 감시 주소 목록 — 부팅 중 시스템 변수 초기화 추적용. *)

val watch_mem_entries : unit -> (int * int * int * int) list
(** (명령 서수, 주소, 값, 근접 PC) — 시간 순. PC 는 폴링된 사이클
    중간이라 정확한 명령 경계가 아니다. *)

val vram_hex : t -> int -> int -> unit
(** VRAM [from] 부터 [len] 바이트 hex 를 stderr 로. *)

val ram_hex : t -> int -> int -> unit
(** RAM [from] 부터 [len] 바이트 hex 를 stderr 로 — 페이지0 스왑 게임
    코드 해독용. 매퍼 세그먼트 0 기준. *)

val vdp_write_log : t -> (int * int * int) list
(** VDP 포트 쓰기 로그 — 부트 디버깅용. *)

val tx_state : t -> bool * int * int * int * int
(** (전송 중, transfer 횟수, 남은 줄, 줄 내 남은 바이트, 현재 Y). *)

val vdp_status0 : t -> int
val vdp_line : t -> int * int
(** S#0 현재값 — 부트 디버깅용. *)

val vdp_irq_active : t -> bool
(** VDP 인터럽트 라인 상태 — 부트 디버깅용. *)

val cpu_halted : t -> bool
(** Z80 HALT 상태 — 부트 디버깅용. *)

val vdp_regs : t -> int array
(** VDP 레지스터 — 부트 디버깅용. *)

val ppi_a : t -> int
(** PPI 포트 A (슬롯 셀렉트) — 부트 디버깅용. *)

val slot3_sel : t -> int
(** 슬롯3 서브슬롯 선택(0xFFFF) — 부트 디버깅용. *)

val cmd_history : t -> (int * int * int * int) list

val debug_dump : t -> unit
(** VDP 레지스터·VRAM 통계·PPI 를 stderr 로 — 부트 디버깅용. *)

val frame_dims : t -> int * int
val frame_rgb : t -> string

val serialize : t -> string
(** 아직 없다 — P1 범위 밖. 호출하면 예외. *)

val restore : state:string -> t

(** 관측 — 화면을 그리지 않는 클라이언트(텍스트 keeper)가 기계 상태를 읽는
    면. 프레임 픽셀은 {!frame_rgb}, 여기는 그 아래의 구조다. *)

type display_mode = Vdp.display_mode =
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
  | Undefined of int  (** M1=bit0 … M5=bit4 로 읽은 5비트 코드 *)

val display_mode : t -> display_mode
val display_mode_to_string : display_mode -> string

val vram_read : t -> int -> int
(** VRAM 한 바이트. name table(R#2<<10)·SAT(R#5<<7) 을 관측자가 직접 읽는다. *)
