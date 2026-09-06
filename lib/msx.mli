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

val create : machine:machine -> t

val name : t -> string

val load_cartridge : t -> string -> unit

val set_key : t -> key -> pressed:bool -> unit

val step : t -> frames:int -> unit
(** [frames] 프레임만큼 진행. 한 프레임 = 262 라인 × 228 사이클. *)

val dump_pc : t -> int
(** 현재 PC — 부트 하네스 판정용. *)

val screen_text : t -> string
(** name table 을 40×24 (또는 32×24) 문자 그리드로 — 부트 판정용. *)

val debug_dump : t -> unit
(** VDP 레지스터·VRAM 통계·PPI 를 stderr 로 — 부트 디버깅용. *)

val frame_dims : t -> int * int
val frame_rgb : t -> string

val serialize : t -> string
(** 아직 없다 — P1 범위 밖. 호출하면 예외. *)

val restore : state:string -> t
