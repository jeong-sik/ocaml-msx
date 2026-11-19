(** 스프라이트 규칙 — VDP 구조체와 무관한 순수 함수. *)

type mode = Mode1 | Mode2

type env = {
  regs : int array;
  vr : int -> int;  (** 논리 VRAM 주소 → 바이트 *)
  mode : mode;
}

type row = {
  index : int;
  x0 : int;
  bits : int;
  colour : int;
  cc : bool;
  ic : bool;
}

val entries : env -> int list * int
(** 목록에 든 번호들과 목록을 끝낸 번호(종결자가 없으면 32). *)

val displayed : env -> int list -> int -> row list * int option
(** [displayed e indices line]: [line] 에 표시되는 스프라이트 행(번호 순)과,
    줄당 제한을 넘은 첫 스프라이트 번호. *)

val paint : env -> opaque0:bool -> row list -> int array -> unit
(** 한 줄의 픽셀을 길이 256 배열에 채운다. 투명은 -1. *)

type scan = { collided : bool; fifth : int option; stop : int }

val scan : env -> opaque0:bool -> lines:int -> scan
