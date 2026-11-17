(** Panasoft SW-M001 PAC SRAM 카트. 동작 정본은 openMSX
    src/memory/MSXPac.cc 다. 0x5FFE·0x5FFF 에 0x4D·0x69 를 쓰면 SRAM
    8190바이트가 0x4000-0x5FFD 에 열린다. 닫혀 있으면 모두 0xFF 를 읽고
    SRAM 쓰기는 버려진다. *)

type t

val sram_size : int
(** SRAM 크기, 0x1FFE(8190)바이트. *)

val file_size : int
(** openMSX PAC 파일 크기: 머리 16바이트 + SRAM, 8206바이트. *)

val create : unit -> t
(** 빈 PAC. SRAM 은 0xFF, 두 레지스터도 0xFF 라 닫혀 있다. *)

val read : t -> int -> int
(** page 1(0x4000-0x7FFF) 안의 주소를 읽는다. *)

val write : t -> int -> int -> unit
(** page 1 안의 주소에 쓴다. *)

val of_file : string -> (t, string) result
(** openMSX 형식("PAC2 BACKUP DATA" + SRAM) 파일 내용. 크기나 머리가
    틀리면 [Error]. 레지스터는 전원 투입 값(닫힘)이다. *)

val to_file : t -> string
(** openMSX 형식 파일 내용. *)

val write_state : State_codec.writer -> t -> unit
(** SRAM 과 두 레지스터. *)

val read_state : State_codec.reader -> t
(** SRAM 크기가 8190바이트가 아니면 상태를 거부한다. *)
