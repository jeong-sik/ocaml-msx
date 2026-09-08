(* MSX2 머신 배선 (C-BIOS 구성):
   슬롯0 = main ROM 32KB(페이지0·1) + logo ROM 16KB(페이지2)
   슬롯3-0 = sub ROM 16KB, 슬롯3-2 = RAM 매퍼(512KB)
   PPI A(0xA8) 가 페이지→슬롯을, 0xFFFF 가 슬롯3 의 2차를 고른다.
   포트: VDP 0x98-0x9B, PPI 0xA8-0xAB, PSG 0xA0-0xA2, 매퍼 0xFC-0xFF. *)

let cycles_per_frame = 262 * 228

type key =
  | Up | Down | Left | Right | Space | Trigger_a | Trigger_b
  | Esc | Return | Function of int | Char of char

(* 논리 키가 닿는 자리. 키보드 매트릭스 (행 0-10, 비트 0-7) 의 정본은
   openMSX share/unicodemaps/unicodemap.int (국제 배열, <ROW><COL>).
   글자는 대소문자를 같은 키로 본다 — SHIFT 는 매트릭스의 다른 키다.
   Trigger_a/b 는 조이스틱 1 의 버튼이라 PSG R#14 로 읽히고, 표에 없는
   글자와 F6 이상은 Unmapped. *)
type key_target = Matrix of int * int | Joy1_button of int | Unmapped

let key_rows = 11

let key_target = function
  | Up -> Matrix (8, 5)
  | Down -> Matrix (8, 6)
  | Left -> Matrix (8, 4)
  | Right -> Matrix (8, 7)
  | Space -> Matrix (8, 0)
  | Esc -> Matrix (7, 2)
  | Return -> Matrix (7, 7)
  | Function 1 -> Matrix (6, 5)
  | Function 2 -> Matrix (6, 6)
  | Function 3 -> Matrix (6, 7)
  | Function 4 -> Matrix (7, 0)
  | Function 5 -> Matrix (7, 1)
  | Function _ -> Unmapped
  | Trigger_a -> Joy1_button 4
  | Trigger_b -> Joy1_button 5
  | Char c -> (
    match Char.uppercase_ascii c with
    | '0' .. '7' as d -> Matrix (0, Char.code d - Char.code '0')
    | '8' -> Matrix (1, 0)
    | '9' -> Matrix (1, 1)
    | '-' -> Matrix (1, 2)
    | '=' -> Matrix (1, 3)
    | '\\' -> Matrix (1, 4)
    | '[' -> Matrix (1, 5)
    | ']' -> Matrix (1, 6)
    | ';' -> Matrix (1, 7)
    | '\'' -> Matrix (2, 0)
    | '`' -> Matrix (2, 1)
    | ',' -> Matrix (2, 2)
    | '.' -> Matrix (2, 3)
    | '/' -> Matrix (2, 4)
    | 'A' -> Matrix (2, 6)
    | 'B' -> Matrix (2, 7)
    | 'C' .. 'J' as l -> Matrix (3, Char.code l - Char.code 'C')
    | 'K' .. 'R' as l -> Matrix (4, Char.code l - Char.code 'K')
    | 'S' .. 'Z' as l -> Matrix (5, Char.code l - Char.code 'S')
    | ' ' -> Matrix (8, 0)
    | _ -> Unmapped)

type machine = { ram_kb : int; vram_kb : int; roms : string list }

(* MegaROM cartridge mappers. A cart larger than 32KB (or a 32KB one built for a
   mapper) can't sit flat in the 0x4000-0xBFFF window; it shows one of its
   segments per window and swaps them when the running code writes a bank
   register. [Flat] is the plain 16/32KB cart with no banking. The four here are
   the common ones (openMSX RomKonami / RomKonamiSCC / RomAscii8 / RomAscii16);
   the SCC sound chip is not modelled, only its banking. *)
type cart_mapper = Flat | Konami | Konami_scc | Ascii8 | Ascii16

(* WD279x 근사 상태. 즉시-완료 모델: 명령을 받으면 상태 읽기 한 번 안에
   seek/read 준비를 끝낸다 — 실기 타이밍의 근사이고, 로더가 상태 비트를
   폴링하는 한 관측상 같다. [buf] 는 READ SECTOR 가 채운 섹터 한 개. *)
type fdc_state = {
  mutable cmd : int;         (** 마지막 명령 바이트 — 상태 산출용 *)
  mutable track : int;       (** 트랙 레지스터 *)
  mutable sector : int;      (** 섹터 레지스터 *)
  mutable data : int;        (** 데이터 레지스터 (seek 목적지 포함) *)
  mutable side : int;        (** 0xD4 bit1 *)
  mutable motor : bool;      (** 0xD4 bit3 *)
  mutable busy : bool;
  mutable drq : bool;
  mutable intr : bool;
  buf : Bytes.t;             (** READ SECTOR 버퍼, 512 *)
  mutable pos : int;
}

type t = {
  cpu : Z80.t;
  vdp : Vdp.t;
  mutable main_rom : Bytes.t;
  mutable logo_rom : Bytes.t;
  mutable sub_rom : Bytes.t;
  mutable cart : Bytes.t;
  mutable cart_mapper : cart_mapper;
  cart_banks : int array;
      (** four 8KB bank registers. Konami/SCC/ASCII8 use all four (one per 8KB
          window); ASCII16 uses [.(0)]/[.(1)] as 16KB banks. *)
  mutable disk : Dsk.t option;  (** 플로피 이미지 — DSKIO 트랩이 서비스 *)
  mutable fdc : fdc_state;
      (** WD279x 컨트롤러 근사: 명령/상태, 트랙·섹터 레지스터, 섹터 버퍼.
          포트 0xD0-0xD4 로 로더가 직접 말을 건다. *)
  ram : Bytes.t;
  mutable mapper : int array;
  mutable ppi_a : int;
  mutable ppi_c : int;
  mutable slot3_sel : int;
  keys : bool array;  (** key_rows × 8, 눌림=true *)
  psg : int array;
  mutable psg_latch : int;
  mutable joy1 : int;  (** 조이스틱 1 입력 6비트, active low — 빈 포트 0x3F *)
  cpu_ref : t option ref;
}

let rom_or_empty = function "" -> Bytes.make 0x4000 '\000' | s -> Bytes.of_string s

(* Which 8KB segment a MegaROM shows in window [w] (0..3, one per 8KB of
   0x4000-0xBFFF). The 8KB mappers read the register straight; ASCII16 splits a
   16KB bank into its two 8KB halves. *)
let cart_seg8 m w =
  match m.cart_mapper with
  | Konami | Konami_scc | Ascii8 -> m.cart_banks.(w)
  | Ascii16 -> (m.cart_banks.(w lsr 1) lsl 1) lor (w land 1)
  | Flat -> w

(* A write into the cart window selects a bank. Each mapper decodes the target
   address differently; a write that hits no register is ignored (ROM is not
   RAM). Segment values are masked to the ROM size at read time. *)
let cart_bank_write m a v =
  match m.cart_mapper with
  | Flat -> ()
  | Konami ->
    (* 0x4000-0x5FFF fixed to segment 0; the upper three windows select. *)
    (match a land 0xe000 with
     | 0x6000 -> m.cart_banks.(1) <- v
     | 0x8000 -> m.cart_banks.(2) <- v
     | 0xa000 -> m.cart_banks.(3) <- v
     | _ -> ())
  | Konami_scc ->
    (* Register at 0x5000/0x7000/0x9000/0xB000: (a land 0x1800) = 0x1000. *)
    if a land 0x1800 = 0x1000 then m.cart_banks.((a lsr 13) - 2) <- v
  | Ascii8 ->
    (match a land 0xf800 with
     | 0x6000 -> m.cart_banks.(0) <- v
     | 0x6800 -> m.cart_banks.(1) <- v
     | 0x7000 -> m.cart_banks.(2) <- v
     | 0x7800 -> m.cart_banks.(3) <- v
     | _ -> ())
  | Ascii16 ->
    (match a land 0xf800 with
     | 0x6000 -> m.cart_banks.(0) <- v
     | 0x7000 -> m.cart_banks.(1) <- v
     | _ -> ())

(* Guess a mapper from the ROM by looking at where its code writes bank
   registers ([ld (nn),a]). Only for ROMs over 32KB; smaller carts sit flat.

   The decision is by *distinctive* register, not raw count: an ASCII8 game also
   writes 0x6000/0x7000 (those are two of its four registers), so a plain count
   mistakes it for ASCII16. But 0x6800/0x7800 are ASCII8's alone, 0x5000/0x9000/
   0xB000 are Konami-SCC's alone, and 0x8000/0xA000 are Konami's alone. ASCII16
   owns no unique register, so it is the fallback when 0x6000/0x7000 are written
   with none of the above. Konami is the last-resort default. The guess can be
   wrong; [load_cartridge ~mapper] overrides it. *)
let guess_mapper rom =
  let len = String.length rom in
  if len <= 0x8000 then Flat
  else begin
    let a8 = ref 0 and scc = ref 0 and konami = ref 0 and a16 = ref 0 in
    for i = 0 to len - 3 do
      if Char.code rom.[i] = 0x32 then begin
        let addr = Char.code rom.[i + 1] lor (Char.code rom.[i + 2] lsl 8) in
        (match addr with 0x6800 | 0x7800 -> incr a8 | _ -> ());
        (match addr with 0x5000 | 0x9000 | 0xb000 -> incr scc | _ -> ());
        (match addr with 0x8000 | 0xa000 -> incr konami | _ -> ());
        (match addr with 0x6000 | 0x7000 -> incr a16 | _ -> ())
      end
    done;
    if !a8 > 0 && !a8 >= !scc && !a8 >= !konami then Ascii8
    else if !scc > 0 && !scc >= !konami then Konami_scc
    else if !konami > 0 then Konami
    else if !a16 > 0 then Ascii16
    else Konami
  end

(* 슬롯1 0x4000-0x7FFF: "AB" 헤더, 0x4002 장치 코드 0, 나머지 RET. *)
let disk_rom_bytes =
  let b = Bytes.make 0x4000 '\xc9' in
  Bytes.set b 0 'A';
  Bytes.set b 1 'B';
  Bytes.set b 2 '\x00';
  Bytes.set b 3 '\xc9';
  b

let mem_read m addr =
  let a = addr land 0xffff in
  let page = a lsr 14 in
  let slot =
    match page with
    | 0 -> m.ppi_a land 3
    | 1 -> (m.ppi_a lsr 2) land 3
    | 2 -> (m.ppi_a lsr 4) land 3
    | _ -> (m.ppi_a lsr 6) land 3
  in
  if a = 0xffff && slot = 3 then lnot m.slot3_sel land 0xff
  else if
    a >= 0x4000 && a < 0xc000 && slot = 2 && m.cart_mapper <> Flat
    && Bytes.length m.cart > 0
  then begin
    (* MegaROM: 0x4000-0xBFFF is four 8KB windows, each showing the segment its
       bank register selects. The cart sits in slot 2 (see load_cartridge). *)
    let seg = cart_seg8 m ((a lsr 13) - 2) in
    let off = (seg * 0x2000) + (a land 0x1fff) in
    Char.code (Bytes.get m.cart (off mod Bytes.length m.cart))
  end
  else begin
    let off = a land 0x3fff in
    match slot with
    | 0 | 1 | 2 ->
      (* 32KB 카트리지 파일: 앞 16KB 가 페이지1(0x4000), 뒷 16KB 가
         페이지2(0x8000) — 페이지 내 주소 off 에 페이지2 면 0x4000 을
         더해 파일 뒷반을 읽는다 (앞반을 다시 보면 미러가 된다). *)
      let cart_off = if page = 2 then 0x4000 + off else off in
      let rom, roff =
        match slot, page with
        | 0, 0 | 0, 1 -> (m.main_rom, off)
        | 1, 1 when Option.is_some m.disk ->
        (* 디스크 장착 중 슬롯1 페이지1 은 가상 DISK BIOS: "AB" 시그니처와
           엔트리들. 실행은 트랩이 진입을 가로채니 바이트는 RET — 놓친 호출이
           조용히 복귀하는 것으로 끝난다. *)
        (disk_rom_bytes, off)
    | 1, 0 | 1, 1 -> (m.main_rom, off)
        (* calslt 가 init 호출 시 전 페이지를 카트리지 슬롯으로 스왑하므로
           부트 초반 로고(슬롯0 페이지2)와는 시점이 갈린다. *)
        | 0, 2 | 1, 2 -> (m.logo_rom, off)
        | 2, 2 ->
          (* 32KB 카트는 뒷 16KB. 16KB 카트는 A15 를 해독하지 않아 페이지2 에
             앞 16KB 가 다시 보인다 (미러). 경계는 >= — 정확히 0x8000 인
             파일이 로고 ROM 으로 새는 게 스펠렁커 halt 사태의 원인이었다. *)
          if Bytes.length m.cart >= 0x8000 then (m.cart, cart_off)
          else if Bytes.length m.cart >= 0x4000 then (m.cart, off)
          else (m.logo_rom, off)
        | 2, 0 | 2, 1 ->
          if Bytes.length m.cart >= 0x4000 then (m.cart, cart_off)
          else (m.main_rom, off)
        | 2, 3 | 1, 3 | 0, 3 -> (m.main_rom, off)
        | _ -> (m.logo_rom, off)
      in
      if Bytes.length rom = 0 then 0xff
      else Char.code (Bytes.get rom (min roff (Bytes.length rom - 1)))
    | _ ->
      (* 슬롯3: 2차 선택 (0xFFFF 하위 2비트×4, 여기선 전 페이지 단일값). *)
      if m.slot3_sel land 3 = 0 && page <> 3 then
        (* sub ROM 은 페이지0·1 자리. *)
        Char.code (Bytes.get m.sub_rom off)
      else begin
        let seg = m.mapper.(page) in
        let base = ((seg * 0x4000) + off) mod (Bytes.length m.ram) in
        Char.code (Bytes.get m.ram base)
      end
  end

let mem_write m addr v =
  let a = addr land 0xffff in
  let page = a lsr 14 in
  let slot =
    match page with
    | 0 -> m.ppi_a land 3
    | 1 -> (m.ppi_a lsr 2) land 3
    | 2 -> (m.ppi_a lsr 4) land 3
    | _ -> (m.ppi_a lsr 6) land 3
  in
  if a = 0xffff && slot = 3 then m.slot3_sel <- v land 0xff
  else if
    slot = 2 && a >= 0x4000 && a < 0xc000 && m.cart_mapper <> Flat
  then
    (* A write into the cart window is a bank select, not a store. *)
    cart_bank_write m a (v land 0xff)
  else if slot = 3 && (m.slot3_sel land 3 = 2 || page = 3) then begin
    let seg = m.mapper.(page) in
    let base = ((seg * 0x4000) + (a land 0x3fff)) mod (Bytes.length m.ram) in
    Bytes.set m.ram base (Char.chr (v land 0xff))
  end

let rtc_reg = ref 0

(* PSG R#14 = 조이스틱 포트 입력. R#15 bit6 이 포트 선택(0 = 1번), 2번 포트는
   비어 있다. bit6 = 키배열 점퍼(50on = 0), bit7 = 카세트 입력(0).
   정본: openMSX MSXPSG::readA, DummyJoystick::read = 0x3F. *)
let joystick_idle = 0x3f

let psg_read m =
  match m.psg_latch with
  | 14 -> if m.psg.(15) land 0x40 = 0 then m.joy1 else joystick_idle
  | r when r < 16 -> m.psg.(r)
  | _ -> 0xff

(* ---------- FDC (WD279x 근사) — 포트 0xD0-0xD4 ----------

   정본은 openMSX src/fdc/WD2793.cc. 즉시-완료 모델: RESTORE/SEEK 은 상태
   읽기 한 번 안에 끝나고, READ SECTOR 는 명령 받는 순간 버퍼를 채워
   DRQ 를 올린다 — 로더가 상태 비트를 폴링하는 한 실기와 관측이 같다.
   논리 섹터 = (track*2 + side)*9 + (sector-1). *)

let fdc_log : (int * int * int) array = Array.make 256 (0, 0, 0)
let fdc_log_i = ref 0
let fdc_note kind port v =
  fdc_log.(!fdc_log_i land 255) <- (kind, port, v);
  incr fdc_log_i

let fdc_status m =
  let f = m.fdc in
  (* READ 계열 진행 중이면 DRQ, 아니면 ready(0). not-ready 는 디스크가
     없을 때만 — 있으면 언제나 ready. *)
  let base = if Option.is_none m.disk then 0x80 else 0x00 in
  let drq = if f.drq then 0x02 else 0x00 in
  let busy = if f.busy then 0x01 else 0x00 in
  base lor drq lor busy

let fdc_load_sector m =
  let f = m.fdc in
  match m.disk with
  | None -> f.drq <- false
  | Some d -> (
      let logical = ((f.track * 2) + f.side) * 9 + (f.sector - 1) in
      match Dsk.read_sector d logical with
      | Some (b : string) ->
          Bytes.blit_string b 0 f.buf 0 (String.length b);
          f.pos <- 0;
          f.drq <- true;
          f.busy <- true
      | None -> f.drq <- false)

let fdc_write m port v =
  let f = m.fdc in
  match port land 0xff with
  | 0xD0 ->
      fdc_note 1 0xD0 v;
      f.cmd <- v land 0xf0;
      (match v land 0xf0 with
       | 0x00 -> f.track <- 0; f.intr <- true (* RESTORE *)
       | 0x10 -> f.track <- f.data; f.intr <- true (* SEEK *)
       | 0x80 | 0x90 | 0xA0 | 0xB0 -> fdc_load_sector m (* READ SECTOR *)
       | 0xC0 -> f.intr <- true (* READ ADDRESS — 최소 *)
       | 0xD0 -> f.busy <- false; f.intr <- true (* FORCE INTERRUPT *)
       | _ -> f.intr <- true)
  | 0xD1 -> f.track <- v land 0xff
  | 0xD2 -> f.sector <- v land 0xff
  | 0xD3 -> f.data <- v land 0xff
  | 0xD4 ->
      (* 시스템 컨트롤: bit1 side, bit3 motor (배선은 로더 로그로 맞춘다). *)
      fdc_note 1 0xD4 v;
      f.side <- (v lsr 1) land 1;
      f.motor <- v land 0x08 <> 0
  | _ -> ()

let fdc_read m port =
  let f = m.fdc in
  match port land 0xff with
  | 0xD0 ->
      let st = fdc_status m in
      fdc_note 0 0xD0 st;
      (* 상태 읽기가 seek 완료를 소비한다 — 즉시-완료 모델의 표현. *)
      f.busy <- false;
      st
  | 0xD1 -> f.track
  | 0xD2 -> f.sector
  | 0xD3 ->
      let b = Char.code (Bytes.get f.buf f.pos) in
      if f.pos < 511 then f.pos <- f.pos + 1
      else begin
        f.drq <- false;
        f.intr <- true
      end;
      fdc_note 0 0xD3 b;
      b
  | _ -> 0xff

let fdc_recent_calls () =
  Array.init (min !fdc_log_i 256) (fun k -> fdc_log.((!fdc_log_i - min !fdc_log_i 256 + k) land 255))

let port_read m port =
  match port land 0xff with
  | 0xD0 | 0xD1 | 0xD2 | 0xD3 -> fdc_read m port
  | 0x98 -> Vdp.io_read m.vdp ~port:0x98
  | 0x99 -> Vdp.io_read m.vdp ~port:0x99
  | 0xA8 -> m.ppi_a
  | 0xA9 ->
    (* 키보드: PPI C 하위 4비트가 행(0-10), B 가 그 행의 8키 (눌림 = 0). *)
    let row = m.ppi_c land 0x0f in
    if row >= key_rows then 0xff
    else begin
      let bits = ref 0xff in
      for b = 0 to 7 do
        if m.keys.((row * 8) + b) then bits := !bits land lnot (1 lsl b)
      done;
      !bits
    end
  | 0xAA -> m.ppi_c
  | 0xFC | 0xFD | 0xFE | 0xFF -> m.mapper.(port land 3)
  | 0xA2 -> psg_read m
  | _ -> 0xff

let port_write m port v =
  match port land 0xff with
  | 0xD0 | 0xD1 | 0xD2 | 0xD3 | 0xD4 -> fdc_write m port v
  | 0x98 | 0x99 | 0x9A | 0x9B -> Vdp.io_write m.vdp ~port:(port land 0xff) v
  | 0xA8 -> m.ppi_a <- v land 0xff
  | 0xAA -> m.ppi_c <- v land 0xff
  | 0xA0 -> m.psg_latch <- v land 0xff
  | 0xA1 -> if m.psg_latch < 16 then m.psg.(m.psg_latch) <- v land 0xff
  | 0xFC | 0xFD | 0xFE | 0xFF -> m.mapper.(port land 3) <- v land 0x3f
  | 0xB4 -> rtc_reg := v
  | _ -> ()

(* 메모리 쓰기 감시 — write 클로저가 참조하므로 create 보다 앞에. *)
let watch_mem_on = ref false
let watch_mem_addrs : int list ref = ref []
let watch_mem_log : (int * int * int * int) list ref = ref []
let instr_count = ref 0

let create ~machine =
  let main = match machine.roms with x :: _ -> x | [] -> "" in
  let logo = match machine.roms with _ :: x :: _ -> x | _ -> "" in
  let sub = match machine.roms with _ :: _ :: x :: _ -> x | _ -> "" in
  let ram_kb = max 64 machine.ram_kb in
  let m_ref : t option ref = ref None in
  let read a = match !m_ref with Some m -> mem_read m a | None -> 0xff in
  let write a v =
    match !m_ref with
    | Some m ->
      mem_write m a v;
      (* 감시 주소 쓰기 기록 — 명령 경계가 아니라 폴링된 사이클 중이라
         PC 는 근접 위치다. *)
      if !watch_mem_on && List.mem a !watch_mem_addrs then
        watch_mem_log :=
          (!instr_count, a, v land 0xff, Z80.dump_pc m.cpu) :: !watch_mem_log
    | None -> () in
  let pin p = match !m_ref with Some m -> port_read m p | None -> 0xff in
  let pout p v = match !m_ref with Some m -> port_write m p v | None -> () in
  let m =
    {
      cpu = Z80.create ~read ~write ~port_in:pin ~port_out:pout;
      vdp = Vdp.create ();
      main_rom = rom_or_empty main;
      logo_rom = rom_or_empty logo;
      sub_rom = rom_or_empty sub;
      cart = Bytes.make 0 '\000';
      cart_mapper = Flat;
      cart_banks = [| 0; 1; 2; 3 |];
      disk = None;
      fdc =
        { cmd = 0; track = 0; sector = 1; data = 0; side = 0; motor = false;
          busy = false; drq = false; intr = false; buf = Bytes.make 512 '\x00';
          pos = 0 };
      ram = Bytes.make (ram_kb * 1024) '\000';
      mapper = Array.make 4 3;
      ppi_a = 0x00;
      ppi_c = 0x00;
      slot3_sel = 0x00;
      keys = Array.make (key_rows * 8) false;
      psg = Array.make 16 0;
      psg_latch = 0;
      joy1 = joystick_idle;
      cpu_ref = m_ref;
    }
  in
  m_ref := Some m;
  m

let palette_entries t = Array.init 16 (Vdp.palette_rgb t.vdp)

let name t = Printf.sprintf "MSX2/C-BIOS (%dKB RAM)" (Bytes.length t.ram / 1024)

let load_cartridge ?mapper t rom =
  t.cart <- Bytes.of_string rom;
  t.cart_mapper <- (match mapper with Some m -> m | None -> guess_mapper rom);
  (* Reset the banks linear so a MegaROM boots: segment 0 at 0x4000 holds the
     "AB" header and INIT vector, and the running code sets the selectable banks
     before it relies on them. *)
  t.cart_banks.(0) <- 0;
  t.cart_banks.(1) <- 1;
  t.cart_banks.(2) <- 2;
  t.cart_banks.(3) <- 3;
  (* mem_read 은 카트리지를 슬롯2 페이지0·1 에 둔다. 페이지0 을 슬롯2 로
     돌리면 BIOS(슬롯0) 를 잃어 부트가 안 되니, 페이지1(bits2-3) 만
     슬롯2 로 보인다 — C-BIOS 가 0x4000 의 "AB" 헤더를 찾는 자리. *)
  t.ppi_a <- (t.ppi_a land 0xf3) lor 0x08

(* 표준 2DD(9섹터 2헤드) DPB — 게임이 파일시스템을 물을 때 돌려주는 값. *)
let dpb_2dd : string =
  "\xf9\x09\x02\x02\x02\x01\x01\x02\x70\x00\x0a\xf5\x03\xf9\x02\x00"

let disk_trap_counts_arr : int array = Array.make 8 0
let disk_trap_index = function
  | 0x4013 -> 0 | 0x4016 -> 1 | 0x4019 -> 2 | 0x401c -> 3 | 0x401f -> 4
  | _ -> 5

(* DISK BIOS 엔트리 서비스. 0x4013 DSKIO(읽기만: A=드라이브, C=섹터 수,
   DE=논리 섹터, HL=버퍼), 0x4016 DSKCHG(변경 없음), 0x4019 GETDPB(표준
   2DD), 0x401C CHOICE(빈 답), 0x401F DSKFMT(거부). 성공은 CF 를 내리고
   실패는 CF 와 A 에 코드를 싣는다 — 호출자가 보는 계약 그대로. *)
let serve_disk_entry m pc =
  disk_trap_counts_arr.(disk_trap_index pc) <- disk_trap_counts_arr.(disk_trap_index pc) + 1;
  match pc with
  | 0x4013 ->
      let cpu = m.cpu in
      let drive = Z80.dump_a cpu in
      let count = Z80.dump_bc cpu land 0xff in
      let start = Z80.dump_de cpu in
      let buf = Z80.dump_hl cpu in
      let fail code = Z80.set_af cpu ((code lsl 8) lor 0x01) in
      if drive <> 0 then fail 0x0c (* no drive *)
      else begin
        match m.disk with
        | None -> fail 0x0c
        | Some d ->
            let ok = ref true in
            for i = 0 to count - 1 do
              match Dsk.read_sector d (start + i) with
              | Some b ->
                  String.iteri
                    (fun j c ->
                      mem_write m ((buf + (i * 512) + j) land 0xffff)
                        (Char.code c))
                    b
              | None -> ok := false
            done;
            if !ok then Z80.set_af cpu ((Z80.dump_a cpu lsl 8) land 0xff00)
            else fail 0x0d (* sector not found *)
      end
  | 0x4016 ->
      let cpu = m.cpu in
      Z80.set_bc cpu (Z80.dump_bc cpu land 0xff00);
      Z80.set_af cpu ((Z80.dump_a cpu lsl 8) land 0xff00)
  | 0x4019 ->
      let cpu = m.cpu in
      let hl = Z80.dump_hl cpu in
      String.iteri
        (fun j c -> mem_write m ((hl + j) land 0xffff) (Char.code c))
        dpb_2dd;
      Z80.set_af cpu ((Z80.dump_a cpu lsl 8) land 0xff00)
  | 0x401c ->
      let cpu = m.cpu in
      Z80.set_bc cpu (Z80.dump_bc cpu land 0xff00);
      Z80.set_af cpu ((Z80.dump_a cpu lsl 8) land 0xff00)
  | 0x401f -> Z80.set_af m.cpu ((0x0d lsl 8) lor 0x01) (* write-protect *)
  | _ -> ()

let disk_entry_pc = function
  | 0x4013 | 0x4016 | 0x4019 | 0x401c | 0x401f -> true
  | _ -> false

let load_disk m image =
  m.disk <- Some (Dsk.parse image);
  Z80.set_entry_trap m.cpu
    (Some (fun pc -> if disk_entry_pc pc then begin serve_disk_entry m pc; true end else false))

let boot_disk m =
  match m.disk with
  | None -> Error "no disk loaded"
  | Some d -> (
      match Dsk.read_sector d 0 with
      | None -> Error "boot sector unreadable"
      | Some b ->
          String.iteri (fun i c -> mem_write m (0xC000 + i) (Char.code c)) b;
          Z80.set_pc m.cpu 0xC000;
          Ok ())

let disk_trap_counts () = disk_trap_counts_arr

let set_key t k ~pressed =
  match key_target k with
  | Matrix (row, bit) ->
    t.keys.((row * 8) + bit) <- pressed;
    true
  | Joy1_button b ->
    t.joy1 <- (if pressed then t.joy1 land lnot (1 lsl b) else t.joy1 lor (1 lsl b));
    true
  | Unmapped -> false

let port_in t port = port_read t port
let port_out t port v = port_write t port v

let ldirvm_log = ref false
let ldirvm_calls = ref []
let pc_hist_on = ref false
let trace_from : (int * int) option ref = ref None
let trace_remaining = ref 0
let ring = Array.make 64 0
let ri = ref 0
let watch_enter : (int * int) option ref = ref None
let pc_hist = Array.make 256 0

let step t ~frames =
  for _ = 1 to frames do
    let budget = ref cycles_per_frame in
    while !budget > 0 do
      if Vdp.int_active t.vdp then ignore (Z80.interrupt t.cpu);
      let pc0 = Z80.dump_pc t.cpu in
      (match !trace_from with
       | Some (target, n) when pc0 = target && !trace_remaining = 0 ->
         trace_remaining := n
       | _ -> ());
      if !trace_remaining > 0 then begin
        decr trace_remaining;
        Printf.eprintf "t %04x af=%04x bc=%04x de=%04x hl=%04x sp=%04x ppi=%02x\n%!"
          pc0
          (((Z80.dump_a t.cpu) lsl 8) lor Z80.dump_f t.cpu)
          (Z80.dump_bc t.cpu) (Z80.dump_de t.cpu) (Z80.dump_hl t.cpu)
          (Z80.dump_sp t.cpu) t.ppi_a
      end;
      let pc = Z80.dump_pc t.cpu in
      ring.(!ri land 63) <- pc;
      incr ri;
      (match !watch_enter with
       | Some (lo, hi) when pc >= lo && pc < hi ->
         watch_enter := None;
         Printf.eprintf "== entered %04x-%04x at #%d; prev steps:\n%!" lo hi
           !instr_count;
         for k = max 0 (!ri - 40) to !ri - 1 do
           Printf.eprintf "r %04x\n%!" ring.(k land 63)
         done
       | _ -> ());
      if !pc_hist_on then begin
        let i = pc lsr 8 in
        pc_hist.(i) <- pc_hist.(i) + 1
      end;
      if !ldirvm_log && Z80.dump_pc t.cpu = 0x005C then
        ldirvm_calls :=
          (Z80.dump_hl t.cpu, Z80.dump_de t.cpu, Z80.dump_bc t.cpu)
          :: !ldirvm_calls;
      let used = Z80.step t.cpu in
      incr instr_count;
      ignore (Vdp.advance t.vdp ~cycles:used);
      budget := !budget - used
    done
  done

let dump_pc t = Z80.dump_pc t.cpu

let screen_text t =
  let v = t.vdp in
  let r = Vdp.regs v in
  let text = (r.(1) land 0x10) <> 0 in
  let cols = if text then 40 else 32 in
  let nt = ((r.(2) land 0x1f) lsl 10) in
  let b = Buffer.create 512 in
  for row = 0 to 23 do
    for col = 0 to cols - 1 do
      let c = Char.code (Bytes.get (Vdp.vram v) ((nt + row * cols + col) land 0x1ffff)) in
      Buffer.add_char b
        (if c >= 32 && c < 127 then Char.chr c
         else if c = 0 then ' '
         else if c >= 0x41 && c <= 0x5a then Char.chr (c + 32)
         else '.')
    done;
    Buffer.add_char b '\n'
  done;
  Buffer.contents b

let vdp_write_log t = Vdp.write_log t.vdp

let vram_hex t from len =
  let b = Vdp.vram t.vdp in
  for row = 0 to (len - 1) / 16 do
    Printf.eprintf "%05x:" (from + row * 16);
    for i = 0 to 15 do
      Printf.eprintf " %02x"
        (Char.code (Bytes.get b ((from + row * 16 + i) land 0x1ffff)))
    done;
    Printf.eprintf "\n%!"
  done

(* RAM hex 덤프 — 페이지0 가 슬롯3(RAM) 로 스왑된 게임 코드를 읽는다.
   page0 에 보이는 세그먼트는 매퍼(mapper.(0)) 가 고른다. *)
let ram_hex t from len =
  let base = t.mapper.(0) * 0x4000 in
  for row = 0 to (len - 1) / 16 do
    Printf.eprintf "ram %04x:" (from + row * 16);
    for i = 0 to 15 do
      let a = base + ((from + row * 16 + i) land 0x3fff) in
      Printf.eprintf " %02x" (Char.code (Bytes.get t.ram a))
    done
  done;
  Printf.eprintf "\n%!"

let set_ldirvm_log b = ldirvm_log := b

let set_watch_mem addrs =
  watch_mem_addrs := addrs;
  watch_mem_log := [];
  watch_mem_on := true

let watch_mem_entries () = List.rev !watch_mem_log
let set_pc_hist b = pc_hist_on := b
let set_watch_enter lo hi = watch_enter := Some (lo, hi)
let set_trace_from pc n = trace_from := Some (pc, n)
let pc_histogram () = Array.copy pc_hist

let ldirvm_log_calls () = List.rev !ldirvm_calls

let tx_state t = Vdp.tx_state t.vdp
let cmd_history t = Vdp.cmd_history t.vdp
let vdp_status0 t = Vdp.status0 t.vdp
let vdp_line t = Vdp.line_now t.vdp
let vdp_irq_active t = Vdp.int_active t.vdp
let cpu_halted t = Z80.halted t.cpu
let vdp_regs t = Vdp.regs t.vdp
let ppi_a t = t.ppi_a
let slot3_sel t = t.slot3_sel
let cart_mapper t = t.cart_mapper

let debug_dump t =
  let v = t.vdp in
  let nz = ref 0 in
  Bytes.iter (fun c -> if c <> '\000' then incr nz) (Vdp.vram v);
  let blocks =
    String.concat " "
      (List.init 16 (fun b ->
         let c = ref 0 in
         for i = b * 0x400 to b * 0x400 + 0x3ff do
           if Bytes.get (Vdp.vram v) i <> '\000' then incr c
         done;
         if !c > 0 then Printf.sprintf "%x:%d" (b * 0x400) !c else ""))
  in
  Printf.eprintf "ppi_a=%02x slot3=%02x vram_nz=%d regs=%s\nblocks=%s\n%!"
    t.ppi_a t.slot3_sel !nz
    (String.concat " "
       (List.init 8 (fun i -> Printf.sprintf "R%d=%02x" i (Vdp.regs v).(i))))
    blocks

let frame_dims _ = (256, 192)

let frame_rgb t = Vdp.frame_rgb t.vdp

let serialize _ = failwith "savestate: P1 범위 밖"
let restore ~state:_ = failwith "savestate: P1 범위 밖"

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
  | Undefined of int

let display_mode t = Vdp.display_mode t.vdp
let display_mode_to_string = Vdp.display_mode_to_string
let vram_read t addr = Vdp.vram_read t.vdp addr
