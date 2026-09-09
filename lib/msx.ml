(* MSX2 머신 배선 (C-BIOS 구성):
   슬롯0 = main ROM 32KB(페이지0·1) + logo ROM 16KB(페이지2)
   슬롯3-0 = sub ROM 16KB, 슬롯3-2 = RAM 매퍼(512KB)
   PPI A(0xA8) 가 페이지→슬롯을, 0xFFFF 가 슬롯3 의 2차를 고른다.
   포트: VDP 0x98-0x9B, PPI 0xA8-0xAB, PSG 0xA0-0xA2, 매퍼 0xFC-0xFF. *)

let cycles_per_frame = 262 * 228

(* RAM 매퍼 포트(0xFC-0xFF) 쓰기 — 세그먼트 스왑 관찰용. 페이지별 쓰기 수. *)
let mapper_writes : int array = Array.make 4 0

type key =
  | Up | Down | Left | Right | Space | Trigger_a | Trigger_b
  | Shift | Ctrl | Graph
  | Esc | Return | Function of int | Char of char | Backspace

(* 논리 키가 닿는 자리. 키보드 매트릭스 (행 0-10, 비트 0-7) 의 정본은
   openMSX share/unicodemaps/unicodemap.int (국제 배열, <ROW><COL>).
   글자는 대소문자를 같은 키로 본다 — SHIFT 는 매트릭스의 다른 키(행 6 비트 0)라
   별도 키로 두고, SHIFT+글자 는 둘을 동시에 눌러 만든다. Shift/Ctrl/Graph 는
   행 6 의 모디파이어(비트 0/1/2). 조이스틱 1 은 PSG R#14 의 비트로 읽힌다:
   방향 0-3(위·아래·왼·오), 트리거 4-5 (전부 active-low). 방향키는 커서(매트릭스
   행 8)와 조이스틱 방향 비트를 함께 구동한다(Matrix_joy) — GTSTCK(0) 게임(커서
   읽기)과 GTSTCK(1)·PSG 직독 게임(조이스틱 읽기)을 한 키로 커버한다. 표에 없는
   글자와 F6 이상은 Unmapped. *)
type key_target =
  | Matrix of int * int
  | Joy1_bit of int
  | Matrix_joy of (int * int) * int
  | Unmapped

let key_rows = 11

let key_target = function
  | Up -> Matrix_joy ((8, 5), 0)
  | Down -> Matrix_joy ((8, 6), 1)
  | Left -> Matrix_joy ((8, 4), 2)
  | Right -> Matrix_joy ((8, 7), 3)
  | Space -> Matrix (8, 0)
  | Esc -> Matrix (7, 2)
  | Return -> Matrix (7, 7)
  | Backspace -> Matrix (7, 5)
  | Shift -> Matrix (6, 0)
  | Ctrl -> Matrix (6, 1)
  | Graph -> Matrix (6, 2)
  | Function 1 -> Matrix (6, 5)
  | Function 2 -> Matrix (6, 6)
  | Function 3 -> Matrix (6, 7)
  | Function 4 -> Matrix (7, 0)
  | Function 5 -> Matrix (7, 1)
  | Function _ -> Unmapped
  | Trigger_a -> Joy1_bit 4
  | Trigger_b -> Joy1_bit 5
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
type cart_mapper = Flat | Konami | Konami_scc | Ascii8 | Ascii16 | Ascii8_sram

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
  mutable cart_sram : Bytes.t;
      (** battery RAM for an [Ascii8_sram] (Koei) cart: a bank whose value has
          [cart_sram_bit] set reads/writes here instead of ROM. Empty otherwise. *)
  mutable cart_sram_bit : int;
      (** the bank-value bit that selects SRAM (just above the ROM's segment
          range), e.g. 0x20 for a 256KB cart. *)
  mutable disk : Bytes.t;
      (** the floppy image, 512 bytes a sector (empty = no drive). The disk
          interface ROM itself rides in [cart]; see [load_disk]. *)
  mutable disk_dma : int;  (** BDOS transfer address, set by function 0x1A *)
  bdos_files : (int, Bytes.t * int) Hashtbl.t;
      (** FCB 주소 → (파일 내용, 읽은 위치) — BDOS 스텁의 서버 쪽 상태.
          로더가 여러 FCB 를 번갈아 열기 때문에 마지막 파일 하나로는
          부족하다 (삼국지2 는 _OPEN 을 6번 부른다). *)
  mutable frames : int;
  mutable rtc_reg : int;
  mutable con_esc : int;
      (** VT52 escape-sequence state for the BDOS console: 0=plain, 1=after
          ESC, 2=after "ESC Y" (row byte next), 3=column byte next. *)
  mutable rst30_pending : (int * int) list;
      (** RST 30h 인터슬롯의 복귀 대기: (복귀 PC, 저장한 ppi_a) 최근 것 먼저.
          워밍업 재생 경로(page0 = RAM)에서 디스크 로더가 쓰는 벡터를
          트램페린이 대신 서기 위한 상태. *)
  mutable hle_disk : bool;
      (** 인터페이스 ROM 이 HLE(RET 채움)이면 true — disk_trap 이 BIOS 엔트리를
          OCaml 로 서빙한다. 실ROM(cbios_disk.rom)을 심으면 false: ROM 코드가
          직접 돌고 트랩은 물러난다. *)
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
  | Konami | Konami_scc | Ascii8 | Ascii8_sram -> m.cart_banks.(w)
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
  | Ascii8 | Ascii8_sram ->
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
    (* ASCII8 detection returns the SRAM-capable variant: the SRAM only engages
       when a bank sets its select bit (Koei games do; plain ASCII8 never does),
       so it is a safe superset and lets Koei carts boot without an override. *)
    if !a8 > 0 && !a8 >= !scc && !a8 >= !konami then Ascii8_sram
    else if !scc > 0 && !scc >= !konami then Konami_scc
    else if !konami > 0 then Konami
    else if !a16 > 0 then Ascii16
    else Konami
  end

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
    let w = (a lsr 13) - 2 in
    let bank = m.cart_banks.(w) in
    if
      m.cart_mapper = Ascii8_sram && m.cart_sram_bit <> 0
      && bank land m.cart_sram_bit <> 0 && Bytes.length m.cart_sram > 0
    then begin
      (* A Koei cart maps its battery RAM into a window whose bank has the SRAM
         bit; the low bits pick the 8KB SRAM page. *)
      let pages = Bytes.length m.cart_sram / 0x2000 in
      let off = ((bank land (pages - 1)) * 0x2000) + (a land 0x1fff) in
      Char.code (Bytes.get m.cart_sram off)
    end
    else begin
      let seg = cart_seg8 m w in
      let off = (seg * 0x2000) + (a land 0x1fff) in
      Char.code (Bytes.get m.cart (off mod Bytes.length m.cart))
    end
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
      (* 슬롯3: 2차 선택 (0xFFFF 하위 2비트×4, 여기선 전 페이지 단일값).
         서브 배치는 NMS8250 실기를 따른다 — 3-0 = RAM 매퍼, 3-1 = sub ROM,
         3-2 = 디스크 인터페이스 ROM. 커널류 로더가 EXPTBL 을 훑어 0xFFFF 를
         2 로 쓰고 0x4000 을 읽으면 "AB" 를 발견하게 된다(룬마스터 II 실측:
         3-1 에 두면 드라이브 슬롯 순회가 끝나지 않는다). *)
      if m.slot3_sel land 3 = 1 && page <> 3 then
        (* sub ROM 은 페이지0·1 자리. *)
        Char.code (Bytes.get m.sub_rom off)
      else if
        m.slot3_sel land 3 = 2 && page <> 3
        && Bytes.length m.disk > 0 && Bytes.length m.cart >= 0x4000
      then
        Char.code (Bytes.get m.cart (min off (Bytes.length m.cart - 1)))
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
  then begin
    let w = (a lsr 13) - 2 in
    if
      a >= 0x8000 && m.cart_mapper = Ascii8_sram && m.cart_sram_bit <> 0
      && m.cart_banks.(w) land m.cart_sram_bit <> 0 && Bytes.length m.cart_sram > 0
    then begin
      (* A store into a window mapped to battery RAM lands in the SRAM. *)
      let pages = Bytes.length m.cart_sram / 0x2000 in
      let off = ((m.cart_banks.(w) land (pages - 1)) * 0x2000) + (a land 0x1fff) in
      Bytes.set m.cart_sram off (Char.chr (v land 0xff))
    end
    else
      (* Otherwise a write into the cart window is a bank select, not a store. *)
      cart_bank_write m a (v land 0xff)
  end
  else if slot = 3 && (m.slot3_sel land 3 = 0 || page = 3) then begin
    let seg = m.mapper.(page) in
    let base = ((seg * 0x4000) + (a land 0x3fff)) mod (Bytes.length m.ram) in
    Bytes.set m.ram base (Char.chr (v land 0xff))
  end

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

let fdc_create () =
  {
    cmd = 0;
    track = 0;
    sector = 1;
    data = 0;
    side = 0;
    motor = false;
    busy = false;
    drq = false;
    intr = false;
    buf = Bytes.make 512 '\000';
    pos = 0;
  }

let fdc_status m =
  let f = m.fdc in
  (* READ 계열 진행 중이면 DRQ, 아니면 ready(0). not-ready 는 디스크가
     없을 때만 — 있으면 언제나 ready. *)
  let base = if Bytes.length m.disk = 0 then 0x80 else 0x00 in
  let drq = if f.drq then 0x02 else 0x00 in
  let busy = if f.busy then 0x01 else 0x00 in
  base lor drq lor busy

let fdc_load_sector m =
  let f = m.fdc in
  if Bytes.length m.disk = 0 then f.drq <- false
  else begin
    let logical = ((f.track * 2) + f.side) * 9 + (f.sector - 1) in
    let off = logical * 512 in
    if off + 512 > Bytes.length m.disk then f.drq <- false
    else begin
      Bytes.blit m.disk off f.buf 0 512;
      f.pos <- 0;
      f.drq <- true;
      f.busy <- true
    end
  end

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
  | 0xFC | 0xFD | 0xFE | 0xFF ->
    mapper_writes.(port land 3) <- mapper_writes.(port land 3) + 1;
    m.mapper.(port land 3) <- v land 0x3f
  | 0xB4 -> m.rtc_reg <- v land 0xff
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
      cart_sram = Bytes.make 0 '\000';
      cart_sram_bit = 0;
      disk = Bytes.make 0 '\000';
      disk_dma = 0x0080;
      bdos_files = Hashtbl.create 4;
      frames = 0;
      rtc_reg = 0;
      con_esc = 0;
      rst30_pending = [];
      hle_disk = true;
      fdc = fdc_create ();
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
  (* A Koei cart gets 32KB of battery RAM (covers KoeiSRAM8 and KoeiSRAM32); the
     SRAM-select bit is the first bit above the ROM's segment range. *)
  (match t.cart_mapper with
   | Ascii8_sram ->
     let nseg = (String.length rom + 0x1fff) / 0x2000 in
     let bit = ref 1 in
     while !bit < nseg do bit := !bit lsl 1 done;
     t.cart_sram_bit <- !bit;
     t.cart_sram <- Bytes.make 0x8000 '\000'
   | _ ->
     t.cart_sram_bit <- 0;
     t.cart_sram <- Bytes.make 0 '\000');
  (* mem_read 은 카트리지를 슬롯2 페이지0·1 에 둔다. 페이지0 을 슬롯2 로
     돌리면 BIOS(슬롯0) 를 잃어 부트가 안 되니, 페이지1(bits2-3) 만
     슬롯2 로 보인다 — C-BIOS 가 0x4000 의 "AB" 헤더를 찾는 자리. *)
  t.ppi_a <- (t.ppi_a land 0xf3) lor 0x08

(* --- Disk interface (HLE) -------------------------------------------------
   A game disk boots through a disk interface ROM that rides in the cartridge
   slot: C-BIOS finds its "AB" header and calls INIT, the same slot path a game
   cart uses. The ROM's BIOS entries are not WD2793 code but addresses the step
   loop traps ([disk_trap]); the trap moves whole 512-byte sectors between the
   .dsk image and RAM in OCaml. No floppy controller is emulated -- the sector
   transfer is the whole model. *)

let disk_sector_bytes = 512
let disk_init_entry = 0x4100 (* the "AB" INIT vector; C-BIOS CALLSLTs here *)
let disk_dskio_entry = 0x4010 (* DSKIO; the standard disk BIOS jumptable offset *)
let disk_inienv_entry = 0x4030 (* INIENV; the MSX-DOS kernel init calls this first *)
let disk_bdos_entry = 0xf37d (* MSX DISK-BASIC system-call entry; C = function *)
let disk_rst30_entry = 0x0030 (* inter-slot call vector the disk ROM plants *)
let disk_boot_addr = 0xc000 (* boot sector lands here; entry at +0x1e *)

(* Observation: every disk BIOS entry the boot code hits, so an offline run can
   show what convention the .dsk expects (which addresses, which registers). *)
let disk_calls : (int * int * int * int * int * int) list ref = ref []
let disk_call_log = ref false
let set_disk_call_log b = disk_call_log := b
let disk_call_entries () = List.rev !disk_calls

(* Per-function BDOS call counts, for boot diagnosis: which functions a
   loader actually exercises (_RDBLK 27 calls in Sangokushi II's loader). *)
let bdos_call_counts : int array = Array.make 256 0

(* 실제 디스크 ROM(cbios_disk.rom) — 있으면 이식 대상. 헤더 0x4000-0x400F 가
   비어 카트로 못 띄우므로 AB + INIT 벡터만 얹는다. C-BIOS 가 INIT 로 부르는
   0x4030(INIENV) 을 진입점으로 준다 — 하드웨어 초기화·page0 프리미티브 설치
   까지 실ROM 코드에 맡긴다(HLE 트랩은 끈다). *)
let disk_rom_real () =
  let read p =
    try
      let ic = open_in_bin p in
      let s = really_input_string ic (in_channel_length ic) in
      close_in ic;
      Some (Bytes.of_string s)
    with Sys_error _ -> None
  in
  let candidates =
    [ "roms/cbios/cbios_disk.rom"; "../roms/cbios/cbios_disk.rom";
      "../../roms/cbios/cbios_disk.rom"; "../../../roms/cbios/cbios_disk.rom" ]
  in
  match List.find_map read candidates with
  | Some rom when Bytes.length rom >= 0x4000 ->
      let b = Bytes.sub rom 0 0x4000 in
      Bytes.set b 0 'A';
      Bytes.set b 1 'B';
      Bytes.set b 2 (Char.chr (disk_inienv_entry land 0xff));
      Bytes.set b 3 (Char.chr ((disk_inienv_entry lsr 8) land 0xff));
      Some b
  | _ -> None

let disk_rom_bytes () =
  let rom = Bytes.make 0x4000 '\xc9' (* every unentered byte is a RET *) in
  Bytes.set rom 0 'A';
  Bytes.set rom 1 'B';
  Bytes.set rom 2 (Char.chr (disk_init_entry land 0xff));
  Bytes.set rom 3 (Char.chr ((disk_init_entry lsr 8) land 0xff));
  Bytes.to_string rom

(* Move [count] 512-byte sectors between the disk image and RAM. [write] false
   reads disk -> RAM. Sectors past the image read as zero and drop on write, the
   way a real controller reports a seek error; the caller sets the flags. *)
let disk_transfer t ~write ~sector ~count ~addr =
  let len = Bytes.length t.disk in
  for s = 0 to count - 1 do
    let disk_off = (sector + s) * disk_sector_bytes in
    for i = 0 to disk_sector_bytes - 1 do
      let mem = (addr + (s * disk_sector_bytes) + i) land 0xffff in
      let doff = disk_off + i in
      if write then begin
        if doff < len then Bytes.set t.disk doff (Char.chr (mem_read t mem))
      end
      else begin
        let v = if doff < len then Char.code (Bytes.get t.disk doff) else 0 in
        mem_write t mem v
      end
    done
  done

(* --- FAT12 read-only file system on the .dsk image -----------------------
   Enough of FAT12 to find a file in the root directory and read its bytes, so
   the BDOS trap can load MSXDOS.SYS (and whatever the boot opens). The BPB is in
   the boot sector; only fields the reader needs are decoded. *)

let dsk_u8 t off = if off < Bytes.length t.disk then Char.code (Bytes.get t.disk off) else 0
let dsk_u16 t off = dsk_u8 t off lor (dsk_u8 t (off + 1) lsl 8)

let rec ilog2 n = if n <= 1 then 0 else 1 + ilog2 (n / 2)

type fat12 = {
  bytes_per_sec : int;
  sec_per_clus : int;
  sec_per_fat : int;
  root_start_sec : int;
  root_entries : int;
  data_start_sec : int;
  fat_start_sec : int;
}

let fat12_of t =
  let bytes_per_sec = dsk_u16 t 0x0b in
  let sec_per_clus = dsk_u8 t 0x0d in
  let reserved = dsk_u16 t 0x0e in
  let num_fats = dsk_u8 t 0x10 in
  let root_entries = dsk_u16 t 0x11 in
  let sec_per_fat = dsk_u16 t 0x16 in
  let root_start_sec = reserved + (num_fats * sec_per_fat) in
  let root_sectors = ((root_entries * 32) + bytes_per_sec - 1) / bytes_per_sec in
  { bytes_per_sec; sec_per_clus; sec_per_fat; root_start_sec; root_entries;
    data_start_sec = root_start_sec + root_sectors; fat_start_sec = reserved }

(* Next cluster in the FAT12 chain (12 bits packed, low/high nibble by parity). *)
let fat12_next t fs cluster =
  let base = fs.fat_start_sec * fs.bytes_per_sec in
  let off = base + (cluster * 3 / 2) in
  let v = dsk_u8 t off lor (dsk_u8 t (off + 1) lsl 8) in
  if cluster land 1 = 0 then v land 0xfff else (v lsr 4) land 0xfff

(* The 11-byte directory name (8+3, space padded) of the root entry, uppercased
   the way a stored FAT name already is. *)
let fat12_find t fs name11 =
  let entry_at i = (fs.root_start_sec * fs.bytes_per_sec) + (i * 32) in
  let rec scan i =
    if i >= fs.root_entries then None
    else
      let e = entry_at i in
      let first = dsk_u8 t e in
      if first = 0x00 then None (* no more entries *)
      else if first = 0xe5 then scan (i + 1) (* deleted *)
      else
        let matches = ref true in
        for k = 0 to 10 do
          if dsk_u8 t (e + k) <> Char.code name11.[k] then matches := false
        done;
        if !matches then Some e else scan (i + 1)
  in
  scan 0

(* The whole file's bytes, walking its cluster chain up to the directory size. *)
let fat12_read t fs dir_entry =
  let start = dsk_u16 t (dir_entry + 0x1a) in
  let size =
    dsk_u8 t (dir_entry + 0x1c)
    lor (dsk_u8 t (dir_entry + 0x1d) lsl 8)
    lor (dsk_u8 t (dir_entry + 0x1e) lsl 16)
    lor (dsk_u8 t (dir_entry + 0x1f) lsl 24)
  in
  let out = Buffer.create size in
  let clus_bytes = fs.sec_per_clus * fs.bytes_per_sec in
  let rec walk cluster =
    if cluster < 2 || cluster >= 0xff8 || Buffer.length out >= size then ()
    else begin
      let sec = fs.data_start_sec + ((cluster - 2) * fs.sec_per_clus) in
      let off = sec * fs.bytes_per_sec in
      for i = 0 to clus_bytes - 1 do
        if Buffer.length out < size then Buffer.add_char out (Bytes.get t.disk (off + i))
      done;
      walk (fat12_next t fs cluster)
    end
  in
  walk start;
  Buffer.to_bytes out

let fat12_open t name11 =
  if Bytes.length t.disk = 0 then None
  else
    let fs = fat12_of t in
    match fat12_find t fs name11 with
    | None -> None
    | Some e -> Some (fat12_read t fs e)

let load_disk ?(interface_rom = true) ?(real_rom = false) t dsk =
  t.disk <- Bytes.of_string dsk;
  Hashtbl.reset t.bdos_files;
  t.disk_dma <- 0x0080;
  (* The interface ROM occupies the cartridge slot; loading it the cart way puts
     it in slot 2 page 1 with page 1 selected, so C-BIOS finds the "AB" header
     and calls INIT -- the proven path a game cart takes. [~interface_rom:false]
     keeps page 1 off slot 2 for the warm-up replay ({!boot_disk}): a C-BIOS
     boot that finds the interface ROM re-enters the sector boot every boot
     cycle (observed), so the replay path wants a plain BIOS boot first. The
     ROM still rides in [cart] though: a 2nd-stage loader that walks EXPTBL
     looking for a disk interface (Rune Master's kernel) reads it through the
     slot 3-1 view and needs its "AB" header to be findable. *)
  (match (if real_rom then disk_rom_real () else None) with
   | Some real ->
     (* 실ROM 이 있으면 이걸 올리고 HLE 트랩을 끈다 — ROM 의 INIENV/DSKIO 가
         page0 프리미티브 설치와 물리 I/O 를 맡는다(룬마스터 계열 로더의
         전제). 없으면 기존 RET 채움 + 트랩 경로. 실ROM 은 물리 I/O 코드가
         없음이 밝혀져 실험 인프라로만 남는다(cbios_disk.rom dskio_done 참조). *)
     load_cartridge ~mapper:Flat t (Bytes.to_string real);
     t.hle_disk <- false
   | None ->
     if interface_rom then load_cartridge ~mapper:Flat t (disk_rom_bytes ())
     else begin
       (* 워밍업 재생 경로: ppi 는 건드리지 않고 cart 만 채운다 — 위 슬롯3-1
          뷰가 이 ROM 을 보이게. *)
       t.cart <- Bytes.of_string (disk_rom_bytes ());
       t.cart_mapper <- Flat;
       t.cart_banks.(0) <- 0;
       t.cart_banks.(1) <- 1;
       t.cart_banks.(2) <- 2;
       t.cart_banks.(3) <- 3
     end)

let disk_image t =
  if Bytes.length t.disk = 0 then None else Some (Bytes.to_string t.disk)

let change_disk t image =
  let size = String.length image in
  if Bytes.length t.disk = 0 then Error "no disk drive is loaded"
  else if size = 0 || size mod disk_sector_bytes <> 0 then
    Error "disk image must contain complete 512-byte sectors"
  else begin
    let disk = Bytes.of_string image in
    t.disk <- disk;
    Hashtbl.reset t.bdos_files;
    Ok ()
  end

(* The warm-up replay of the Disk ROM's second-stage call (MSX2 Technical
   Handbook ch.3 step 7), for harnesses and lanes: after the C-BIOS boot run
   (720 frames plants the F380 inter-slot primitives in RAM), this puts the
   machine where that call leaves it -- boot sector at 0xC000, RAM in page 0,
   a call to 0xC01E with carry set so the sector's [RET NC] falls through
   into its loader. The cart-INIT path this module also wires boots the same
   sector, but a game's first stage then calls back into C-BIOS BIOS entries
   that reboot the machine (observed: Sangokushi II restarts at f~330); this
   replay is the path that reaches the title screen. *)
let boot_disk t =
  if Bytes.length t.disk = 0 then Error "no disk loaded"
  else if Bytes.length t.disk < 512 then Error "boot sector unreadable"
  else begin
    disk_transfer t ~write:false ~sector:0 ~count:1 ~addr:0xc000;
    t.ppi_a <- 0xfb;
    (* page0-3 전부 슬롯3 인 ppi 에서 서브슬롯0 = RAM 매퍼(NMS8250 배치).
       실기의 디스크 ROM 부트는 EXPTBL(슬롯 확장·식별자)과 RAMAD0-3(RAM 이
       사는 슬롯)도 채워 두는데 C-BIOS 워밍업은 우리 배치를 거기에 기록하지
       않는다 — EXPTBL 을 훑는 2nd stage 커널(룬마스터 II)이 드라이브를
       못 찾아 슬롯 순회가 끝나지 않았다(실측). 서브는 단순(0x00), RAM 는
       slot3-0 = 식별자 0x80|(0<<2)|3. *)
    t.slot3_sel <- t.slot3_sel land 0xfc;
    mem_write t 0xfcc4 0x83;
    for i = 0 to 3 do
      mem_write t (0xfcc5 + i) 0x00;
      mem_write t (0xf340 + i) 0x83
    done;
    t.rst30_pending <- [];
    (* SCNCNT(0xF3F6) 를 성숙 주기(3) 로 시드한다. KEYINT 의 키 스캔은 이 카운터가
       0 까로 내려올 때만 도는데, 재생 시점의 RAM 이 부팅 직후(0) 라면 첫 스캔이
       256 인터럽트 뒤에야 온다. 실기는 디스크 로딩 동안 카운터가 이미 성숙해
       게임 첫 프레임부터 스캔이 돈다. "PRESS SPACE KEY" 타이틀이 스페이스를
       못 받고 타임아웃 리셋을 거는 것(룬마스터 1)이 이 창 때문이었다.
       게임이 로드되며 매퍼를 다시 배선하면(룬마스터 1 은 page3 을 seg0 으로
       돌린다) 시드한 물리 세그먼트가 보이지 않게 되므로, page3 이 가리킬 수
       있는 세그먼트 전부에 심는다 — 0xF3F6 매핑이 어느 물리로 갈려도 성숙해
       있어야 실기의 "로딩 끝난 상태"와 같다. *)
    let saved_p3 = t.mapper.(3) in
    let nseg = Bytes.length t.ram / 0x4000 in
    for s = 0 to nseg - 1 do
      t.mapper.(3) <- s;
      mem_write t 0xf3f6 3;
      (* The boot sector reloads SP from 0xF674 ("ld sp,(0f674h)"); a real
         disk ROM leaves the system stack pointer there. Unseeded it reads
         garbage inside the 0xC000s, so the growing call stack chews through
         the kernel's own code at 0xC5xx and every later "mystery jump" is
         corrupted code, not logic. Park it just under the one we set. *)
      mem_write t 0xf674 0x1f;
      mem_write t 0xf675 0xf5
    done;
    t.mapper.(3) <- saved_p3;
    Z80.set_sp t.cpu 0xf51f;
    Z80.set_af t.cpu ((0x00 lsl 8) lor 0x01);
    Z80.set_pc t.cpu 0xc01e;
    Ok ()
  end

let bdos_counts () = bdos_call_counts

(* The standard disk BIOS jumptable entries at 0x4013+ (DSKCHG/GETDPB/CHOICE/
   DSKFMT) and the loader-observed DSKIO convention at 0x4013 (A=drive,
   C=sectors, DE=logical sector, HL=buffer — not the 0x4010 B-register one):
   a Sangokushi II boot sector calls these on its side of the interface ROM.
   Success drops carry, failure sets carry and a code in A. *)
let serve_disk_entry t pc =
  let cpu = t.cpu in
  if !disk_call_log then
    disk_calls := (pc, Z80.dump_a cpu, Z80.dump_bc cpu, Z80.dump_de cpu,
                   Z80.dump_hl cpu, Z80.dump_f cpu) :: !disk_calls;
  let ret =
    let sp = Z80.dump_sp cpu in
    let r = mem_read t sp lor (mem_read t (sp + 1) lsl 8) in
    Z80.set_sp cpu ((sp + 2) land 0xffff);
    r
  in
  let af_ok a = Z80.set_af cpu ((a lsl 8) land 0xff00) in
  (match pc with
   | 0x4013 ->
     let drive = Z80.dump_a cpu in
     let count = Z80.dump_bc cpu land 0xff in
     let start = Z80.dump_de cpu in
     let buf = Z80.dump_hl cpu in
     if drive <> 0 || Bytes.length t.disk = 0 then
       Z80.set_af cpu ((0x0c lsl 8) lor 0x01)
     else begin
       disk_transfer t ~write:false ~sector:start ~count ~addr:buf;
       (* 이미지 밖 섹터는 0으로 읽힌다 — 디스크 트랩 계약상 성공. *)
       af_ok (Z80.dump_a cpu)
     end
   | 0x4016 ->
     Z80.set_bc cpu (Z80.dump_bc cpu land 0xff00);
     af_ok (Z80.dump_a cpu)
   | 0x4019 ->
     let hl = Z80.dump_hl cpu in
     (* 표준 2DD(9섹터 2헤드) DPB. *)
     String.iteri
       (fun j c -> mem_write t ((hl + j) land 0xffff) (Char.code c))
       "\xf9\x09\x02\x02\x02\x01\x01\x02\x70\x00\x0a\xf5\x03\xf9\x02\x00";
     af_ok (Z80.dump_a cpu)
   | 0x401c ->
     Z80.set_bc cpu (Z80.dump_bc cpu land 0xff00);
     af_ok (Z80.dump_a cpu)
   | 0x401f ->
     Z80.set_af cpu ((0x0d lsl 8) lor 0x01) (* write-protect *)
   | _ -> ());
  Z80.set_pc cpu ret

let disk_entry_pc = function
  | 0x4013 | 0x4016 | 0x4019 | 0x401c | 0x401f -> true
  | _ -> false

(* RST 30h 트램펄린 — 실기의 디스크 ROM 이 0x0030 에 깔아 두던 인터슬롯
   핸들러 대역. RST 30h 가 push 한 반환주소는 서술자(slot 1 + 주소 2)를
   가리킨다: 서술자를 읽고, 그 자리에 서술자 건너뛴 복귀 주소를 심은 뒤
   대상 페이지의 슬롯 배선을 돌려 대상으로 건다. 대상의 RET 이 복귀 주소로
   돌아오면 disk_trap 이 배선을 되돌린다. page0 이 RAM(워밍업 재생 경로)
   일 때만 건다 — C-BIOS 부팅 중(page0 = main ROM)의 자기 RST 30h 사용은
   ROM 코드가 직접 처리한다 (실측: 카트 INIT 호출). *)
let serve_rst30 t =
  let cpu = t.cpu in
  let sp = Z80.dump_sp cpu in
  let rd16 a = mem_read t a lor (mem_read t ((a + 1) land 0xffff) lsl 8) in
  let wr16 a v =
    mem_write t a (v land 0xff);
    mem_write t ((a + 1) land 0xffff) (v lsr 8)
  in
  let desc = rd16 sp in
  let slot = mem_read t desc in
  let addr = rd16 ((desc + 1) land 0xffff) in
  wr16 sp ((desc + 3) land 0xffff);
  let page = addr lsr 14 in
  let shift = page * 2 in
  t.rst30_pending <- ((desc + 3) land 0xffff, t.ppi_a) :: t.rst30_pending;
  t.ppi_a <-
    (t.ppi_a land (lnot (3 lsl shift) land 0xff)) lor ((slot land 3) lsl shift);
  addr

(* CALSLT (BIOS 0x001C) served in OCaml for the warm-up replay, where page 0
   is RAM and the real BIOS entry is not visible. Contract per the C-BIOS
   source: IY high byte = target slot in RDSLT's A format (0x80 | sub<<2 |
   primary; 0 = slot 0), IX = address to call. We repoint the target's page
   at that slot, register the restore like an RST 30h return, and jump. *)
let serve_calslt t =
  let cpu = t.cpu in
  let slot = (Z80.dump_iy cpu lsr 8) land 0xff in
  let target = Z80.dump_ix cpu in
  let page = target lsr 14 in
  let shift = page * 2 in
  let sp = Z80.dump_sp cpu in
  let ret = mem_read t sp lor (mem_read t ((sp + 1) land 0xffff) lsl 8) in
  Z80.set_sp cpu ((sp + 2) land 0xffff);
  t.rst30_pending <- (ret, t.ppi_a) :: t.rst30_pending;
  t.ppi_a <-
    (t.ppi_a land (lnot (3 lsl shift) land 0xff)) lor ((slot land 3) lsl shift);
  (* An expanded slot 3 target also selects its sub-slot for the target's
     page. The replay's CALSLT callers so far only target slot 0 (RSLREG),
     which leaves [slot3_sel] alone. *)
  if slot land 0x80 <> 0 && (slot land 3) = 3 && page <> 3 then
    t.slot3_sel <- (t.slot3_sel land 0xfc) lor ((slot lsr 2) land 3);
  target

(* Serviced in the step loop before the opcode at [pc] runs. Returns true when
   [pc] is a disk BIOS entry the trap handled (moving the CPU state on). *)
let disk_trap t pc =
  (match t.rst30_pending with
   | (cont, saved) :: rest when pc = cont ->
     (* 인터슬롯 대상이 복귀했다 — 슬롯 배선을 되돌리고 실행 계속. *)
     t.rst30_pending <- rest;
     t.ppi_a <- saved
   | _ -> ());
  if Bytes.length t.disk = 0 || not t.hle_disk then false
  else if pc = disk_rst30_entry && t.ppi_a land 3 = 3 then begin
    Z80.set_pc t.cpu (serve_rst30 t);
    true
  end
  else if pc = 0x001c && t.ppi_a land 3 = 3 then begin
    (* CALSLT: only reachable through empty page-0 RAM (the warm-up replay),
       never from real BIOS -- there page 0 is ROM and 0x001C runs natively. *)
    Z80.set_pc t.cpu (serve_calslt t);
    true
  end
  else if pc >= 0x4000 && pc < 0x8000
          && (let slot = (t.ppi_a lsr 2) land 3 in slot = 0 || slot = 3) then false
  else if pc = disk_init_entry then begin
    if !disk_call_log then
      disk_calls := (pc, Z80.dump_a t.cpu, Z80.dump_bc t.cpu, Z80.dump_de t.cpu,
                     Z80.dump_hl t.cpu, Z80.dump_f t.cpu) :: !disk_calls;
    (* INIT: read the boot sector to 0xC000 and enter it at +0x1e with carry
       clear -- the boot code's first byte is RET NC, a check that the read
       succeeded. *)
    disk_transfer t ~write:false ~sector:0 ~count:1 ~addr:disk_boot_addr;
    (* A real disk ROM's boot procedure enables RAM in the low pages before it
       loads the DOS kernel. Do the same: pages 0 and 2 to slot 3 (the RAM
       mapper, sub 0 in the NMS8250 layout -- the pre-NMS code selected sub 2
       because that was RAM then). Page 0 holds the kernel at 0x0100; page 2
       holds its stack (the kernel sets SP=0x9000). Without RAM in page 2 the
       stack lands on the logo ROM and CALL/RET reads back garbage. *)
    t.ppi_a <- (t.ppi_a land 0xcc) lor 0x33;
    t.slot3_sel <- t.slot3_sel land 0xfc;
    (* Enter the boot sector at +0x1e with carry SET: its first byte is RET NC,
       which the disk ROM uses to bail when the sector is not bootable. Carry
       set means "boot this", so the code runs instead of returning. *)
    Z80.set_af t.cpu ((0x00 lsl 8) lor 0x01) (* A=0 (drive 0), carry set *);
    Z80.set_pc t.cpu (disk_boot_addr + 0x1e);
    true
  end
  else if pc = disk_inienv_entry then begin
    (* INIENV, observation stub: just return. Measures how far the kernel
       gets before it needs what a real INIENV installs. *)
    let sp = Z80.dump_sp t.cpu in
    let ret = mem_read t sp lor (mem_read t (sp + 1) lsl 8) in
    Z80.set_sp t.cpu ((sp + 2) land 0xffff);
    Z80.set_pc t.cpu ret;
    true
  end
  else if pc = disk_dskio_entry then begin
    (* DSKIO: A=drive, B=sectors, C=media, DE=first sector, HL=addr, carry=write
       on entry. Success returns carry clear, B=0 remaining; then RET. *)
    let a = Z80.dump_a t.cpu in
    let f = Z80.dump_f t.cpu in
    let write = f land 0x01 <> 0 in
    let count = (Z80.dump_bc t.cpu lsr 8) land 0xff in
    let sector = Z80.dump_de t.cpu in
    let addr = Z80.dump_hl t.cpu in
    if !disk_call_log then
      disk_calls := (pc, a, Z80.dump_bc t.cpu, sector, addr, f) :: !disk_calls;
    disk_transfer t ~write ~sector ~count ~addr;
    Z80.set_af t.cpu ((a lsl 8) lor (f land 0xfe)) (* carry clear = success *);
    Z80.set_bc t.cpu (Z80.dump_bc t.cpu land 0x00ff) (* B=0 remaining *);
    let sp = Z80.dump_sp t.cpu in
    let ret = mem_read t sp lor (mem_read t (sp + 1) lsl 8) in
    Z80.set_sp t.cpu ((sp + 2) land 0xffff);
    Z80.set_pc t.cpu ret;
    true
  end
  else if pc = disk_bdos_entry then begin
    (* MSX DISK-BASIC system call (C = function). The file-load path the boot
       uses is served against the FAT12 image; per-file state lives in
       [bdos_files] keyed by the FCB address. A=0 success, A=0xFF failure --
       the boot does INC A / JR Z, so 0 continues and 0xFF branches to its
       error path. _RDBLK reads from the FCB's random-record field and
       advances it, the way a real MSX-DOS leaves it for the next call --
       Sangokushi II's loader issues 27 back-to-back _RDBLK calls this way. *)
    let c = Z80.dump_bc t.cpu land 0xff in
    let de = Z80.dump_de t.cpu in
    let fcb i = mem_read t ((de + i) land 0xffff) in
    bdos_call_counts.(c) <- bdos_call_counts.(c) + 1;
    if !disk_call_log then
      disk_calls := (pc, c, Z80.dump_bc t.cpu, de, Z80.dump_hl t.cpu, Z80.dump_f t.cpu)
                    :: !disk_calls;
    let a =
      match c with
      | 0x06 ->
        (* _DIRIO: console in/out. No console -- an input poll (E=0xFF) gets
           "no character ready" (A=0), output is consumed. *)
        0x00
      | 0x07 ->
        (* _DIRIN: direct console input. Served non-blockingly from the
           keyboard matrix: the space bar (row 8, bit 0) reports its ASCII
           code while held, nothing held reads A=0. What a "press any key"
           loader needs -- without it every poll fails (0xFF) and the loader
           restarts its boot loop forever (Rune Master II does exactly
           this). *)
        (if t.keys.((8 * 8) + 0) then 0x20 else 0x00)
      | 0x09 ->
        (* _STROUT: print a '$'-terminated string -- consumed, no console. *)
        0x00
      | 0x0f -> (
        let name = String.init 11 (fun i -> Char.chr (fcb (1 + i))) in
        match fat12_open t name with
        | Some data ->
          if !disk_call_log then
            Printf.eprintf "BDOS open '%s' -> %d bytes\n%!" name (Bytes.length data);
          Hashtbl.replace t.bdos_files de (data, 0);
          let sz = Bytes.length data in
          mem_write t ((de + 0x10) land 0xffff) (sz land 0xff);
          mem_write t ((de + 0x11) land 0xffff) ((sz lsr 8) land 0xff);
          mem_write t ((de + 0x12) land 0xffff) ((sz lsr 16) land 0xff);
          mem_write t ((de + 0x13) land 0xffff) ((sz lsr 24) land 0xff);
          0x00
        | None ->
          if !disk_call_log then Printf.eprintf "BDOS open '%s' -> NOT FOUND\n%!" name;
          0xff)
      | 0x16 -> (
        let name = String.init 11 (fun i -> Char.chr (fcb (1 + i))) in
        if fcb 0 > 1 then 0xff
        else
          let validated = Disk_fat12.validate_writable_file ~image:t.disk ~name in
          let existing = if Result.is_ok validated && fcb 0x0c <> 0 then fat12_open t name else None in
          let result = match validated, existing with
            | Error error, _ -> Error error
            | Ok (), Some data -> Ok (t.disk, data)
            | Ok (), None -> Result.map (fun disk -> disk, Bytes.empty)
                (Disk_fat12.put_file ~image:t.disk ~name ~data:Bytes.empty) in
          match result with
          | Error _ -> 0xff
          | Ok (disk, data) ->
            t.disk <- disk;
            Hashtbl.replace t.bdos_files de (data, 0);
            let extent = fcb 0x0c in
            for i = 0x0c to 0x24 do mem_write t ((de + i) land 0xffff) 0 done;
            mem_write t ((de + 0x0c) land 0xffff) extent;
            mem_write t ((de + 0x0e) land 0xffff) 128;
            for i = 0 to 3 do
              mem_write t ((de + 0x10 + i) land 0xffff) ((Bytes.length data lsr (8 * i)) land 255)
            done;
            0x00)
      | 0x26 -> (
        let name = String.init 11 (fun i -> Char.chr (fcb (1 + i))) in
        let current = if Hashtbl.mem t.bdos_files de
          && Result.is_ok (Disk_fat12.validate_writable_file ~image:t.disk ~name)
          then fat12_open t name else None in
        match current with
        | None -> 0x01
        | Some data ->
          let size = fcb 0x0e lor (fcb 0x0f lsl 8) in
          let record = fcb 0x21 lor (fcb 0x22 lsl 8) lor (fcb 0x23 lsl 16)
            lor (if size < 64 then fcb 0x24 lsl 24 else 0) in
          let count = Z80.dump_hl t.cpu in
          let start = record * size and amount = count * size in
          let length = if count = 0 then start else max (Bytes.length data) (start + amount) in
          if size = 0 || amount > 65536 || length > Bytes.length t.disk || fcb 0 > 1 then 0x01
          else
            let updated = Bytes.make length '\000' in
            Bytes.blit data 0 updated 0 (min length (Bytes.length data));
            for i = 0 to amount - 1 do
              Bytes.set updated (start + i) (Char.chr (mem_read t ((t.disk_dma + i) land 0xffff)))
            done;
            let name = String.init 11 (fun i -> Char.chr (fcb (1 + i))) in
            match Disk_fat12.put_file ~image:t.disk ~name ~data:updated with
            | Error _ -> 0x01
            | Ok disk ->
              t.disk <- disk;
              Hashtbl.replace t.bdos_files de (updated, start + amount);
              for i = 0 to (if size < 64 then 3 else 2) do
                mem_write t ((de + 0x21 + i) land 0xffff) (((record + count) lsr (8 * i)) land 255)
              done;
              for i = 0 to 3 do
                mem_write t ((de + 0x10 + i) land 0xffff) ((length lsr (8 * i)) land 255)
              done;
              0x00)
      | 0x10 ->
        (* _CLOSE: drop the server-side file state. *)
        Hashtbl.remove t.bdos_files de;
        0x00
      | 0x14 -> (
        (* _RDSEQ: one record (FCB+0x0E, default 128) from the sequential
           position to the DMA address. A=1 on end-of-file, the CP/M code. *)
        match Hashtbl.find_opt t.bdos_files de with
        | None -> 0xff
        | Some (data, pos) ->
          let rec_size =
            let r = fcb 0x0e lor (fcb 0x0f lsl 8) in
            if r = 0 then 128 else r
          in
          let take = min rec_size (Bytes.length data - pos) in
          if take <= 0 then 1
          else begin
            for i = 0 to take - 1 do
              mem_write t ((t.disk_dma + i) land 0xffff)
                (Char.code (Bytes.get data (pos + i)))
            done;
            Hashtbl.replace t.bdos_files de (data, pos + take);
            0x00
          end)
      | 0x1a ->
        t.disk_dma <- de;
        0x00
      | 0x0d ->
        (* Disk reset: default drive A, DMA back to 0x0080. *)
        t.disk_dma <- 0x0080;
        0x00
      | 0x19 ->
        (* Default drive: A. *)
        0x00
      | 0x1b when Bytes.length t.disk > 0 ->
        (* Disk information (MSX-DOS specific). A=sectors/cluster, BC=sector
           size, DE=clusters+1, IX=DPB address, IY=FAT in memory. The FAT copy
           and DPB live above the boot sector image: FAT @ 0xC800 (one FAT),
           DPB right after it -- what a real disk ROM installs for the boot
           loader to parse. *)
        let fs = fat12_of t in
        let fat_bytes = fs.sec_per_fat * fs.bytes_per_sec in
        let fat_addr = 0xc800 in
        let dpb = fat_addr + fat_bytes in
        let w off v =
          mem_write t (dpb + off) (v land 0xff);
          mem_write t (dpb + off + 1) ((v lsr 8) land 0xff)
        in
        for i = 0 to fat_bytes - 1 do
          mem_write t (fat_addr + i)
            (dsk_u8 t ((fs.fat_start_sec * fs.bytes_per_sec) + i))
        done;
        let clusters = (dsk_u16 t 0x13 - fs.data_start_sec) / fs.sec_per_clus in
        mem_write t dpb 0; (* drive A *)
        mem_write t (dpb + 1) (dsk_u8 t 0x15); (* media ID *)
        w 2 fs.bytes_per_sec;
        mem_write t (dpb + 4) ((fs.bytes_per_sec / 32) - 1); (* dir mask *)
        mem_write t (dpb + 5) (ilog2 (fs.bytes_per_sec / 32));
        mem_write t (dpb + 6) (fs.sec_per_clus - 1); (* cluster mask *)
        mem_write t (dpb + 7) (ilog2 fs.sec_per_clus);
        w 8 fs.fat_start_sec; (* top sector of FAT *)
        mem_write t (dpb + 10) (dsk_u8 t 0x10); (* number of FATs *)
        mem_write t (dpb + 11) fs.root_entries;
        w 12 fs.data_start_sec; (* top sector of data area *)
        w 14 (clusters + 1);
        mem_write t (dpb + 16) fs.sec_per_fat;
        w 17 fs.root_start_sec;
        w 19 fat_addr; (* FAT address in memory *)
        Z80.set_bc t.cpu fs.bytes_per_sec;
        Z80.set_de t.cpu (clusters + 1);
        Z80.set_ix t.cpu dpb;
        Z80.set_iy t.cpu fat_addr;
        fs.sec_per_clus
      | 0x2f when Bytes.length t.disk > 0 ->
        (* Absolute logical-sector read: DE=first sector, H=count, L=drive.
           The .dsk is a raw image, so a logical sector IS a file sector. *)
        let sector = Z80.dump_de t.cpu in
        let count = (Z80.dump_hl t.cpu lsr 8) land 0xff in
        disk_transfer t ~write:false ~sector ~count ~addr:t.disk_dma;
        (* A 2nd-stage loader that is a customised MSX-DOS kernel (Rune Master's
           sector 3) rides a jp table at its head -- entries 3 bytes apart --
           that belongs in page 0: the DOS kernel's primitive vectors
           (0x000C = its sector-read entry, 0x001C = CALSLT trampoline,
           0x0024 = slot-id arithmetic). The table length varies by game (20
           entries in Rune Master, 24 in Rune Master II), so plant as many
           consecutive entries as the data actually has. A real boot leaves
           those installed in page 0 RAM; our replay never installs them, so
           the loader's first CALL 0x000C slides into empty RAM and dies. *)
        let jp_table = ref true in
        for k = 0 to 2 do
          if mem_read t ((t.disk_dma + (3 * k)) land 0xffff) <> 0xc3 then
            jp_table := false
        done;
        let len = ref 0 in
        while !len < 0xc0 && mem_read t ((t.disk_dma + !len) land 0xffff) = 0xc3 do
          len := !len + 3
        done;
        (* Rune Master 1's header mixes data entries into the table (0x09
           jumps into game code at 0x5EC3, 0x0C is JP NZ -- falling through
           to the 0x0F entry when Z), so "every byte is a jp" rejects it and
           page 0 stays empty. The first three entries being jp is enough
           evidence; copy the whole 0x3F header so the data entries sit in
           page 0 exactly where a real boot leaves them. *)
        if !jp_table && !len >= 9 then begin
          let n = max !len 0x3f in
          if !disk_call_log then
            Printf.eprintf "BDOS 2F: planting page0 vectors from %04x (%d bytes)\n%!"
              t.disk_dma n;
          for i = 0 to n - 1 do
            mem_write t i (mem_read t ((t.disk_dma + i) land 0xffff))
          done
        end;
        0x00
      | 0x27 -> (
        match Hashtbl.find_opt t.bdos_files de with
        | None -> 0xff
        | Some (data, _) ->
          let configured_size = fcb 0x0e lor (fcb 0x0f lsl 8) in
          let size = if configured_size = 0 then 128 else configured_size in
          let record = fcb 0x21 lor (fcb 0x22 lsl 8) lor (fcb 0x23 lsl 16)
            lor (if size < 64 then fcb 0x24 lsl 24 else 0) in
          let count = Z80.dump_hl t.cpu in
          let start = record * size and want = count * size in
          if size = 0 || want > 65536 then (Z80.set_hl t.cpu 0; 0x01)
          else
            let available = max 0 (Bytes.length data - start) in
            let amount = min want available in
            let records = (amount + size - 1) / size in
            for i = 0 to records * size - 1 do
              let byte = if i < amount then Char.code (Bytes.get data (start + i)) else 0 in
              mem_write t ((t.disk_dma + i) land 0xffff) byte
            done;
            for i = 0 to (if size < 64 then 3 else 2) do
              mem_write t ((de + 0x21 + i) land 0xffff) (((record + records) lsr (8 * i)) land 255)
            done;
            Hashtbl.replace t.bdos_files de (data, start + amount);
            Z80.set_hl t.cpu records;
            if records < count then 0x01 else 0x00)
      | _ -> 0xff
    in
    if c = 0x16 then Z80.set_hl t.cpu ((Z80.dump_hl t.cpu land 0xff00) lor a);
    Z80.set_af t.cpu ((a lsl 8) lor (Z80.dump_f t.cpu land 0xff));
    let sp = Z80.dump_sp t.cpu in
    let ret = mem_read t sp lor (mem_read t (sp + 1) lsl 8) in
    Z80.set_sp t.cpu ((sp + 2) land 0xffff);
    Z80.set_pc t.cpu ret;
    true
  end
  else if disk_entry_pc pc then begin
    serve_disk_entry t pc;
    true
  end
  else false

let set_key t k ~pressed =
  let set_matrix row bit = t.keys.((row * 8) + bit) <- pressed in
  let set_joy bit =
    t.joy1 <- (if pressed then t.joy1 land lnot (1 lsl bit) else t.joy1 lor (1 lsl bit))
  in
  match key_target k with
  | Matrix (row, bit) ->
    set_matrix row bit;
    true
  | Joy1_bit bit ->
    set_joy bit;
    true
  | Matrix_joy ((row, bit), joy) ->
    set_matrix row bit;
    set_joy joy;
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
      (* A disk BIOS entry is served in OCaml (HLE), not by fetching the ROM's
         opcode there; the trap moves PC on, so charge a nominal call's cycles. *)
      let used = if disk_trap t pc then 18 else Z80.step t.cpu in
      incr instr_count;
      ignore (Vdp.advance t.vdp ~cycles:used);
      budget := !budget - used
    done;
    t.frames <- t.frames + 1
  done

let frame_number t = t.frames
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
  Printf.eprintf
    "ppi_a=%02x slot3=%02x mapper=%d,%d,%d,%d (writes %d,%d,%d,%d)\n%!"
    t.ppi_a t.slot3_sel
    t.mapper.(0) t.mapper.(1) t.mapper.(2) t.mapper.(3)
    mapper_writes.(0) mapper_writes.(1) mapper_writes.(2) mapper_writes.(3);
  Printf.eprintf "vram_nz=%d regs=%s\nblocks=%s\n%!"
    !nz
    (String.concat " "
       (List.init 8 (fun i -> Printf.sprintf "R%d=%02x" i (Vdp.regs v).(i))))
    blocks

let frame_dims t = Vdp.frame_dims t.vdp

let frame_rgb t = Vdp.frame_rgb t.vdp

let mapper_code = function
  | Flat -> 0 | Konami -> 1 | Konami_scc -> 2 | Ascii8 -> 3 | Ascii16 -> 4 | Ascii8_sram -> 5
let mapper_of_code = function
  | 0 -> Flat | 1 -> Konami | 2 -> Konami_scc | 3 -> Ascii8 | 4 -> Ascii16 | 5 -> Ascii8_sram
  | _ -> State_codec.fail "invalid cartridge mapper"

let serialize t =
  let w = State_codec.writer () in
  (* RAM/media first so restore can create correctly bound CPU callbacks. *)
  List.iter (State_codec.put_bytes w)
    [t.ram; t.main_rom; t.logo_rom; t.sub_rom; t.cart; t.cart_sram; t.disk];
  State_codec.put_int w (mapper_code t.cart_mapper);
  State_codec.put_int_array w t.cart_banks;
  State_codec.put_int_array w t.mapper;
  State_codec.put_int_array w t.psg;
  Array.iter (State_codec.put_bool w) t.keys;
  State_codec.put_int w t.frames;
  State_codec.put_int w t.cart_sram_bit;
  State_codec.put_int w t.disk_dma;
  State_codec.put_int w t.rtc_reg;
  State_codec.put_int w t.con_esc;
  State_codec.put_int w t.ppi_a;
  State_codec.put_int w t.ppi_c;
  State_codec.put_int w t.slot3_sel;
  State_codec.put_int w t.psg_latch;
  State_codec.put_int w t.joy1;
  State_codec.put_int w (List.length t.rst30_pending);
  List.iter (fun (pc, slots) -> State_codec.put_int w pc; State_codec.put_int w slots) t.rst30_pending;
  let files = Hashtbl.to_seq t.bdos_files |> List.of_seq |> List.sort (fun (a,_) (b,_) -> compare a b) in
  State_codec.put_int w (List.length files);
  List.iter (fun (fcb, (data, pos)) ->
    State_codec.put_int w fcb; State_codec.put_bytes w data; State_codec.put_int w pos) files;
  Z80.write_state w t.cpu;
  Vdp.write_state w t.vdp;
  State_codec.finish w

let restore ~state =
  try
    let r = State_codec.reader state in
    let ram = State_codec.get_bytes r in
    (* Current mapper exposes 6 bank bits, 16K each. *)
    let ram_size = Bytes.length ram in
    if ram_size < 65536 || ram_size > 64 * 16384 || ram_size mod 16384 <> 0 then
      State_codec.fail "invalid RAM size";
    let main = State_codec.get_bytes r in
    let logo = State_codec.get_bytes r in
    let sub = State_codec.get_bytes r in
    let t = create ~machine:{ram_kb = ram_size / 1024; vram_kb = 128;
        roms = List.map Bytes.to_string [main; logo; sub]} in
    Bytes.blit ram 0 t.ram 0 ram_size;
    t.cart <- State_codec.get_bytes r;
    t.cart_sram <- State_codec.get_bytes r;
    let sram_size = Bytes.length t.cart_sram in
    if sram_size <> 0 && (sram_size mod 8192 <> 0 || sram_size land (sram_size - 1) <> 0) then
      State_codec.fail "invalid cartridge SRAM size";
    t.disk <- State_codec.get_bytes r;
    t.cart_mapper <- mapper_of_code (State_codec.get_int r ~min:0 ~max:5);
    State_codec.fill_int_array r ~min:0 ~max:255 t.cart_banks;
    State_codec.fill_int_array r ~min:0 ~max:63 t.mapper;
    State_codec.fill_int_array r ~min:0 ~max:255 t.psg;
    Array.iteri (fun i _ -> t.keys.(i) <- State_codec.get_bool r) t.keys;
    t.frames <- State_codec.get_int r ~min:0 ~max:max_int;
    t.cart_sram_bit <- State_codec.get_int r ~min:0 ~max:max_int;
    t.disk_dma <- State_codec.get_int r ~min:0 ~max:65535;
    t.rtc_reg <- State_codec.get_int r ~min:0 ~max:255;
    t.con_esc <- State_codec.get_int r ~min:0 ~max:3;
    t.ppi_a <- State_codec.get_int r ~min:0 ~max:255;
    t.ppi_c <- State_codec.get_int r ~min:0 ~max:255;
    t.slot3_sel <- State_codec.get_int r ~min:0 ~max:255;
    t.psg_latch <- State_codec.get_int r ~min:0 ~max:255;
    t.joy1 <- State_codec.get_int r ~min:0 ~max:255;
    let pending = State_codec.get_int r ~min:0 ~max:(State_codec.remaining r / 16) in
    t.rst30_pending <- List.init pending (fun _ ->
      let pc = State_codec.get_int r ~min:0 ~max:65535 in
      let slots = State_codec.get_int r ~min:0 ~max:255 in pc, slots);
    let files = State_codec.get_int r ~min:0 ~max:65536 in
    for _ = 1 to files do
      let fcb = State_codec.get_int r ~min:0 ~max:65535 in
      if Hashtbl.mem t.bdos_files fcb then State_codec.fail "duplicate saved FCB";
      let data = State_codec.get_bytes r in
      let pos = State_codec.get_int r ~min:0 ~max:max_int in
      Hashtbl.add t.bdos_files fcb (data, pos)
    done;
    Z80.read_state r t.cpu;
    Vdp.read_state r t.vdp;
    State_codec.end_of_input r;
    Ok t
  with State_codec.Invalid_state message -> Error message


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
