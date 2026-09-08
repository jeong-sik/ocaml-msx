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
  mutable disk : Bytes.t;
      (** the floppy image, 512 bytes a sector (empty = no drive). The disk
          interface ROM itself rides in [cart]; see [load_disk]. *)
  mutable disk_dma : int;  (** BDOS transfer address, set by function 0x1A *)
  mutable disk_open : Bytes.t option;
      (** bytes of the file the last BDOS Open (0x0F) found, for Read Block. *)
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

let port_read m port =
  match port land 0xff with
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
      disk = Bytes.make 0 '\000';
      disk_dma = 0x0080;
      disk_open = None;
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
let disk_bdos_entry = 0xf37d (* MSX DISK-BASIC system-call entry; C = function *)
let disk_boot_addr = 0xc000 (* boot sector lands here; entry at +0x1e *)

(* Observation: every disk BIOS entry the boot code hits, so an offline run can
   show what convention the .dsk expects (which addresses, which registers). *)
let disk_calls : (int * int * int * int * int * int) list ref = ref []
let disk_call_log = ref false
let set_disk_call_log b = disk_call_log := b
let disk_call_entries () = List.rev !disk_calls

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

type fat12 = {
  bytes_per_sec : int;
  sec_per_clus : int;
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
  { bytes_per_sec; sec_per_clus; root_start_sec; root_entries;
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

let load_disk t dsk =
  t.disk <- Bytes.of_string dsk;
  t.disk_open <- None;
  t.disk_dma <- 0x0080;
  (* The interface ROM occupies the cartridge slot; loading it the cart way puts
     it in slot 2 page 1 with page 1 selected, so C-BIOS finds the "AB" header
     and calls INIT -- the proven path a game cart takes. *)
  load_cartridge ~mapper:Flat t (disk_rom_bytes ())

(* Serviced in the step loop before the opcode at [pc] runs. Returns true when
   [pc] is a disk BIOS entry the trap handled (moving the CPU state on). *)
let disk_trap t pc =
  if Bytes.length t.disk = 0 then false
  else if pc = disk_init_entry then begin
    if !disk_call_log then
      disk_calls := (pc, Z80.dump_a t.cpu, Z80.dump_bc t.cpu, Z80.dump_de t.cpu,
                     Z80.dump_hl t.cpu, Z80.dump_f t.cpu) :: !disk_calls;
    (* INIT: read the boot sector to 0xC000 and enter it at +0x1e with carry
       clear -- the boot code's first byte is RET NC, a check that the read
       succeeded. *)
    disk_transfer t ~write:false ~sector:0 ~count:1 ~addr:disk_boot_addr;
    (* A real disk ROM's boot procedure enables RAM in page 0 before it loads
       the DOS kernel to 0x0100. Do the same: page 0 to slot 3 (the RAM mapper),
       with its sub-slot on the RAM bank. Page 1 stays the disk ROM (the kernel
       calls DSKIO there); page 2/3 are left as they are. Without RAM in page 0
       the load to 0x0100 lands on the BIOS ROM and is dropped. *)
    t.ppi_a <- (t.ppi_a land 0xfc) lor 0x03;
    t.slot3_sel <- (t.slot3_sel land 0xfc) lor 0x02;
    (* Enter the boot sector at +0x1e with carry SET: its first byte is RET NC,
       which the disk ROM uses to bail when the sector is not bootable. Carry
       set means "boot this", so the code runs instead of returning. *)
    Z80.set_af t.cpu ((0x00 lsl 8) lor 0x01) (* A=0 (drive 0), carry set *);
    Z80.set_pc t.cpu (disk_boot_addr + 0x1e);
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
    (* MSX DISK-BASIC system call (C = function). Only the file-load path the
       boot uses is served, against the FAT12 image: Open File (0x0F), Set DMA
       (0x1A), Random block read (0x27). A=0 success, A=0xFF failure -- the boot
       does INC A / JR Z, so 0 continues and 0xFF branches to its error path. *)
    let c = Z80.dump_bc t.cpu land 0xff in
    let de = Z80.dump_de t.cpu in
    let fcb i = mem_read t ((de + i) land 0xffff) in
    if !disk_call_log then
      disk_calls := (pc, c, Z80.dump_bc t.cpu, de, Z80.dump_hl t.cpu, Z80.dump_f t.cpu)
                    :: !disk_calls;
    let a =
      match c with
      | 0x0f -> (
        let name = String.init 11 (fun i -> Char.chr (fcb (1 + i))) in
        match fat12_open t name with
        | Some data ->
          if !disk_call_log then
            Printf.eprintf "BDOS open '%s' -> %d bytes\n%!" name (Bytes.length data);
          t.disk_open <- Some data;
          let sz = Bytes.length data in
          mem_write t ((de + 0x10) land 0xffff) (sz land 0xff);
          mem_write t ((de + 0x11) land 0xffff) ((sz lsr 8) land 0xff);
          mem_write t ((de + 0x12) land 0xffff) ((sz lsr 16) land 0xff);
          mem_write t ((de + 0x13) land 0xffff) ((sz lsr 24) land 0xff);
          0x00
        | None ->
          if !disk_call_log then Printf.eprintf "BDOS open '%s' -> NOT FOUND\n%!" name;
          0xff)
      | 0x1a ->
        t.disk_dma <- de;
        0x00
      | 0x27 -> (
        match t.disk_open with
        | None -> 0xff
        | Some data ->
          let rec_size =
            let r = fcb 0x0e lor (fcb 0x0f lsl 8) in
            if r = 0 then 128 else r
          in
          let rand_rec =
            fcb 0x21 lor (fcb 0x22 lsl 8) lor (fcb 0x23 lsl 16) lor (fcb 0x24 lsl 24)
          in
          let count = Z80.dump_hl t.cpu in
          let start = rand_rec * rec_size in
          let want = count * rec_size in
          let avail = max 0 (Bytes.length data - start) in
          let n = min want avail in
          for i = 0 to n - 1 do
            mem_write t ((t.disk_dma + i) land 0xffff) (Char.code (Bytes.get data (start + i)))
          done;
          Z80.set_hl t.cpu (n / rec_size);
          if n < want then 0x01 else 0x00)
      | _ -> 0xff
    in
    Z80.set_af t.cpu ((a lsl 8) lor (Z80.dump_f t.cpu land 0xff));
    let sp = Z80.dump_sp t.cpu in
    let ret = mem_read t sp lor (mem_read t (sp + 1) lsl 8) in
    Z80.set_sp t.cpu ((sp + 2) land 0xffff);
    Z80.set_pc t.cpu ret;
    true
  end
  else false

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
      (* A disk BIOS entry is served in OCaml (HLE), not by fetching the ROM's
         opcode there; the trap moves PC on, so charge a nominal call's cycles. *)
      let used = if disk_trap t pc then 18 else Z80.step t.cpu in
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
