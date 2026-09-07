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

type t = {
  cpu : Z80.t;
  vdp : Vdp.t;
  mutable main_rom : Bytes.t;
  mutable logo_rom : Bytes.t;
  mutable sub_rom : Bytes.t;
  mutable cart : Bytes.t;
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

let name t = Printf.sprintf "MSX2/C-BIOS (%dKB RAM)" (Bytes.length t.ram / 1024)

let load_cartridge t rom =
  t.cart <- Bytes.of_string rom;
  (* mem_read 은 카트리지를 슬롯2 페이지0·1 에 둔다. 페이지0 을 슬롯2 로
     돌리면 BIOS(슬롯0) 를 잃어 부트가 안 되니, 페이지1(bits2-3) 만
     슬롯2 로 보인다 — C-BIOS 가 0x4000 의 "AB" 헤더를 찾는 자리. *)
  t.ppi_a <- (t.ppi_a land 0xf3) lor 0x08

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
