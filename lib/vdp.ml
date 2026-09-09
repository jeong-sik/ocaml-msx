(* V9938 의 TMS9918 호환 표면. 모드 비트: M3 = R0 bit1, M2 = R1 bit3,
   M1 = R1 bit4 (텍스트). 테이블 베이스:
   TEXT   NT=R2<<10, PT=R4<<8,  색 = R7 (FG<<4|BG)
   G1     NT=R2<<10, PT=R4<<11, CT=R3<<6 (타일/8 바이트)
   G2     NT=R2<<10, PT=R4<<11, CT=R3<<6 — 어드레싱에 0x800/0x40 미러
   팔레트 쓰기: 0x9A 인덱스, 0x9B 두 번 — 첫 (R<<4|B), 둘째 (G<<4).
   VBlank: 라인 192 진입에서 status0 bit7 과 INT. IE = R1 bit5. *)

type t = {
  regs : int array;
  vram : Bytes.t;
  mutable addr : int;
  mutable write_mode : bool;
  mutable latch_first : bool;
  mutable latch_lo : int;
  mutable read_buf : int;
  mutable pal_idx : int;
  mutable pal_first : int option;
  palette : int array;
  mutable status0 : int;
  mutable status1 : int;
  mutable line : int;
  mutable cycle_in_line : int;
  mutable int_pending : bool;
  mutable cmd_ce : bool;
  (* LMMC/HMMC 전송 상태 — CMR 를 쓰면 진입, R#44(CLR) 쓰기마다
     1바이트가 흘러간다 (openMSX setCmdReg case 0x0C). *)
  mutable tx_active : bool;
  mutable tx_hm : bool;
  mutable tx_dx : int;
  mutable tx_adx : int;
  mutable tx_anx : int;
  mutable tx_nx : int;
  mutable tx_dy : int;
  mutable tx_ny : int;
  mutable cmd_tr : bool;
  (* CMR 발행 전 R#44 쓰기 — 명령 시작 시 첫 픽셀로 소비된다
     (openMSX transfer 플래그는 명령 밖에서도 세운다). *)
  mutable tx_pending : bool;
  (* 포트 쓰기 로그 — 부트 디버깅용 링버퍼. *)
  mutable wlog : (int * int * int) array;
  mutable wlog_i : int;
  (* 최근 명령 발행 이력 — 부트 디버깅용. *)
  mutable cmd_log : (int * int * int * int) list;
}

let lines_per_frame = 262
let cycles_per_line = 228
let visible_lines t = if t.regs.(9) land 0x80 = 0 then 192 else 212

(* V9938 초기 팔레트 (data book appendix 8) — GGRRBB 3비트 채널. *)
let default_palette =
  [| 0x000; 0x000; 0x611; 0x733; 0x117; 0x327; 0x151; 0x627;
     0x171; 0x373; 0x661; 0x664; 0x411; 0x265; 0x555; 0x777 |]

let grb_to_rgb v =
  let g = (v lsr 8) land 7 and r = (v lsr 4) land 7 and b = v land 7 in
  ((r * 255) / 7, (g * 255) / 7, (b * 255) / 7)

let create () =
  let pal = Array.make 16 0 in
  Array.iteri (fun i v -> pal.(i) <- v) default_palette;
  {
    regs = Array.make 47 0;
    vram = Bytes.make 0x20000 '\000';
    addr = 0;
    write_mode = false;
    latch_first = true;
    latch_lo = 0;
    read_buf = 0;
    pal_idx = 0;
    pal_first = None;
    palette = pal;
    status0 = 0;
    status1 = 0;
    line = 0;
    cycle_in_line = 0;
    int_pending = false;
    cmd_ce = false;
    tx_active = false;
    tx_hm = false;
    tx_dx = 0;
    tx_adx = 0;
    tx_anx = 0;
    tx_nx = 0;
    tx_dy = 0;
    tx_ny = 0;
    cmd_tr = false;
    tx_pending = false;
    wlog = Array.make 65536 (0, 0, 0);
    wlog_i = 0;
    cmd_log = [];
  }

(* V9938 화면 모드표 (application manual 표 "screen mode register bits";
   openMSX DisplayMode 의 base 코드와 같은 비트 배치 M1=bit0 … M5=bit4).
   M1 = R#1 bit4, M2 = R#1 bit3, M3 = R#0 bit1, M4 = R#0 bit2, M5 = R#0 bit3.
   표에 없는 조합은 코드를 그대로 든 Undefined 다 — 렌더러가 무엇을 그리든
   관측자는 기계가 무슨 모드라고 말하는지를 받는다. *)
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

let display_mode_code t =
  let r0 = t.regs.(0) and r1 = t.regs.(1) in
  ((r1 lsr 4) land 1)
  lor (((r1 lsr 3) land 1) lsl 1)
  lor (((r0 lsr 1) land 1) lsl 2)
  lor (((r0 lsr 2) land 1) lsl 3)
  lor (((r0 lsr 3) land 1) lsl 4)

let display_mode t =
  match display_mode_code t with
  | 0x00 -> Graphic1
  | 0x04 -> Graphic2
  | 0x08 -> Graphic3
  | 0x0c -> Graphic4
  | 0x10 -> Graphic5
  | 0x14 -> Graphic6
  | 0x1c -> Graphic7
  | 0x02 -> Multicolor
  | 0x01 -> Text1
  | 0x09 -> Text2
  | code -> Undefined code

let display_mode_to_string = function
  | Text1 -> "TEXT1"
  | Text2 -> "TEXT2"
  | Multicolor -> "MULTICOLOR"
  | Graphic1 -> "GRAPHIC1"
  | Graphic2 -> "GRAPHIC2"
  | Graphic3 -> "GRAPHIC3"
  | Graphic4 -> "GRAPHIC4"
  | Graphic5 -> "GRAPHIC5"
  | Graphic6 -> "GRAPHIC6"
  | Graphic7 -> "GRAPHIC7"
  | Undefined code -> Printf.sprintf "UNDEFINED(0x%02x)" code

(* All bitmap users share native pixel geometry. VRAM is stored in physical
   bank order: SCREEN7/8 interleave successive CPU bytes across 64K banks. *)
type bitmap_layout = { width : int; pixels_per_byte : int; planar : bool }

let bitmap_layout t =
  match display_mode t with
  | Graphic5 -> { width = 512; pixels_per_byte = 4; planar = false }
  | Graphic6 -> { width = 512; pixels_per_byte = 2; planar = true }
  | Graphic7 -> { width = 256; pixels_per_byte = 1; planar = true }
  | _ -> { width = 256; pixels_per_byte = 2; planar = false }

let pixels_per_byte t = (bitmap_layout t).pixels_per_byte
let reg16 t lo hi = ((t.regs.(hi) land 1) lsl 8) lor t.regs.(lo)
let reg_y t lo hi = ((t.regs.(hi) land 3) lsl 8) lor t.regs.(lo)

let physical_addr t a =
  let a = a land 0x1ffff in
  if (bitmap_layout t).planar then ((a land 1) lsl 16) lor (a lsr 1)
  else a

let vram_addr t extended x y =
  let l = bitmap_layout t in
  let a = physical_addr t
      ((y * (l.width / l.pixels_per_byte))
       + ((x land (l.width - 1)) / l.pixels_per_byte)) in
  if extended then 0x20000 lor (a land 0xffff) else a

(* This machine has 128K VRAM; expansion RAM reads float high and writes
   are discarded, never aliased to a display page. *)
let read_vram t a = if a >= 0x20000 then 0xff else Char.code (Bytes.get t.vram a)
let write_vram t a v =
  if a < 0x20000 then Bytes.set t.vram a (Char.chr (v land 0xff))

let logical op a c =
  if op land 8 <> 0 && c = 0 then a
  else match op land 7 with
  | 0 -> c
  | 1 -> a land c
  | 2 -> a lor c
  | 3 -> a lxor c
  | 4 -> lnot c
  | _ -> a

let pixel_shift t x =
  let ppb = pixels_per_byte t in
  (ppb - 1 - (x land (ppb - 1))) * (8 / ppb)
let pixel_mask t = (1 lsl (8 / pixels_per_byte t)) - 1
let point t extended x y =
  (read_vram t (vram_addr t extended x y) lsr pixel_shift t x) land pixel_mask t
let pset t extended x y op color =
  let a = vram_addr t extended x y in
  let old = read_vram t a and mask = pixel_mask t and shift = pixel_shift t x in
  let color = logical op ((old lsr shift) land mask) (color land mask) land mask in
  write_vram t a ((old land (lnot (mask lsl shift))) lor (color lsl shift))

let clip_x t ~byte ~reverse ~x ~nx =
  let unit_size = if byte then pixels_per_byte t else 1 in
  let width = (bitmap_layout t).width / unit_size in
  let x = x / unit_size and n = nx / unit_size in
  if x >= width then 1
  else min (if n = 0 then width else n) (if reverse then x + 1 else width - x)
let clip_y ~reverse ~y ~ny =
  let n = if ny = 0 then 1024 else ny in
  if reverse then min n (y + 1) else n

(* 블록 명령 실행 — 즉시 완료 (CE 는 관찰되지 않는다). CPU 동기 전송
   (LMMC/HMMC) 은 R#44 쓰기 구동 transfer_byte 로, POINT/SRCH/LINE/
   LMCM 은 아직 없다. *)
let exec_command t =
  let cmd = t.regs.(46) lsr 4 and op = t.regs.(46) land 15 in
  let arg = t.regs.(45) in
  let reverse_x = arg land 4 <> 0 and reverse_y = arg land 8 <> 0 in
  let mxs = arg land 0x10 <> 0 and mxd = arg land 0x20 <> 0 in
  let sx = reg16 t 32 33 and sy = reg_y t 34 35 in
  let dx = reg16 t 36 37 and dy = reg_y t 38 39 in
  let byte = cmd >= 0xc in
  let nx = if cmd = 0xe then 512 else reg16 t 40 41 in
  let ny = reg_y t 42 43 in
  let src_x = if cmd = 0xe then dx else sx in
  let copy = cmd = 9 || cmd = 0xd || cmd = 0xe in
  let nx =
    let dest = clip_x t ~byte ~reverse:reverse_x ~x:dx ~nx in
    if copy then min dest (clip_x t ~byte ~reverse:reverse_x ~x:src_x ~nx)
    else dest in
  let ny =
    let dest = clip_y ~reverse:reverse_y ~y:dy ~ny in
    if copy then min dest (clip_y ~reverse:reverse_y ~y:sy ~ny) else dest in
  let step_x n = (if reverse_x then -n else n) * (if byte then pixels_per_byte t else 1) in
  let step_y n = if reverse_y then -n else n in
  if cmd = 5 then pset t mxd dx dy op t.regs.(44)
  else if cmd = 8 || cmd = 9 || cmd = 0xc || cmd = 0xd || cmd = 0xe then
    for row = 0 to ny - 1 do
      for col = 0 to nx - 1 do
        let x = dx + step_x col and y = dy + step_y row in
        if byte then begin
          let value = if copy then
              read_vram t (vram_addr t (if cmd = 0xe then mxd else mxs)
                (src_x + step_x col) (sy + step_y row))
            else t.regs.(44) in
          write_vram t (vram_addr t mxd x y) value
        end else begin
          let value = if copy then point t mxs (sx + step_x col) (sy + step_y row)
            else t.regs.(44) in
          pset t mxd x y op value
        end
      done
    done

(* LMMC/HMMC 전송 한 바이트 — 전송 모드에서 R#44(CLR) 쓰기가 곧
   데이터다 (openMSX setCmdReg case 0x0C → executeLmmc/executeHmmc).
   HMMC 는 바이트 단위, LMMC 는 픽셀 단위 pset. *)
let tx_count = ref 0

let transfer_byte t v =
  incr tx_count;
  t.cmd_tr <- true; (* 다음 바이트 즉시 준비 *)
  let arg = t.regs.(45) in
  let dix = arg land 0x04 = 0 and diy = arg land 0x08 = 0 in
  let mxd = arg land 0x20 <> 0 in
  let op = t.regs.(46) land 0x0f in
  let ppb = pixels_per_byte t in
  if t.tx_hm then begin
    write_vram t (vram_addr t mxd t.tx_adx t.tx_dy) v;
    t.tx_adx <- t.tx_adx + ((if dix then 1 else -1) * ppb)
  end else begin
    pset t mxd t.tx_adx t.tx_dy op v;
    t.tx_adx <- t.tx_adx + (if dix then 1 else -1)
  end;
  t.tx_anx <- t.tx_anx - 1;
  if t.tx_anx = 0 then begin
    t.tx_dy <- t.tx_dy + (if diy then 1 else -1);
    t.tx_adx <- t.tx_dx;
    t.tx_anx <- t.tx_nx;
    t.tx_ny <- t.tx_ny - 1;
    if t.tx_ny = 0 then begin
      t.tx_active <- false;
      t.cmd_ce <- false
      (* TR 은 다음 S#2 읽기에서 해제된다 — openMSX commandDone. *)
    end
  end

let start_command t =
  let cmr_v = t.regs.(46) in
  t.cmd_log <- (cmr_v, reg16 t 36 37, reg16 t 38 39, reg16 t 42 43) :: List.filteri (fun i _ -> i < 31) t.cmd_log;
  let cmd = t.regs.(46) lsr 4 in
  if cmd = 0xB || cmd = 0xF then begin
    t.tx_active <- true;
    t.tx_hm <- cmd = 0xF;
    t.tx_dx <- reg16 t 36 37;
    t.tx_adx <- t.tx_dx;
    (* openMSX clipNX_1_byte / clipNX_1_pixel: 한 줄 폭을 넘는 NX 는
       잘린다 — HMMC 는 바이트 단위, LMMC 는 픽셀 단위. *)
    t.tx_nx <- clip_x t ~byte:t.tx_hm ~reverse:(t.regs.(45) land 4 <> 0)
        ~x:t.tx_dx ~nx:(reg16 t 40 41);
    t.tx_anx <- t.tx_nx;
    t.tx_dy <- reg_y t 38 39;
    t.tx_ny <- clip_y ~reverse:(t.regs.(45) land 8 <> 0)
        ~y:t.tx_dy ~ny:(reg_y t 42 43);
    t.cmd_ce <- true;
    t.cmd_tr <- true;
    (* 발행 전 R#44 에 쓰인 첫 픽셀이 큐에 있으면 즉시 소비. *)
    if t.tx_pending then begin
      t.tx_pending <- false;
      transfer_byte t t.regs.(44)
    end
  end
  else begin
    t.tx_active <- false;
    t.cmd_ce <- false;
    t.cmd_tr <- false;
    exec_command t
  end

let set_reg t r v =
  t.regs.(r) <- v land 0xff;
  if r = 44 then begin
    if t.tx_active then transfer_byte t v else t.tx_pending <- true
  end
  else if r = 46 then start_command t
  else if r = 16 then begin
    (* R#16 이 팔레트 인덱스를 다시 고르면 진행 중이던 첫 바이트는 버린다.
       흘러들면 다음 색의 채널이 한 칸 어긋난다. *)
    t.pal_first <- None
  end
  else if r = 0 && v land 0x10 = 0 then
    (* IE1 을 끄면 라인 인터럽트 플래그(S#1 bit0)도 지운다 — openMSX 가
       R#0 쓰기에서 irqHorizontal.reset() 하는 것과 같은 계약. 이 게이트가
       없으면 IE1=0 인 게임이 매 프레임 FH=1 을 읽는다 (룬마스터 II 의
       갤러리 핸들러가 5109 증가 경로를 영원히 건너뛰어 프리즈). *)
    t.status1 <- t.status1 land 0xfe

let palette_rgb t i =
  if i land 0x10 = 0 then grb_to_rgb t.palette.(i land 15)
  else (0, 0, 0)

let log_w t port a v =
  (* 전체 주소 = R#14<<14 | 래치 (0x98 데이터 쓰기 기준). *)
  let full =
    if port land 0xff = 0x98 then (((t.regs.(14) land 7) lsl 14) lor (a land 0x3fff))
    else a
  in
  t.wlog.(t.wlog_i land 65535) <- (port, full, v land 0xff);
  t.wlog_i <- t.wlog_i + 1

let cpu_addr t = physical_addr t (((t.regs.(14) land 7) lsl 14) lor t.addr)
let increment_addr t =
  t.addr <- (t.addr + 1) land 0x3fff;
  if t.addr = 0 && t.regs.(0) land 0x0c <> 0 then
    t.regs.(14) <- (t.regs.(14) + 1) land 7
let prefetch t =
  t.read_buf <- read_vram t (cpu_addr t);
  increment_addr t

let io_write t ~port v =
  log_w t (port land 0xff) t.addr v;
  match port with
  | 0x98 ->
    write_vram t (cpu_addr t) v;
    t.read_buf <- v land 0xff;
    increment_addr t
  | 0x99 ->
    if t.latch_first then begin
      t.latch_lo <- v land 0xff;
      t.latch_first <- false
    end else begin
      let b2 = v land 0xff in
      if b2 land 0x80 <> 0 then begin
        (* 레지스터 쓰기: 첫 바이트가 값, 둘째가 0x80|번호. *)
        let r = b2 land 0x3f in
        if r < 47 then set_reg t r (t.latch_lo land 0xff)
      end else begin
        t.addr <- ((b2 land 0x3f) lsl 8) lor t.latch_lo;
        t.write_mode <- b2 land 0x40 <> 0;
        if not t.write_mode then prefetch t
      end;
      t.latch_first <- true
    end
  | 0x9A ->
    (* V9938 palette bytes: 0RRR0BBB, then 00000GGG. *)
    (match t.pal_first with
     | None -> t.pal_first <- Some (v land 0xff)
     | Some lo ->
       let r = (lo lsr 4) land 7 and b = lo land 7 in
       let g = v land 7 in
       let idx = t.regs.(16) land 0x0f in
       t.palette.(idx) <- (g lsl 8) lor (r lsl 4) lor b;
       t.regs.(16) <- (idx + 1) land 0x0f;
       t.pal_first <- None)
  | 0x9B ->
    (* 간접 레지스터 쓰기: R#17 이 포인터, bit7=1 이면 증가 금지. *)
    let p = t.regs.(17) in
    set_reg t (p land 0x3f) v;
    if p land 0x80 = 0 then t.regs.(17) <- (p + 1) land 0x3f
  | _ -> ()

let io_read t ~port =
  match port with
  | 0x98 ->
    let v = t.read_buf in
    prefetch t;
    v
  | 0x99 ->
    let s =
      match t.regs.(15) land 0x0f with
      | 0 ->
        let x = t.status0 in
        t.status0 <- 0;
        t.int_pending <- false;
        x
      | 1 ->
        let x = t.status1 in
        t.status1 <- 0;
        x
      | 2 ->
        (* TR: 전송 가능. 즉시 실행이라 전송 중엔 언제나 받을 수
           있다 — 읽어도 유지. 완료 직후에는 한 번 1을 보여주고
           해제한다 (openMSX commandDone: "TR is reset when S#2 is
           read next") — 완료 감지는 그다음 읽기부터 TR=0 && CE=0. *)
        let v =
          ((if t.cmd_ce then 1 else 0)
           lor (if t.cmd_tr || t.tx_active then 0x80 else 0))
          lor 0x7e
        in
        if not t.tx_active then t.cmd_tr <- false;
        v
      | _ -> 0xff
    in
    t.latch_first <- true;
    s
  | _ -> 0xff

let advance t ~cycles =
  let hit = ref false in
  t.cycle_in_line <- t.cycle_in_line + cycles;
  while t.cycle_in_line >= cycles_per_line do
    t.cycle_in_line <- t.cycle_in_line - cycles_per_line;
    t.line <- t.line + 1;
    if t.line = visible_lines t then begin
      t.status0 <- t.status0 lor 0x80;
      t.int_pending <- true;
      hit := true;
      (* 인터럽트 발생 시 상태 레지스터 선택(R#15)을 0 으로 되돌린다 —
         V9938 계약. BIOS KEYINT 는 `in a,(0x99)` 후 `jp p,int_end` 로
         VBlank 플래그(S#0 bit7)를 검사하는데, 게임이 S#1 선택(R#15=1)을
         남긴 채 인터럽트가 걸리면 이 리셋이 없는 한 KEYINT 는 S#1 을
         읽고 스캔 없이 종료한다 — 키가 영원히 KEYBUF 에 들어가지 않는다
         (룬마스터 1 의 CONIN 이 비는 경로). *)
      t.regs.(15) <- 0
    end;
    (* 라인 인터럽트: 표시 라인이 R#19 와 같아질 때 S#1 bit0. FH 플래그는
       IE1(R#0 bit4) 이 켜져 있을 때만 래치된다 — V9938 정본(openMSX
       execHScan: "if (controlRegs[0] & 0x10)"). IE1 없이도 세우면 IE1=0 으로
       라인 인터럽트를 쓰지 않는 게임이 매 프레임 S#1 읽기에서 1 을 본다. *)
    if
      t.regs.(0) land 0x10 <> 0
      && t.line < visible_lines t && t.line = t.regs.(19)
    then begin
      t.status1 <- t.status1 lor 1;
      t.int_pending <- true;
      hit := true
    end;
    if t.line >= lines_per_frame then t.line <- 0
  done;
  !hit

let int_active t =
  let ie0 = t.regs.(1) land 0x20 <> 0 and ie1 = t.regs.(0) land 0x10 <> 0 in
  (t.status0 land 0x80 <> 0 && ie0) || (t.status1 land 1 <> 0 && ie1)

let blanked t = t.regs.(1) land 0x40 = 0
let vram t = t.vram
let tx_state t = (t.tx_active, !tx_count, t.tx_ny, t.tx_anx, t.tx_dy)
let cmd_history t = List.rev t.cmd_log
let regs t = t.regs
let status0 t = t.status0
let line_now t = (t.line, t.cycle_in_line)

let write_log t =
  let n = min t.wlog_i 65536 in
  let out = ref [] in
  for k = 1 to n do
    out := t.wlog.((t.wlog_i - k) land 65535) :: !out
  done;
  !out

(* 모드 판정 (V9938 모드표: M5=R0bit3, M4=R0bit2, M3=R0bit1,
   M2=R1bit3, M1=R1bit4). G4 비트맵 = MSX2 SCREEN5 (M4+M3),
   베이스는 R#2 의 A16/A15 (bit6/5) × 32K — 하위 5비트는 11111 고정.
   멀티컬러(M2) 는 아직 없다. *)
let mode_text t = t.regs.(1) land 0x10 <> 0
let mode_g2 t =
  not (mode_text t) && t.regs.(0) land 0x02 <> 0
  && t.regs.(0) land 0x04 = 0
(* G4(SCREEN5) 는 M5 가 꺼져 있다 — 0x06 마스크만으론 SCREEN8(M5+M4+M3,
   R0=0x0E) 도 걸려버린다. *)
let mode_s5 t =
  not (mode_text t) && t.regs.(0) land 0x08 = 0 && t.regs.(0) land 0x06 = 0x06

(* SCREEN6/7/8 — 512폭 비트맵 계열(GRAPHIC 5/6/7). R0 bit3 이 이 셋의
   공통 비트(GRAPHIC 4 는 bit3=0 이라 mode_s5 가 먼저 잡는다). *)
let mode_bitmap_hi t = not (mode_text t) && t.regs.(0) land 0x08 <> 0

let vram_read t addr = Char.code (Bytes.get t.vram (addr land 0x1ffff))

(* 스프라이트 모드 1 — SCREEN1-3. SAT = R#5<<7 (32엔트리 × 4바이트:
   Y, X, 패턴번호, 컬러), PT = R#6<<11. R#1 bit0 = MAG(2배), bit1 =
   SIZE(16×16). Y 값은 실제보다 1 크고, 0xD0 인 엔트리부터 뒤는 없으며
   0xFF 는 그 엔트리만 화면 밖이다. 컬러 0 은 투명, 컬러 bit7(EC) 은
   X 를 16px 왼쪽으로 민다. 우선순위는 번호가 낮은 쪽이 위 — 큰 번호부터
   그린다. 라인당 표시 제한(4/8)과 5th-sprite·충돌 플래그(S#0 bit6/5)
   는 아직 없다. *)
let render_sprites t ~w ~h put =
  let vr a = Char.code (Bytes.get t.vram a) in
  let sat = (t.regs.(5) land 0x7f) lsl 7 in
  let pt = (t.regs.(6) land 0x07) lsl 11 in
  let size16 = t.regs.(1) land 0x02 <> 0 in
  let mag = t.regs.(1) land 0x01 <> 0 in
  let pat_len = if size16 then 32 else 8 in
  let edge = if size16 then 16 else 8 in
  let step = if mag then 2 else 1 in
  let sput x y c =
    if x >= 0 && x < w && y >= 0 && y < h then put x y c
  in
  (* 표시할 엔트리를 번호순으로 모은 뒤 역순으로 그린다 — 0xD0 는
     목록 수집 자체를 끝낸다. *)
  let active = ref [] in
  (try
     for i = 0 to 31 do
       let e = sat + i * 4 in
       match vr e with
       | 0xd0 -> raise Exit
       | 0xff -> () (* 이 엔트리만 화면 밖 *)
       | _ -> active := i :: !active
     done
   with Exit -> ());
  List.iter
    (fun i ->
      let e = sat + i * 4 in
      let y0 = ((vr e - 1) land 0xff) and x_raw = vr (e + 1) in
      let pat = vr (e + 2) and col = vr (e + 3) in
      let c = col land 0x0f in
      let x0 = if col land 0x80 <> 0 then x_raw - 16 else x_raw in
      if c <> 0 then
        let pbase =
          pt + (if size16 then (pat land 0xfc) * pat_len else pat * pat_len)
        in
        for row = 0 to edge - 1 do
          (* 16×16 은 한 줄이 2바이트: 상위 바이트가 왼쪽 8px. *)
          let bits =
            if size16 then
              (vr (pbase + row * 2) lsl 8) lor vr (pbase + row * 2 + 1)
            else vr (pbase + row)
          in
          let top = if size16 then 15 else 7 in
          for b = 0 to edge - 1 do
            if (bits lsr (top - b)) land 1 = 1 then
              for my = 0 to step - 1 do
                for mx = 0 to step - 1 do
                  sput (x0 + b * step + mx) (y0 + row * step + my) c
                done
              done
          done
        done)
    !active

let frame_dims t =
  match display_mode t with
  | Graphic4 | Graphic5 | Graphic6 | Graphic7 ->
      ((bitmap_layout t).width, if t.regs.(9) land 0x80 = 0 then 192 else 212)
  | _ -> (256, 192)

let frame_rgb t =
  let w, h = frame_dims t in
  let img = Bytes.make (w * h * 3) '\000' in
  let put x y c =
    let (r, g, b) = palette_rgb t c in
    let i = (y * w + x) * 3 in
    Bytes.set img i (Char.chr r);
    Bytes.set img (i + 1) (Char.chr g);
    Bytes.set img (i + 2) (Char.chr b)
  in
  if blanked t then Bytes.to_string img
  else begin
    let vr addr = Char.code (Bytes.get t.vram (addr land 0x1ffff)) in
    let r = t.regs in
    if mode_s5 t || mode_bitmap_hi t then begin
      let layout = bitmap_layout t in
      let page = if layout.planar then ((r.(2) lsr 6) land 1) * 256
        else ((r.(2) lsr 5) land 3) * 256 in
      for y = 0 to h - 1 do
        let vy = page + ((y + r.(23)) land 255) in
        for x = 0 to w - 1 do
          let color = point t false x vy in
          if display_mode t = Graphic7 then begin
            (* SCREEN8 byte = GGGRRRBB, one byte per native pixel. *)
            let i = (y * w + x) * 3 in
            Bytes.set img i (Char.chr (((color lsr 2) land 7) * 255 / 7));
            Bytes.set img (i + 1) (Char.chr (((color lsr 5) land 7) * 255 / 7));
            Bytes.set img (i + 2) (Char.chr ((color land 3) * 255 / 3))
          end else put x y color
        done
      done
    end
    else if mode_text t then begin
      (* TEXT1: 40×24, 6×8 셀 — 좌우 8px 는 R7 배경색. *)
      let nt = (r.(2) land 0x7f) lsl 10 in
      let pt = (r.(4) land 0x7) lsl 8 in
      let fg = (r.(7) lsr 4) land 0xf and bg = r.(7) land 0xf in
      for y = 0 to h - 1 do
        for x = 0 to 7 do put x y bg done;
        for x = w - 8 to w - 1 do put x y bg done
      done;
      for row = 0 to 23 do
        for col = 0 to 39 do
          let ch = vr (nt + row * 40 + col) in
          for py = 0 to 7 do
            let pat = vr (pt + ch * 8 + py) in
            for px = 0 to 5 do
              let x = 8 + col * 6 + px in
              let y = row * 8 + py in
              if (pat lsr (7 - px)) land 1 = 1 then put x y fg else put x y bg
            done
          done
        done
      done
    end
    else begin
      let g2 = mode_g2 t in
      let nt = (r.(2) land 0x7f) lsl 10 in
      (* 패턴/컬러 주소 = base land index (openMSX VRAMWindow::readNP).
         base 는 레지스터를 제자리에 두고 안 쓰는 하위 비트를 1 로 채운 값,
         index 는 안 쓰는 상위 비트를 1 로 채운 값이다. G2 의 index 는
         13비트(third<<11 | name<<3 | 행) 라 R#3 bits0-6 과 R#4 bits0-1 이
         index 상위 비트의 AND 마스크가 된다 — TMS 시절 미러 트릭이 여기서
         나온다. G2 는 컬러도 패턴처럼 행마다 한 바이트다. G1 은 패턴
         11비트(name<<3 | 행), 컬러 5비트(name>>3). *)
      let pt_base = ((r.(4) land 0x3f) lsl 11) lor 0x7ff in
      let ct_base = ((r.(10) land 0x07) lsl 14) lor (r.(3) lsl 6) lor 0x3f in
      for row = 0 to 23 do
        let third = row lsr 3 in
        for col = 0 to 31 do
          let name = vr (nt + (row * 32) + col) in
          for py = 0 to 7 do
            let pat_addr, col_addr =
              if g2 then begin
                let index = (lnot 0 lsl 13) lor (third lsl 11) lor (name lsl 3) lor py in
                (pt_base land index, ct_base land index)
              end
              else
                ( pt_base land ((lnot 0 lsl 11) lor (name lsl 3) lor py),
                  ct_base land ((lnot 0 lsl 6) lor (name lsr 3)) )
            in
            let bits = vr pat_addr in
            let col_byte = vr col_addr in
            let fg = (col_byte lsr 4) land 0xf and bg = col_byte land 0xf in
            for px = 0 to 7 do
              let x = (col * 8) + px in
              let y = (row * 8) + py in
              if (bits lsr (7 - px)) land 1 = 1 then put x y fg else put x y bg
            done
          done
        done
      done;
      (* SCREEN1-3 에서만 모드 1 스프라이트를 올린다 (TEXT 는 스프라이트가
         없고 SCREEN5+ 는 모드 2 가 따로 있다). *)
      render_sprites t ~w ~h put
    end;
    Bytes.to_string img
  end

let write_state w t =
  State_codec.put_int_array w t.regs;
  State_codec.put_bytes w t.vram;
  State_codec.put_int_array w t.palette;
  State_codec.put_bool w (Option.is_some t.pal_first);
  Option.iter (State_codec.put_int w) t.pal_first;
  State_codec.put_int w t.addr;
  State_codec.put_int w t.latch_lo;
  State_codec.put_int w t.read_buf;
  State_codec.put_int w t.pal_idx;
  State_codec.put_int w t.status0;
  State_codec.put_int w t.status1;
  State_codec.put_int w t.line;
  State_codec.put_int w t.cycle_in_line;
  State_codec.put_int w t.tx_dx;
  State_codec.put_int w t.tx_adx;
  State_codec.put_int w t.tx_anx;
  State_codec.put_int w t.tx_nx;
  State_codec.put_int w t.tx_dy;
  State_codec.put_int w t.tx_ny;
  State_codec.put_bool w t.write_mode;
  State_codec.put_bool w t.latch_first;
  State_codec.put_bool w t.int_pending;
  State_codec.put_bool w t.cmd_ce;
  State_codec.put_bool w t.tx_active;
  State_codec.put_bool w t.tx_hm;
  State_codec.put_bool w t.cmd_tr;
  State_codec.put_bool w t.tx_pending;
  ()

let read_state r t =
  State_codec.fill_int_array r ~min:0 ~max:255 t.regs;
  let vram = State_codec.get_bytes r in
  if Bytes.length vram <> Bytes.length t.vram then State_codec.fail "invalid VRAM size";
  Bytes.blit vram 0 t.vram 0 (Bytes.length vram);
  State_codec.fill_int_array r ~min:0 ~max:0x777 t.palette;
  t.pal_first <- (if State_codec.get_bool r then Some (State_codec.get_int r ~min:0 ~max:255) else None);
  t.addr <- State_codec.get_int r ~min:(0) ~max:16383;
  t.latch_lo <- State_codec.get_int r ~min:(0) ~max:255;
  t.read_buf <- State_codec.get_int r ~min:(0) ~max:255;
  t.pal_idx <- State_codec.get_int r ~min:(0) ~max:15;
  t.status0 <- State_codec.get_int r ~min:(0) ~max:255;
  t.status1 <- State_codec.get_int r ~min:(0) ~max:255;
  t.line <- State_codec.get_int r ~min:(0) ~max:261;
  t.cycle_in_line <- State_codec.get_int r ~min:(0) ~max:227;
  t.tx_dx <- State_codec.get_int r ~min:(0) ~max:511;
  (* ARG and mode can change during a transfer: permit every coordinate
     reachable across 512 units at up to four pixels per byte. *)
  t.tx_adx <- State_codec.get_int r ~min:(-2048) ~max:2559;
  t.tx_anx <- State_codec.get_int r ~min:(0) ~max:512;
  t.tx_nx <- State_codec.get_int r ~min:(0) ~max:512;
  t.tx_dy <- State_codec.get_int r ~min:(-1024) ~max:2047;
  t.tx_ny <- State_codec.get_int r ~min:(0) ~max:1024;
  t.write_mode <- State_codec.get_bool r;
  t.latch_first <- State_codec.get_bool r;
  t.int_pending <- State_codec.get_bool r;
  t.cmd_ce <- State_codec.get_bool r;
  t.tx_active <- State_codec.get_bool r;
  t.tx_hm <- State_codec.get_bool r;
  t.cmd_tr <- State_codec.get_bool r;
  t.tx_pending <- State_codec.get_bool r;
  if t.tx_active && (t.tx_nx = 0 || t.tx_anx = 0 || t.tx_anx > t.tx_nx || t.tx_ny = 0) then
    State_codec.fail "invalid active VDP transfer"
