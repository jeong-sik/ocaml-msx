(* V9938 의 TMS9918 호환 표면. 모드 비트: M3 = R0 bit1, M2 = R1 bit3,
   M1 = R1 bit4 (텍스트). 테이블 베이스:
   TEXT   NT=R2<<10, PT=R4<<11, 색 = R7 (FG<<4|BG)
   G1     NT=R2<<10, PT=R4<<11, CT=R3<<6 (타일/8 바이트)
   G2     NT=R2<<10, PT=R4<<11, CT=R3<<6 — 어드레싱에 0x800/0x40 미러
   팔레트 쓰기: 0x9A 인덱스, 0x9B 두 번 — 첫 (R<<4|B), 둘째 (G<<4).
   VBlank: 라인 192 진입에서 status0 bit7 과 INT. IE = R1 bit5. *)

(* A CPU-synchronised transfer: LMMC/HMMC take one dot or byte per R#44
   write, LMCM hands one dot per S#7 read. *)
type transfer = Lmmc | Hmmc | Lmcm

(* The vertical timing between two display starts, in the display line
   counter's own lines (openMSX master, VDP::scheduleLineCountReset and
   execLineCountReset, measured on FS-A1GT and FS-A1ST). The counter is not
   told about the frame start: it counts on from display line 0 until the
   reset in the next top border. Each constructor names the event still
   ahead and carries the line where it happens. *)
type vertical =
  | Before_frame_start
      (** Counting the display and the borders; the frame starts at
          [frame_start]. *)
  | Before_reset of int
      (** The frame has started (EO flipped, 50/60 Hz latched); the counter
          resets on this line. *)
  | Before_display of int
      (** The counter has reset; it reads 0 again, at display line 0, on this
          line. *)

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
  (* S#3-S#6: where the last sprite collision was seen, +12 / +8 as the
     V9938 reports it (openMSX SpriteChecker collisionX/Y). *)
  mutable collision_x : int;
  mutable collision_y : int;
  mutable line : int;
  mutable cycle_in_line : int;
  (* S#2 EO: the field parity, flipped once a frame (openMSX frameStart). *)
  mutable even_odd : bool;
  (* R#9 bit 1 as latched at the frame start: 313 lines a frame (PAL,
     50 Hz) or 262 (NTSC, 60 Hz), openMSX frameStart. *)
  mutable pal : bool;
  (* The line where the current frame starts, counted like [line]; frame
     starts are [lines_per_frame] lines apart whatever the display does. *)
  mutable frame_start : int;
  mutable vertical : vertical;
  (* R#13 blinking (openMSX blinkState / blinkCount): [blink_on] is the
     ON period, which shows the R#12 colours in TEXT2 and the even page in
     the bitmap modes; [blink_count] is the frames left in this period, 0
     when the colours do not alternate. *)
  mutable blink_on : bool;
  mutable blink_count : int;
  (* R#18 vertical adjust as latched by the display line counter reset:
     lines the display sits below its neutral place, -7..8 (openMSX
     scheduleLineCountReset reads getVerticalAdjust there). *)
  mutable v_shift : int;
  (* R#0, R#1 and R#8 as the CPU last wrote them. Their display mode, blank
     and sprite-disable bits reach [regs] only where the next line starts
     ({!take_deferred}); their other bits apply at once. *)
  mutable written_r0 : int;
  mutable written_r1 : int;
  mutable written_r8 : int;
  mutable int_pending : bool;
  mutable cmd_ce : bool;
  (* 전송 상태 — CMR 를 쓰면 진입한다. tx_* 는 걷는 모서리다: LMMC/HMMC 는
     DX/DY 에서, LMCM 은 SX/SY 에서 출발한다 (openMSX setCmdReg case 0x0C). *)
  mutable tx : transfer option;
  mutable tx_dx : int;
  mutable tx_adx : int;
  mutable tx_anx : int;
  mutable tx_nx : int;
  mutable tx_dy : int;
  mutable tx_ny : int;
  mutable cmd_tr : bool;
  (* 명령 밖에서 들어온 전송 요청(R#44 쓰기, S#7 읽기) — 다음 LMMC/HMMC
     시작 시 첫 점으로 소비된다 (openMSX transfer 플래그는 명령 밖에서도
     세운다). *)
  mutable tx_pending : bool;
  (* S#2 BD: SRCH found its border colour; reading S#9 clears it. *)
  mutable cmd_bd : bool;
  (* ASX, the engine's source-X counter, shown as S#8/S#9 (openMSX
     getBorderX: the hardware returns ASX whatever command ran). LINE keeps
     its Bresenham sum here while it runs. *)
  mutable cmd_asx : int;
  (* A running command other than a transfer (CE set, [tx] None) takes one
     VRAM access at a time ({!engine_step}). [cmd_wait] is the ticks from
     now to its next access; [cmd_phase] is which access of a dot or byte
     comes next; [cmd_adx]/[cmd_anx] are the destination X and the units
     left in this row (LINE counts its dots up); [cmd_src]/[cmd_dst] hold
     the byte read for a later write (openMSX tmpSrc/tmpDst). *)
  mutable cmd_wait : int;
  mutable cmd_phase : int;
  mutable cmd_adx : int;
  mutable cmd_anx : int;
  mutable cmd_src : int;
  mutable cmd_dst : int;
  (* The slot a CPU VRAM access has claimed, in ticks from now: an engine
     access due there moves to the next slot (openMSX stealAccessSlot). *)
  mutable cmd_steal : int option;
  (* 포트 쓰기 로그 — 부트 디버깅용 링버퍼. *)
  mutable wlog : (int * int * int) array;
  mutable wlog_i : int;
  (* 최근 명령 발행 이력 — 부트 디버깅용. *)
  mutable cmd_log : (int * int * int * int) list;
  (* Raster: lines scanned this frame at native width (512 × 212 × 3,
     [raster_wide] marks the 512-wide ones), and the frame the last VBlank
     published. *)
  raster : Bytes.t;
  raster_wide : Bytes.t;
  shown : Bytes.t;
  mutable shown_w : int;
  mutable shown_h : int;
}

let cycles_per_line = 228
let ticks_per_cycle = 6
let ticks_per_line = cycles_per_line * ticks_per_cycle

(* This model's line starts at the right border, 1282 ticks after the sync
   of openMSX's line in the bitmap modes. The command slots and the
   power-on point count from it; R#18 horizontal adjust, which moves the
   border, is not applied to them. *)
let right_border_tick = 1282
let lines_per_frame t = if t.pal then 313 else 262
let visible_lines t = if t.regs.(9) land 0x80 = 0 then 192 else 212

(* R#18 set-adjust: bits 3-0 move the display right and bits 7-4 move it
   down, each by (nibble xor 7) - 7, from -7 to 8; 0 is neutral (openMSX
   execHorAdjust and getVerticalAdjust keep nibble xor 7, neutral 7). *)
let adjust nibble = (nibble lxor 7) - 7
let horizontal_adjust t = adjust (t.regs.(18) land 0x0f)
let vertical_adjust t = adjust (t.regs.(18) lsr 4)

(* The display line counter resets 8 + getVerticalAdjust() lines after the
   frame start, 15 at the neutral R#18 (openMSX scheduleLineCountReset). *)
let reset_delay t = 15 + vertical_adjust t

(* From the reset to display line 0, by R#9 as it is at the reset: 17 or 27
   lines at 60 Hz, 44 or 54 at 50 Hz, for 212 or 192 lines (openMSX
   execLineCountReset). *)
let lines_to_display t =
  (if t.regs.(9) land 0x02 <> 0 then 44 else 17)
  + if t.regs.(9) land 0x80 <> 0 then 0 else 10

(* Restore bounds. The counter runs at most 8 + 15 lines from a frame start
   to its reset and 54 from there to display line 0, and a frame start
   falls at most on counter line 313 - 8 - 17. *)
let max_reset_delay = 23
let max_lines_to_display = 54
let max_counter_line = 313 - 8 - 17 + max_reset_delay + max_lines_to_display

(* The line where display line 0 comes back, once the reset has fixed it. *)
let next_display_line t =
  match t.vertical with
  | Before_display at -> Some at
  | Before_frame_start | Before_reset _ -> None

(* Whether the counter, on the line just finished, still counted up from
   display line 0 rather than up to it from the reset: only those lines can
   match R#19 (openMSX scheduleHScan: after the reset, "never"). *)
let counting_display t =
  match t.vertical with
  | Before_frame_start | Before_reset _ -> true
  | Before_display _ -> false

(* An R#18 write moves a reset still ahead of the beam; one already behind
   it keeps its place (openMSX scheduleLineCountReset). *)
let move_line_count_reset t =
  match t.vertical with
  | Before_reset _ ->
    let at = t.frame_start + reset_delay t in
    if at > t.line then t.vertical <- Before_reset at
  | Before_frame_start | Before_display _ -> ()

(* Frame timing in this model's coordinates. Line 0 is display line 0, and a
   line runs from one right border to the next: the line interrupt fires at
   the boundary, as openMSX scheduleHScan puts it at getRightBorder(). One
   Z80 cycle is 6 VDP ticks (1368 ticks, 228 cycles a line).

   HR (openMSX getHR) is 1 for 312 ticks after the right border, 404 in the
   text modes: 52 and 67.3 cycles.
   VR (openMSX readStatusReg case 2) drops one line before display start, at
   tick 202 of the line above display line 0. That is 1368 - 1282 + 202 =
   288 ticks, 48 cycles, into our last line. It rises with S#0 F at the
   display end, tick 202 of the line below the last display line (openMSX
   scheduleVScan, displayStart + lines * TICKS_PER_LINE): 48 cycles into
   our line [visible_lines]. *)
let hr_cycles_graphic = 52
let hr_cycles_text = 68
let vr_fall_cycle = 48

(* Raster buffer geometry: the widest native line by the tallest frame. *)
let max_width = 512
let max_lines = 212
let stride = max_width * 3

(* V9938 초기 팔레트 (data book appendix 8) — GGRRBB 3비트 채널. *)
let default_palette =
  [| 0x000; 0x000; 0x611; 0x733; 0x117; 0x327; 0x151; 0x627;
     0x171; 0x373; 0x661; 0x664; 0x411; 0x265; 0x555; 0x777 |]

let grb_to_rgb v =
  let g = (v lsr 8) land 7 and r = (v lsr 4) land 7 and b = v land 7 in
  ((r * 255) / 7, (g * 255) / 7, (b * 255) / 7)

(* Power-on is a frame start: openMSX VDP::reset calls frameStart, which
   flips EO out of statusReg2's 0x0C and schedules the counter reset. The
   frame start is placed as far above display line 0 as the reset and the
   top border reach (42 lines with R#9 and R#18 at 0), so the first frame
   is as long as any other. Its tick 0 is 86 ticks into this model's line,
   which begins at the right border. *)
let power_on t =
  let delay = reset_delay t in
  t.frame_start <- lines_per_frame t - delay - lines_to_display t;
  t.line <- t.frame_start;
  t.cycle_in_line <- (ticks_per_line - right_border_tick) / ticks_per_cycle;
  t.even_odd <- true;
  t.vertical <- Before_reset (t.frame_start + delay)

let create () =
  let pal = Array.make 16 0 in
  Array.iteri (fun i v -> pal.(i) <- v) default_palette;
  let t = {
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
    collision_x = 0;
    collision_y = 0;
    line = 0;
    cycle_in_line = 0;
    even_odd = false;
    pal = false;
    frame_start = 0;
    vertical = Before_frame_start;
    blink_on = false;
    blink_count = 0;
    v_shift = 0;
    written_r0 = 0;
    written_r1 = 0;
    written_r8 = 0;
    int_pending = false;
    cmd_ce = false;
    tx = None;
    tx_dx = 0;
    tx_adx = 0;
    tx_anx = 0;
    tx_nx = 0;
    tx_dy = 0;
    tx_ny = 0;
    cmd_tr = false;
    tx_pending = false;
    cmd_bd = false;
    cmd_asx = 0;
    cmd_wait = 0;
    cmd_phase = 0;
    cmd_adx = 0;
    cmd_anx = 0;
    cmd_src = 0;
    cmd_dst = 0;
    cmd_steal = None;
    wlog = Array.make 65536 (0, 0, 0);
    wlog_i = 0;
    cmd_log = [];
    raster = Bytes.make (stride * max_lines) '\000';
    raster_wide = Bytes.make max_lines '\000';
    shown = Bytes.make (stride * max_lines) '\000';
    shown_w = 256;
    shown_h = 192;
  } in
  power_on t;
  t

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

(* HR's length follows openMSX DisplayMode::isTextMode: TEXT1, TEXT2 and
   TEXT1Q (code 0x05, which this table leaves Undefined). *)
let text_timing t =
  match display_mode t with
  | Text1 | Text2 | Undefined 0x05 -> true
  | Multicolor | Graphic1 | Graphic2 | Graphic3 | Graphic4 | Graphic5
  | Graphic6 | Graphic7 | Undefined _ -> false

let horizontal_retrace t =
  t.cycle_in_line < (if text_timing t then hr_cycles_text else hr_cycles_graphic)

(* Where a line's processing starts: 144 ticks after its sync, in the left
   erase (openMSX syncAtNextLine; R#18 moves it with the border, so it
   stays put relative to the right border). This model's line starts at
   the right border of the line above, 1282 ticks into it in the graphic
   modes and 1254 in the text modes, so the point is 230 or 258 ticks in:
   a write before tick 230 (cycle 38) is taken for this line, a later one
   for the next. *)
let line_start_cycle t = if text_timing t then 43 else 39

let vertical_retrace t =
  let visible = visible_lines t in
  (t.line > visible || (t.line = visible && t.cycle_in_line >= vr_fall_cycle))
  && not
       (next_display_line t = Some (t.line + 1)
        && t.cycle_in_line >= vr_fall_cycle)

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
(* Dot [x] of byte [a], whose value was read as [old], becomes [color]
   through the logical operation. *)
let pset_byte t a ~old x op color =
  let mask = pixel_mask t and shift = pixel_shift t x in
  let color = logical op ((old lsr shift) land mask) (color land mask) land mask in
  write_vram t a ((old land (lnot (mask lsl shift))) lor (color lsl shift))

let pset t extended x y op color =
  let a = vram_addr t extended x y in
  pset_byte t a ~old:(read_vram t a) x op color

let clip_x t ~byte ~reverse ~x ~nx =
  let unit_size = if byte then pixels_per_byte t else 1 in
  let width = (bitmap_layout t).width / unit_size in
  let x = x / unit_size and n = nx / unit_size in
  if x >= width then 1
  else min (if n = 0 then width else n) (if reverse then x + 1 else width - x)
let clip_y ~reverse ~y ~ny =
  let n = if ny = 0 then 1024 else ny in
  if reverse then min n (y + 1) else n

(* CMR bits 7-4 (openMSX VDPCmdEngine::executeCommand). 0-3 stop the
   engine: 0 is STOP and 1-3 abort the same way. The CPU-synchronised
   transfers are {!transfer}. *)
type block = Lmmv | Lmmm | Hmmv | Hmmm | Ymmm

type command =
  | Stop
  | Point
  | Pset
  | Srch
  | Line
  | Block of block
  | Transfer of transfer

let command_of_cmr v =
  match (v lsr 4) land 0x0f with
  | 0 | 1 | 2 | 3 -> Stop
  | 4 -> Point
  | 5 -> Pset
  | 6 -> Srch
  | 7 -> Line
  | 8 -> Block Lmmv
  | 9 -> Block Lmmm
  | 0xa -> Transfer Lmcm
  | 0xb -> Transfer Lmmc
  | 0xc -> Block Hmmv
  | 0xd -> Block Hmmm
  | 0xe -> Block Ymmm
  | _ -> Transfer Hmmc (* 0xF; a 4-bit field has no other value *)

(* The V9938 engine runs only in GRAPHIC 4-7; elsewhere a CMR write ends
   at once (openMSX VDPCmdEngine::executeCommand, scrMode < 0). *)
let commands_enabled t =
  match display_mode t with
  | Graphic4 | Graphic5 | Graphic6 | Graphic7 -> true
  | Text1 | Text2 | Multicolor | Graphic1 | Graphic2 | Graphic3 | Undefined _ ->
    false

(* NX and NY are 10 bits, as the Y registers (openMSX setCmdReg 0x09/0x0B). *)
let reg_nx t = reg_y t 40 41
let reg_ny t = reg_y t 42 43

(* The engine runs on SX..NY themselves and leaves them where it stopped,
   so a command the CPU starts without rewriting them carries on from
   there (openMSX VDPCmdEngine keeps them as live state; the CPU cannot
   read them back). Y and NY are 10 bits and wrap. *)
let set_reg_y t lo hi v =
  let v = v land 0x3ff in
  t.regs.(lo) <- v land 0xff;
  t.regs.(hi) <- v lsr 8

(* [rows] finished rows of a block command or transfer move DY (and SY when
   the command reads a source area) one line each in the ARG DIY direction
   and count NY down (openMSX executeLmmv ... executeHmmc: "SY += TY;
   DY += TY; --NY"). DX, SX and NX stay. *)
type row_registers = Dst | Src | Src_and_dst

let finish_rows t regs ~rows ~step =
  let move lo hi = set_reg_y t lo hi (reg_y t lo hi + (step * rows)) in
  (match regs with
   | Dst -> move 38 39
   | Src -> move 34 35
   | Src_and_dst -> move 34 35; move 38 39);
  set_reg_y t 42 43 (reg_ny t - rows)

(* ARG (R#45) bits. *)
let arg_maj = 0x01
let arg_eq = 0x02
let arg_dix = 0x04
let arg_diy = 0x08
let arg_mxs = 0x10
let arg_mxd = 0x20

(* POINT, SRCH and LMCM read absent expansion VRAM as 0xFF, unmasked
   (openMSX executePoint: COL = 0xFF when !hasExtendedVRAM). *)
let cmd_point t extended x y = if extended then 0xff else point t false x y

(* Command timing. The engine reaches VRAM only in the access slots the
   display fetch leaves free, and each step of a command waits at least a
   fixed number of VDP ticks after the previous access. A block command
   therefore runs for many lines with CE set, and the CPU sees VRAM half
   done in between. The slot positions are measured on a real V9938 in
   bitmap modes (openMSX VDPAccessSlots, documented at
   openmsx.org/vdp-vram-timing/vdp-timing.html), as ticks after the line's
   sync; the waits are openMSX VDPCmdEngine's. *)

(* Screen off (BL clear, or the vertical border). *)
let slots_screen_off =
  [| 0; 8; 16; 24; 32; 40; 48; 56; 64; 72; 80; 88; 96; 104; 112; 120;
     164; 172; 180; 188; 196; 204; 212; 220; 228; 236; 244; 252; 260; 268;
     276; 292; 300; 308; 316; 324; 332; 340; 348; 356; 364; 372; 380; 388;
     396; 404; 420; 428; 436; 444; 452; 460; 468; 476; 484; 492; 500; 508;
     516; 524; 532; 548; 556; 564; 572; 580; 588; 596; 604; 612; 620; 628;
     636; 644; 652; 660; 676; 684; 692; 700; 708; 716; 724; 732; 740; 748;
     756; 764; 772; 780; 788; 804; 812; 820; 828; 836; 844; 852; 860; 868;
     876; 884; 892; 900; 908; 916; 932; 940; 948; 956; 964; 972; 980; 988;
     996; 1004; 1012; 1020; 1028; 1036; 1044; 1060; 1068; 1076; 1084; 1092;
     1100; 1108; 1116; 1124; 1132; 1140; 1148; 1156; 1164; 1172; 1188; 1196;
     1204; 1212; 1220; 1228; 1268; 1276; 1284; 1292; 1300; 1308; 1316; 1324;
     1334; 1344; 1352; 1360 |]

(* Display on, sprites off (R#8 SPD set). *)
let slots_sprites_off =
  [| 6; 14; 22; 30; 38; 46; 54; 62; 70; 78; 86; 94; 102; 110; 118; 162;
     170; 182; 188; 214; 220; 246; 252; 278; 310; 316; 342; 348; 374; 380;
     406; 438; 444; 470; 476; 502; 508; 534; 566; 572; 598; 604; 630; 636;
     662; 694; 700; 726; 732; 758; 764; 790; 822; 828; 854; 860; 886; 892;
     918; 950; 956; 982; 988; 1014; 1020; 1046; 1078; 1084; 1110; 1116;
     1142; 1148; 1174; 1206; 1212; 1266; 1274; 1282; 1290; 1298; 1306; 1314;
     1322; 1332; 1342; 1350; 1358; 1366 |]

(* Display on, sprites on: the sprite fetch takes most of the line. *)
let slots_sprites_on =
  [| 28; 92; 162; 170; 188; 220; 252; 316; 348; 380; 444; 476; 508; 572;
     604; 636; 700; 732; 764; 828; 860; 892; 956; 988; 1020; 1084; 1116;
     1148; 1212; 1264; 1330 |]

(* openMSX isDisplayArea runs from the left border of display line 0
   (tick 202, cycle 48 here, where VR falls) to the same point below the
   last display line. *)
let display_edge_cycle = vr_fall_cycle

let display_area t =
  let visible = visible_lines t in
  if t.cycle_in_line < display_edge_cycle then t.line > 0 && t.line <= visible
  else t.line < visible

(* openMSX VDPAccessSlots::getTab for the bitmap modes. *)
let slot_table t =
  if t.regs.(1) land 0x40 = 0 || not (display_area t) then slots_screen_off
  else if t.regs.(8) land 0x02 <> 0 then slots_sprites_off
  else slots_sprites_on

(* The first slot at least [delta] ticks after [at] (ticks into this
   model's line; the answer may lie in a later line). *)
let slot_after slots ~at ~delta =
  let base = (at + right_border_tick) mod ticks_per_line in
  let want = base + delta in
  let n = Array.length slots in
  let value k = slots.(k mod n) + ((k / n) * ticks_per_line) in
  (* Smallest k with value k >= want; want < 2 lines, so k < 3n. *)
  let rec search lo hi =
    if lo >= hi then lo
    else
      let mid = (lo + hi) / 2 in
      if value mid >= want then search lo mid else search (mid + 1) hi
  in
  at + (value (search 0 (3 * n)) - base)

(* Ticks from one engine access to the earliest next one (openMSX
   VDPCmdEngine, the execute functions). A dot command reads the destination byte, then
   writes it [read_to_write] later; a copy reads its source first. The row
   delays include the turn to the next row. *)
let read_to_write = 24
let lmmm_src_to_dst = 32
let hmmv_byte = 48
let hmmv_row = 104
let hmmm_byte = 64
let hmmm_row = 128
let ymmm_byte = 40
let lmmv_dot = 72
let lmmv_row = 136
let lmmm_dot = 64
let lmmm_row = 128
let line_dot = 88
let line_minor_step = 120
let srch_dot = 88

(* A CPU VRAM access waits 16 ticks for its slot (openMSX
   scheduleCpuVramAccess, V99x8), and an engine access due in that slot
   takes the next one. *)
let cpu_access_delay = 16
let steal_delay = 1

type step = Next of int | Done

let dir_x t = if t.regs.(45) land arg_dix <> 0 then -1 else 1
let dir_y t = if t.regs.(45) land arg_diy <> 0 then -1 else 1
let ext_src t = t.regs.(45) land arg_mxs <> 0
let ext_dst t = t.regs.(45) land arg_mxd <> 0
let byte_block = function Hmmv | Hmmm | Ymmm -> true | Lmmv | Lmmm -> false
let x_step t block =
  dir_x t * (if byte_block block then pixels_per_byte t else 1)

(* openMSX tmpNX and tmpNY, from the live registers: the units in a row and
   the rows left, clipped to the screen for the destination and, in a
   copy, the source. YMMM moves whole rows from DX. *)
let row_units t block =
  let reverse = t.regs.(45) land arg_dix <> 0 in
  let clip x nx = clip_x t ~byte:(byte_block block) ~reverse ~x ~nx in
  let sx = reg16 t 32 33 and dx = reg16 t 36 37 and nx = reg_nx t in
  match block with
  | Lmmv | Hmmv -> clip dx nx
  | Lmmm | Hmmm -> min (clip sx nx) (clip dx nx)
  | Ymmm -> clip dx 512

let rows_left t block =
  let reverse = t.regs.(45) land arg_diy <> 0 and ny = reg_ny t in
  let dest = clip_y ~reverse ~y:(reg_y t 38 39) ~ny in
  match block with
  | Lmmv | Hmmv -> dest
  | Lmmm | Hmmm | Ymmm -> min dest (clip_y ~reverse ~y:(reg_y t 34 35) ~ny)

(* LMMM and HMMM walk a source X; the others do not touch ASX. *)
let walks_source = function Lmmm | Hmmm -> true | Lmmv | Hmmv | Ymmm -> false

(* After a unit is written: step along the row, or at its end go to the
   next row (DY, and SY for a copy, move one line and NY counts down;
   openMSX "SY += TY; DY += TY; --NY") and end after the last one. *)
let next_unit t block ~unit_delay ~row_delay =
  let tx = x_step t block in
  t.cmd_adx <- t.cmd_adx + tx;
  if walks_source block then t.cmd_asx <- t.cmd_asx + tx;
  t.cmd_anx <- t.cmd_anx - 1;
  if t.cmd_anx > 0 then Next unit_delay
  else begin
    let last = rows_left t block = 1 in
    let regs = match block with Lmmv | Hmmv -> Dst | Lmmm | Hmmm | Ymmm -> Src_and_dst in
    finish_rows t regs ~rows:1 ~step:(dir_y t);
    t.cmd_adx <- reg16 t 36 37;
    if walks_source block then t.cmd_asx <- reg16 t 32 33;
    t.cmd_anx <- row_units t block;
    if last then Done else Next row_delay
  end

let block_step t block =
  let op = t.regs.(46) land 0x0f and col = t.regs.(44) in
  let sy = reg_y t 34 35 and dy = reg_y t 38 39 in
  let dst () = vram_addr t (ext_dst t) t.cmd_adx dy in
  match block, t.cmd_phase with
  | Hmmv, _ ->
    write_vram t (dst ()) col;
    next_unit t block ~unit_delay:hmmv_byte ~row_delay:hmmv_row
  | Hmmm, 0 ->
    t.cmd_src <- read_vram t (vram_addr t (ext_src t) t.cmd_asx sy);
    t.cmd_phase <- 1;
    Next read_to_write
  | Hmmm, _ ->
    write_vram t (dst ()) t.cmd_src;
    t.cmd_phase <- 0;
    next_unit t block ~unit_delay:hmmm_byte ~row_delay:hmmm_row
  | Ymmm, 0 ->
    (* YMMM reads and writes with MXD (openMSX executeYmmm). *)
    t.cmd_src <- read_vram t (vram_addr t (ext_dst t) t.cmd_adx sy);
    t.cmd_phase <- 1;
    Next read_to_write
  | Ymmm, _ ->
    write_vram t (dst ()) t.cmd_src;
    t.cmd_phase <- 0;
    next_unit t block ~unit_delay:ymmm_byte ~row_delay:ymmm_byte
  | Lmmv, 0 ->
    t.cmd_dst <- read_vram t (dst ());
    t.cmd_phase <- 1;
    Next read_to_write
  | Lmmv, _ ->
    pset_byte t (dst ()) ~old:t.cmd_dst t.cmd_adx op col;
    t.cmd_phase <- 0;
    next_unit t block ~unit_delay:lmmv_dot ~row_delay:lmmv_row
  | Lmmm, 0 ->
    t.cmd_src <- cmd_point t (ext_src t) t.cmd_asx sy;
    t.cmd_phase <- 1;
    Next lmmm_src_to_dst
  | Lmmm, 1 ->
    t.cmd_dst <- read_vram t (dst ());
    t.cmd_phase <- 2;
    Next read_to_write
  | Lmmm, _ ->
    pset_byte t (dst ()) ~old:t.cmd_dst t.cmd_adx op t.cmd_src;
    t.cmd_phase <- 0;
    next_unit t block ~unit_delay:lmmm_dot ~row_delay:lmmm_row

(* SRCH walks ASX from SX along SY until a dot matches CL (EQ=0) or
   differs from it (EQ=1), and sets BD. Leaving the line ends the walk
   without touching BD (openMSX executeSrch). *)
let srch_step t =
  let cl = t.regs.(44) land pixel_mask t and ppl = (bitmap_layout t).width in
  let eq = t.regs.(45) land arg_eq <> 0 in
  if (cmd_point t (ext_src t) t.cmd_asx (reg_y t 34 35) = cl) <> eq then begin
    t.cmd_bd <- true;
    Done
  end
  else begin
    t.cmd_asx <- t.cmd_asx + dir_x t;
    if t.cmd_asx land ppl <> 0 then Done else Next srch_dot
  end

(* LINE draws NX + 1 dots from (DX, DY): NX along the major axis (ARG MAJ),
   NY along the minor one, with ASX as the Bresenham sum. Each dot reads
   its byte, then writes it; a minor step costs 32 ticks more. The order
   of the end tests follows openMSX executeLine ("confirmed on real HW"):
   leaving the line ends after the dot is drawn, and leaving the top edge
   ends the command while the bottom edge wraps. DY moves as it goes and
   is left at -1 (0x3FF) when the line left the top edge. *)
let line_step t =
  let dy = reg_y t 38 39 in
  let a = vram_addr t (ext_dst t) t.cmd_adx dy in
  match t.cmd_phase with
  | 0 ->
    t.cmd_dst <- read_vram t a;
    t.cmd_phase <- 1;
    Next read_to_write
  | _ ->
    pset_byte t a ~old:t.cmd_dst t.cmd_adx (t.regs.(46) land 0x0f) t.regs.(44);
    t.cmd_phase <- 0;
    let tx = dir_x t and ty = dir_y t in
    let nx = reg_nx t and ny = reg_ny t and ppl = (bitmap_layout t).width in
    let reduce () = t.cmd_asx <- (t.cmd_asx - ny) land 0x3ff in
    let count () =
      let anx = t.cmd_anx in
      t.cmd_anx <- anx + 1;
      anx = nx || t.cmd_adx land ppl <> 0
    in
    let step_y () =
      let dy = dy + ty in
      set_reg_y t 38 39 dy;
      ty < 0 && dy < 0
    in
    if t.regs.(45) land arg_maj = 0 then begin
      t.cmd_adx <- t.cmd_adx + tx;
      if count () then Done
      else if t.cmd_asx < ny then begin
        t.cmd_asx <- t.cmd_asx + nx;
        if step_y () then Done else (reduce (); Next line_minor_step)
      end
      else (reduce (); Next line_dot)
    end
    else if step_y () then Done
    else begin
      let delay =
        if t.cmd_asx < ny then begin
          t.cmd_asx <- t.cmd_asx + nx;
          t.cmd_adx <- t.cmd_adx + tx;
          line_minor_step
        end
        else line_dot
      in
      reduce ();
      if count () then Done else Next delay
    end

(* PSET reads the byte, then writes the dot. *)
let pset_step t =
  let a = vram_addr t (ext_dst t) (reg16 t 36 37) (reg_y t 38 39) in
  match t.cmd_phase with
  | 0 ->
    t.cmd_dst <- read_vram t a;
    t.cmd_phase <- 1;
    Next read_to_write
  | _ ->
    pset_byte t a ~old:t.cmd_dst (reg16 t 36 37) (t.regs.(46) land 0x0f) t.regs.(44);
    Done

(* One VRAM access of the running command. Only POINT, PSET, SRCH, LINE
   and the block commands run here; STOP ends at once and the transfers
   move with the CPU. *)
let engine_step t =
  match command_of_cmr t.regs.(46) with
  | Point ->
    t.regs.(44) <- cmd_point t (ext_src t) (reg16 t 32 33) (reg_y t 34 35);
    Done
  | Pset -> pset_step t
  | Srch -> srch_step t
  | Line -> line_step t
  | Block block -> block_step t block
  | Stop | Transfer _ -> Done

let engine_running t = t.cmd_ce && Option.is_none t.tx

(* openMSX commandDone: CE drops; TR waits for the next S#2 read. *)
let end_command t =
  t.cmd_ce <- false;
  t.cmd_asx <- t.cmd_asx land 0x3ff;
  t.cmd_steal <- None

(* The clock has moved [ticks] on to [t.cycle_in_line] under one slot
   table: the engine makes every access due before now. *)
let run_engine t slots ~ticks =
  if engine_running t then begin
    let now = t.cycle_in_line * ticks_per_cycle in
    t.cmd_wait <- t.cmd_wait - ticks;
    t.cmd_steal <- Option.map (fun s -> s - ticks) t.cmd_steal;
    while engine_running t && t.cmd_wait < 0 do
      let at = now + t.cmd_wait in
      match engine_step t with
      | Done -> end_command t
      | Next delta ->
        let next = slot_after slots ~at ~delta - now in
        t.cmd_wait <-
          (match t.cmd_steal with
           | Some s when s = next ->
             t.cmd_steal <- None;
             slot_after slots ~at:(now + s) ~delta:steal_delay - now
           | Some _ | None -> next)
    done;
    match t.cmd_steal with
    | Some s when s < t.cmd_wait -> t.cmd_steal <- None
    | Some _ | None -> ()
  end

(* A CPU VRAM access now: it claims the first slot 16 ticks on. *)
let steal_slot t =
  if engine_running t then begin
    let slots = slot_table t and now = t.cycle_in_line * ticks_per_cycle in
    let s = slot_after slots ~at:now ~delta:cpu_access_delay - now in
    if s = t.cmd_wait then
      t.cmd_wait <- slot_after slots ~at:(now + s) ~delta:steal_delay - now
    else if s > t.cmd_wait then t.cmd_steal <- Some s
  end

(* A command starts at the first free slot from now. *)
let start_timed t command =
  (match command with
   | Srch -> t.cmd_asx <- reg16 t 32 33
   | Line ->
     (* openMSX computes (NX - 1) >> 1 unsigned, so NX = 0 starts the sum at
        0x7FFFFFFF. Only its low 10 bits can show: with NX = 0 the first
        dot ends the line, the sum never reaches a minor step (NY < 1024)
        and the reduction keeps 10 bits. *)
     t.cmd_asx <- (((reg_nx t - 1) land 0xffff_ffff) lsr 1) land 0x3ff;
     t.cmd_adx <- reg16 t 36 37;
     t.cmd_anx <- 0
   | Block block ->
     t.cmd_adx <- reg16 t 36 37;
     t.cmd_anx <- row_units t block;
     if walks_source block then t.cmd_asx <- reg16 t 32 33
   | Point | Pset | Stop | Transfer _ -> ());
  t.cmd_phase <- 0;
  t.cmd_steal <- None;
  t.cmd_ce <- true;
  let now = t.cycle_in_line * ticks_per_cycle in
  t.cmd_wait <- slot_after (slot_table t) ~at:now ~delta:0 - now

(* LMMC/HMMC 전송 한 바이트 — 전송 모드에서 R#44(CLR) 쓰기가 곧
   데이터다 (openMSX setCmdReg case 0x0C → executeLmmc/executeHmmc).
   HMMC 는 바이트 단위, LMMC 는 픽셀 단위 pset. *)
let tx_count = ref 0

(* The walking corner reached the end of a row: go to the next row, and
   end the command after the last one. *)
let next_tx_row t regs ~diy =
  let step = if diy then 1 else -1 in
  t.tx_dy <- t.tx_dy + step;
  finish_rows t regs ~rows:1 ~step;
  t.tx_adx <- t.tx_dx;
  t.tx_anx <- t.tx_nx;
  t.tx_ny <- t.tx_ny - 1;
  if t.tx_ny = 0 then begin
    t.tx <- None;
    t.cmd_ce <- false
    (* TR 은 다음 S#2 읽기에서 해제된다 — openMSX commandDone. *)
  end

let transfer_byte t ~hm v =
  incr tx_count;
  t.cmd_tr <- true; (* 다음 바이트 즉시 준비 *)
  let arg = t.regs.(45) in
  let dix = arg land arg_dix = 0 and diy = arg land arg_diy = 0 in
  let mxd = arg land arg_mxd <> 0 in
  let op = t.regs.(46) land 0x0f in
  let ppb = pixels_per_byte t in
  if hm then begin
    write_vram t (vram_addr t mxd t.tx_adx t.tx_dy) v;
    t.tx_adx <- t.tx_adx + ((if dix then 1 else -1) * ppb)
  end else begin
    pset t mxd t.tx_adx t.tx_dy op v;
    t.tx_adx <- t.tx_adx + (if dix then 1 else -1)
  end;
  t.tx_anx <- t.tx_anx - 1;
  if t.tx_anx = 0 then next_tx_row t Dst ~diy

(* LMCM: read the dot under the corner into COL (R#44, seen as S#7) and
   step on. The CPU takes it with an S#7 read, which asks for the next one;
   the last read ends the command with TR still set (openMSX executeLmcm,
   resetColor). *)
let fetch_dot t =
  let arg = t.regs.(45) in
  let dix = arg land arg_dix = 0 and diy = arg land arg_diy = 0 in
  t.regs.(44) <- cmd_point t (arg land arg_mxs <> 0) t.tx_adx t.tx_dy;
  t.tx_adx <- t.tx_adx + (if dix then 1 else -1);
  t.tx_anx <- t.tx_anx - 1;
  if t.tx_anx = 0 then next_tx_row t Src ~diy;
  t.cmd_asx <- t.tx_adx land 0x3ff

(* One transfer step: COL (R#44) goes out as a dot or byte for LMMC/HMMC,
   or LMCM fetches its next dot into COL. *)
let transfer_step t kind =
  match kind with
  | Lmmc -> transfer_byte t ~hm:false t.regs.(44)
  | Hmmc -> transfer_byte t ~hm:true t.regs.(44)
  | Lmcm -> fetch_dot t

(* Set up the walking corner for LMMC/HMMC (from DX, DY) or LMCM (from SX,
   SY). openMSX clipNX_1_byte / clipNX_1_pixel: 한 줄 폭을 넘는 NX 는
   잘린다 — HMMC 는 바이트 단위, LMMC·LMCM 은 픽셀 단위. *)
let start_transfer t kind ~x ~y =
  let arg = t.regs.(45) in
  t.tx <- Some kind;
  t.tx_dx <- x;
  t.tx_adx <- x;
  t.tx_nx <- clip_x t ~byte:(kind = Hmmc) ~reverse:(arg land arg_dix <> 0) ~x
      ~nx:(reg_nx t);
  t.tx_anx <- t.tx_nx;
  t.tx_dy <- y;
  t.tx_ny <- clip_y ~reverse:(arg land arg_diy <> 0) ~y ~ny:(reg_ny t);
  t.cmd_ce <- true;
  t.cmd_tr <- true

(* An R#44 write and an S#7 read both ask the engine for a transfer
   (openMSX setCmdReg 0x0C and resetColor: "if (!CMD) status &= ~TR;
   transfer = true"). A running transfer takes it at once; with none, the
   request waits and becomes the first dot of the next LMMC/HMMC. *)
let request_transfer t =
  match t.tx with
  | Some kind -> transfer_step t kind
  | None ->
    t.cmd_tr <- false;
    t.tx_pending <- true

let start_command t =
  let cmr_v = t.regs.(46) in
  t.cmd_log <- (cmr_v, reg16 t 36 37, reg16 t 38 39, reg16 t 42 43) :: List.filteri (fun i _ -> i < 31) t.cmd_log;
  (* A new CMR ends whatever ran before; only a transfer raises TR again. *)
  t.tx <- None;
  t.cmd_ce <- false;
  t.cmd_tr <- false;
  t.cmd_steal <- None;
  if commands_enabled t then
    match command_of_cmr cmr_v with
    | Stop -> ()
    | (Point | Pset | Srch | Line | Block _) as command -> start_timed t command
    | Transfer ((Lmmc | Hmmc) as kind) ->
      start_transfer t kind ~x:(reg16 t 36 37) ~y:(reg_y t 38 39);
      (* 발행 전 R#44 에 쓰인 첫 픽셀이 큐에 있으면 즉시 소비. *)
      if t.tx_pending then begin
        t.tx_pending <- false;
        transfer_step t kind
      end
    | Transfer Lmcm ->
      (* startLmcm raises its own request for the first dot; an earlier
         R#44 write is not one. *)
      t.tx_pending <- false;
      start_transfer t Lmcm ~x:(reg16 t 32 33) ~y:(reg_y t 34 35);
      fetch_dot t

(* Writing R#13 restarts blinking even when the value is unchanged (openMSX
   VDP::changeRegister): the ON period starts now unless it is 0 frames,
   and the colours alternate only when both periods are non-zero. *)
let restart_blink t v =
  if t.blink_on = (v land 0xf0 = 0) then t.blink_on <- not t.blink_on;
  t.blink_count <-
    (if v land 0xf0 <> 0 && v land 0x0f <> 0 then (v lsr 4) * 10 else 0)

(* One frame of R#13 blinking, at frame start (openMSX VDP::frameStart):
   each nibble counts tens of frames, high for ON and low for OFF. *)
let step_blink t =
  if t.blink_count <> 0 then begin
    t.blink_count <- t.blink_count - 1;
    if t.blink_count = 0 then begin
      t.blink_on <- not t.blink_on;
      let r13 = t.regs.(13) in
      t.blink_count <- (if t.blink_on then r13 lsr 4 else r13 land 0x0f) * 10
    end
  end

(* Bits whose change waits for the next line start (openMSX
   VDP::changeRegister calls syncAtNextLine for them): the display mode
   (R#0 M3-M5, R#1 M1-M2), blank (R#1 BL) and sprite disable (R#8 SPD).
   The new mode, and so the screen, the sprite engine, the command engine's
   addressing and the CPU's planar addressing, all follow at that point. *)
let deferred_bits r =
  match r with
  | 0 -> 0x0e
  | 1 -> 0x58
  | 8 -> 0x02
  | _ -> 0

let set_reg t r v =
  let v = v land 0xff in
  let deferred = deferred_bits r in
  t.regs.(r) <- (t.regs.(r) land deferred) lor (v land lnot deferred);
  if r = 0 then t.written_r0 <- v
  else if r = 1 then t.written_r1 <- v
  else if r = 8 then t.written_r8 <- v;
  if r = 44 then request_transfer t
  else if r = 13 then restart_blink t (v land 0xff)
  else if r = 46 then start_command t
  else if r = 18 then move_line_count_reset t
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
  steal_slot t;
  t.read_buf <- read_vram t (cpu_addr t);
  increment_addr t

let io_write t ~port v =
  log_w t (port land 0xff) t.addr v;
  match port with
  | 0x98 ->
    steal_slot t;
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
        (* F, 5S and C clear on read; the 5th/9th sprite number stays
           (openMSX SpriteChecker::resetStatus keeps bits 0-4). *)
        let x = t.status0 in
        t.status0 <- t.status0 land 0x1f;
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
        let transferring = Option.is_some t.tx in
        let bit b mask = if b then mask else 0 in
        let v =
          bit (t.cmd_tr || transferring) 0x80
          lor bit (vertical_retrace t) 0x40
          lor bit (horizontal_retrace t) 0x20
          lor bit t.cmd_bd 0x10
          lor 0x0c (* bits 3-2 read 1 (openMSX statusReg2 = 0x0C at reset) *)
          lor bit t.even_odd 0x02
          lor bit t.cmd_ce 0x01
        in
        if not transferring then t.cmd_tr <- false;
        v
      | 3 -> t.collision_x land 0xff
      | 4 -> (t.collision_x lsr 8) lor 0xfe
      | 5 ->
        (* Reading S#5 clears both coordinates (openMSX resetCollision). *)
        let v = t.collision_y land 0xff in
        t.collision_x <- 0;
        t.collision_y <- 0;
        v
      | 6 -> (t.collision_y lsr 8) lor 0xfc
      | 7 ->
        (* COL: the POINT/LMCM dot. Reading it requests the next transfer
           step: LMCM fetches its next dot (openMSX resetColor). *)
        let v = t.regs.(44) in
        request_transfer t;
        v
      | 8 -> t.cmd_asx land 0xff
      | 9 ->
        let v = ((t.cmd_asx lsr 8) land 1) lor 0xfe in
        t.cmd_bd <- false;
        v
      | _ -> 0xff
    in
    t.latch_first <- true;
    s
  | _ -> 0xff

(* Sprites: the display mode picks the sprite mode, R#8 SPD (bit1) and a
   blanked screen (R#1 BL clear) switch the engine off (openMSX
   VDP::spritesEnabled). *)
let sprite_mode t =
  match display_mode t with
  | Graphic1 | Graphic2 | Multicolor -> Some Vdp_sprite.Mode1
  | Graphic3 | Graphic4 | Graphic5 | Graphic6 | Graphic7 -> Some Vdp_sprite.Mode2
  | Text1 | Text2 | Undefined _ -> None

let sprites_on t ~display_line =
  match sprite_mode t with
  | Some mode when t.regs.(8) land 0x02 = 0 && t.regs.(1) land 0x40 <> 0 ->
    let read a = read_vram t (physical_addr t a) in
    Some (mode, Vdp_sprite.evaluate mode ~regs:t.regs ~read ~display_line)
  | Some _ | None -> None

(* Per display line: S#0 5S/9S with the overflowing sprite number, or else
   the number of sprites scanned; then C and S#3-S#6. The 5S number is only
   taken while F and 5S are both clear (openMSX checkSprites1/2, after
   TMS9918.pdf), and C stays until S#0 is read. *)
let check_sprites t ~display_line =
  match sprites_on t ~display_line with
  | None -> ()
  | Some (mode, line) ->
    let s = t.status0 in
    let s =
      match line.overflow with
      | Some n when s land 0xc0 = 0 -> 0x40 lor (s land 0x20) lor n
      | Some _ | None -> s
    in
    let s = if s land 0x40 = 0 then (s land 0x20) lor min line.scanned 31 else s in
    t.status0 <- (t.status0 land 0x80) lor (s land 0x7f);
    if t.status0 land 0x20 = 0 then
      match Vdp_sprite.collision mode ~regs:t.regs line with
      | None -> ()
      | Some x ->
        t.status0 <- t.status0 lor 0x20;
        t.collision_x <- x + 12;
        (* The checked line is one above the shown one. *)
        t.collision_y <- display_line - 1 + 8

let blanked t = t.regs.(1) land 0x40 = 0

(* 모드 판정 (V9938 모드표: M5=R0bit3, M4=R0bit2, M3=R0bit1,
   M2=R1bit3, M1=R1bit4). G4 비트맵 = MSX2 SCREEN5 (M4+M3),
   베이스는 R#2 의 A16/A15 (bit6/5) × 32K — 하위 5비트는 11111 이어야
   모든 줄이 보인다 ({!bitmap_vram_line}). *)
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

(* GRAPHIC 7 sprites take a fixed palette, not the palette registers
   (openMSX Renderer::GRAPHIC7_SPRITE_PALETTE, 0xGRB like [palette]). *)
let graphic7_sprite_palette =
  [| 0x000; 0x002; 0x030; 0x032; 0x300; 0x302; 0x330; 0x332;
     0x472; 0x007; 0x070; 0x077; 0x700; 0x707; 0x770; 0x777 |]

(* Raster. Each display line is drawn into [raster] as the beam leaves it,
   with the registers, palette and VRAM of that moment, so a split made by a
   line interrupt shows from the next line on. Entering VBlank turns the
   scanned lines into [shown], the frame clients read. *)
let packed (r, g, b) = (r lsl 16) lor (g lsl 8) lor b

(* SCREEN8 byte = GGGRRRBB, one byte per native pixel. *)
let graphic7_rgb =
  Array.init 256 (fun c ->
    packed (((c lsr 2) land 7) * 255 / 7, ((c lsr 5) land 7) * 255 / 7,
            (c land 3) * 255 / 3))

let graphic7_sprite_rgb = Array.map (fun v -> packed (grb_to_rgb v)) graphic7_sprite_palette

let line_width t =
  match display_mode t with
  | Text2 | Graphic5 | Graphic6 -> max_width
  | Text1 | Multicolor | Graphic1 | Graphic2 | Graphic3 | Graphic4 | Graphic7
  | Undefined _ -> 256

(* The frame a VBlank publishes: native width and R#9 LN height. Character
   modes show rows 24-26 too when LN selects 212 lines (openMSX renders
   every display line through CharacterConverter). *)
let scan_dims t = (line_width t, visible_lines t)

(* [x] < max_width and [y] < max_lines at every call, so the three bytes
   lie inside the raster; the renderer writes ~100K pixels a frame. *)
let put_rgb raster base x v =
  let o = base + (x * 3) in
  Bytes.unsafe_set raster o (Char.unsafe_chr ((v lsr 16) land 0xff));
  Bytes.unsafe_set raster (o + 1) (Char.unsafe_chr ((v lsr 8) land 0xff));
  Bytes.unsafe_set raster (o + 2) (Char.unsafe_chr (v land 0xff))

(* Background colour 0 shows the backdrop, R#7 bits 3-0, unless R#8 TP is
   set (openMSX SDLRasterizer::precalcColorIndex0). TEXT1, TEXT2 and
   MULTICOLOR ignore TP (measured on a V9958, openMSX #2218, which lists the
   TEXT1Q and MULTIQ codes 0x05 and 0x06 with them); GRAPHIC7 never makes
   colour 0 transparent. *)
let colour0_transparent t =
  match display_mode t with
  | Text1 | Text2 | Multicolor | Undefined (0x05 | 0x06) -> true
  | Graphic7 -> false
  | Graphic1 | Graphic2 | Graphic3 | Graphic4 | Graphic5 | Graphic6 | Undefined _ ->
    t.regs.(8) land 0x20 = 0

(* The border colour at native dot [x], which also fills a blanked line
   (openMSX SDLRasterizer::getBorderColors). GRAPHIC5 splits R#7 into bits
   3-2 for even dots and 1-0 for odd ones; GRAPHIC7 reads all of R#7 as a
   GGGRRRBB colour. [raw] is the palette as RGB. *)
let border_rgb t raw x =
  let r7 = t.regs.(7) in
  match display_mode t with
  | Graphic7 -> graphic7_rgb.(r7)
  | Graphic5 -> raw.(if x land 1 = 0 then (r7 lsr 2) land 3 else r7 land 3)
  | Text1 | Text2 | Multicolor | Graphic1 | Graphic2 | Graphic3 | Graphic4
  | Graphic6 | Undefined _ -> raw.(r7 land 0x0f)

(* Character tables are read as openMSX VRAMWindow::readNP reads them: the
   address is [base land index], where [base] is the register value with
   its unused low bits set and [index] is the offset with every bit from
   [bits] up set. Register bits that overlap the offset act as a mask, the
   source of the TMS-era table mirrors. *)
let table t ~base ~bits index =
  Char.code (Bytes.get t.vram (base land ((lnot 0 lsl bits) lor index) land 0x1ffff))

let name_base t = ((t.regs.(2) land 0x7f) lsl 10) lor 0x3ff
let pattern_base t = ((t.regs.(4) land 0x3f) lsl 11) lor 0x7ff
let colour_base t =
  ((t.regs.(10) land 0x07) lsl 14) lor (t.regs.(3) lsl 6) lor 0x3f

(* TEXT1 and TEXT2 start 9 dots (18 at 512 wide) right of the other modes'
   display start (openMSX getLeftSprites, V99x8, openMSX issue #708); the
   rest of the line is the R#7 background. The character row ignores R#23,
   which only moves the dot row inside each character (openMSX
   PixelRenderer::draw keeps its own text row counter). *)
let text_left = 9

let draw_text_cell set ~x0 ~fg ~bg pattern =
  for px = 0 to 5 do
    set (x0 + px) (if (pattern lsr (7 - px)) land 1 = 1 then fg else bg)
  done

let draw_text1 t set pal y =
  let r = t.regs in
  let fg = pal.((r.(7) lsr 4) land 0xf) and bg = pal.(r.(7) land 0xf) in
  for x = 0 to 255 do set x bg done;
  let row = y / 8 and dot_row = (y + r.(23)) land 7 in
  for col = 0 to 39 do
    (* openMSX renderText1: name index (n + 0xC00) over 12 bits, so R#2
       bits 1-0 pass through for the first 1K names. *)
    let name = (row * 40) + col in
    let code = table t ~base:(name_base t) ~bits:12 ((name + 0xc00) land 0xfff) in
    let pattern = table t ~base:(pattern_base t) ~bits:11 ((code * 8) + dot_row) in
    draw_text_cell set ~x0:(text_left + (col * 6)) ~fg ~bg pattern
  done

(* TEXT2: 80 columns of 6 dots on the 512-dot grid. Each colour table bit
   marks one character (bit 7 = the leftmost of eight); a marked character
   takes the R#12 colours while blinking is in its ON period. The blink
   colours come from the palette itself, and a blink foreground of 0 shows
   the blink background (openMSX CharacterConverter::renderText2). *)
let draw_text2 t set pal raw y =
  let r = t.regs in
  let plain_fg = pal.((r.(7) lsr 4) land 0xf) and plain_bg = pal.(r.(7) land 0xf) in
  let blink_fg, blink_bg =
    if t.blink_on then begin
      let f = (r.(12) lsr 4) land 0xf and b = r.(12) land 0xf in
      (raw.(if f <> 0 then f else b), raw.(b))
    end
    else (plain_fg, plain_bg)
  in
  for x = 0 to max_width - 1 do set x plain_bg done;
  let row = y / 8 and dot_row = (y + r.(23)) land 7 in
  for col = 0 to 79 do
    let marks = table t ~base:(colour_base t) ~bits:9 ((row * 10) + (col / 8)) in
    let marked = (marks lsr (7 - (col land 7))) land 1 = 1 in
    let code = table t ~base:(name_base t) ~bits:12 ((row * 80) + col) in
    let pattern = table t ~base:(pattern_base t) ~bits:11 ((code * 8) + dot_row) in
    let fg, bg = if marked then (blink_fg, blink_bg) else (plain_fg, plain_bg) in
    draw_text_cell set ~x0:((2 * text_left) + (col * 6)) ~fg ~bg pattern
  done

(* MULTICOLOR: each name picks 8 pattern bytes; the byte for this line is
   (scrolled line / 4) mod 8, and its nibbles colour a 4×4 block each
   (openMSX CharacterConverter::renderMulti). *)
let draw_multicolor t set pal dy =
  let row = dy / 8 in
  for col = 0 to 31 do
    let code = table t ~base:(name_base t) ~bits:10 ((row * 32) + col) in
    let c = table t ~base:(pattern_base t) ~bits:11 ((code * 8) lor ((dy / 4) land 7)) in
    let left = pal.(c lsr 4) and right = pal.(c land 0xf) in
    for px = 0 to 3 do
      set ((col * 8) + px) left;
      set ((col * 8) + 4 + px) right
    done
  done

(* GRAPHIC 1-3 tiles at scrolled line [dy]. GRAPHIC 3 (SCREEN4) is GRAPHIC
   2 tiles under sprite mode 2. G2 indexes patterns and colours over 13
   bits (third << 11 | name << 3 | dot row), so R#3 bits 6-0 and R#4 bits
   1-0 mask the high index bits; it reads one colour byte per dot row. G1
   indexes patterns over 11 bits and colours over 6 (name >> 3). *)
let draw_tiles t set pal ~g2 dy =
  let row = dy / 8 and dot_row = dy land 7 in
  let third = row lsr 3 in
  for col = 0 to 31 do
    let name = table t ~base:(name_base t) ~bits:10 ((row * 32) + col) in
    let pattern, colour =
      if g2 then begin
        let index = (third lsl 11) lor (name lsl 3) lor dot_row in
        ( table t ~base:(pattern_base t) ~bits:13 index,
          table t ~base:(colour_base t) ~bits:13 index )
      end
      else
        ( table t ~base:(pattern_base t) ~bits:11 ((name lsl 3) lor dot_row),
          table t ~base:(colour_base t) ~bits:6 (name lsr 3) )
    in
    let fg = pal.((colour lsr 4) land 0xf) and bg = pal.(colour land 0xf) in
    for px = 0 to 7 do
      set ((col * 8) + px) (if (pattern lsr (7 - px)) land 1 = 1 then fg else bg)
    done
  done

(* The VRAM line a bitmap display line shows (openMSX
   SDLRasterizer::drawDisplay). R#2 times 8 masks the scrolled line, so
   bits 6-5 pick the page and bits 4-0 must be set to show every line.
   Planar modes have two pages and drop bit 6. Page bit 0 survives only
   while the odd page shows: always, unless R#9 bit 2 alternates pages and
   this is an even field, or R#13 blinking is in its ON period (openMSX
   VDP::getEvenOddMask). *)
let bitmap_vram_line t ~planar dy =
  let r = t.regs in
  let odd_page = (r.(9) land 0x04 = 0 || t.even_odd) && not t.blink_on in
  let page_mask = (if planar then 0 else 0x200) lor (if odd_page then 0x100 else 0) in
  (((r.(2) land 0x7f) lsl 3) lor 7) land (page_mask lor dy)

let scan_line t y =
  assert (y >= 0 && y < max_lines);
  let width = line_width t in
  Bytes.set t.raster_wide y (if width = max_width then '\001' else '\000');
  let base = y * stride in
  let raster = t.raster in
  let set x v = if x >= 0 && x < width then put_rgb raster base x v in
  let character_mode = not (mode_s5 t || mode_bitmap_hi t) in
  let raw = Array.map (fun v -> packed (grb_to_rgb v)) t.palette in
  if blanked t then for x = 0 to width - 1 do set x (border_rgb t raw x) done
  else begin
    (* [tp]: the palette index colour 0 shows. GRAPHIC5 takes bits 3-2 of
       it for even dots and 1-0 for odd ones. *)
    let tp = if colour0_transparent t then t.regs.(7) land 0x0f else 0 in
    let pal = Array.copy raw in
    pal.(0) <- raw.(tp);
    let dy = (y + t.regs.(23)) land 0xff in
    if not character_mode then begin
      let layout = bitmap_layout t in
      let bytes_per_line = layout.width / layout.pixels_per_byte in
      let row = bitmap_vram_line t ~planar:layout.planar dy * bytes_per_line in
      let byte_at i =
        let a = (row + i) land 0x1ffff in
        Char.code (Bytes.get t.vram (if layout.planar then ((a land 1) lsl 16) lor (a lsr 1) else a))
      in
      match display_mode t with
      | Graphic7 -> for x = 0 to 255 do set x graphic7_rgb.(byte_at x) done
      | Graphic5 ->
        let even0 = raw.(tp lsr 2) and odd0 = raw.(tp land 3) in
        for i = 0 to bytes_per_line - 1 do
          let v = byte_at i in
          for k = 0 to 3 do
            let c = (v lsr ((3 - k) * 2)) land 3 and x = (i * 4) + k in
            set x (if c <> 0 then raw.(c) else if x land 1 = 0 then even0 else odd0)
          done
        done
      | Text1 | Text2 | Multicolor | Graphic1 | Graphic2 | Graphic3 | Graphic4
      | Graphic6 | Undefined _ ->
        let ppb = layout.pixels_per_byte in
        let bits = 8 / ppb in
        let mask = (1 lsl bits) - 1 in
        for i = 0 to bytes_per_line - 1 do
          let v = byte_at i in
          for k = 0 to ppb - 1 do
            set ((i * ppb) + k) pal.((v lsr ((ppb - 1 - k) * bits)) land mask)
          done
        done
    end
    else begin
      match display_mode t with
      | Text1 -> draw_text1 t set pal y
      | Text2 -> draw_text2 t set pal raw y
      | Multicolor -> draw_multicolor t set pal dy
      | Graphic1 -> draw_tiles t set pal ~g2:false dy
      | Graphic2 | Graphic3 -> draw_tiles t set pal ~g2:true dy
      | Graphic4 | Graphic5 | Graphic6 | Graphic7 -> ()
      | Undefined _ ->
        if mode_text t then draw_text1 t set pal y
        else draw_tiles t set pal ~g2:(mode_g2 t) dy
    end;
    (* Sprite dots sit on the 256-dot grid. In the 512-wide GRAPHIC 5 a dot
       covers two native pixels and its colour splits into two 2-bit palette
       indices; GRAPHIC 6 repeats one colour across both (openMSX
       SpriteConverter::drawMode2). Sprites read the palette itself: an
       opaque colour-0 sprite (TP set) is palette 0, not the backdrop
       (SpriteConverter takes palBg). *)
    match sprites_on t ~display_line:y with
    | None -> ()
    | Some (mode, line) ->
      let put =
        match display_mode t with
        | Graphic5 -> fun x c -> set (2 * x) raw.(c lsr 2); set ((2 * x) + 1) raw.(c land 3)
        | Graphic6 -> fun x c -> set (2 * x) raw.(c); set ((2 * x) + 1) raw.(c)
        | Graphic7 -> fun x c -> set x graphic7_sprite_rgb.(c)
        | Text1 | Text2 | Multicolor | Graphic1 | Graphic2 | Graphic3 | Graphic4
        | Undefined _ -> fun x c -> set x raw.(c)
      in
      Vdp_sprite.draw mode ~regs:t.regs line put
  end

(* This model shows the display area at its neutral place. R#18
   horizontal adjust moves a scanned line, sprites included (openMSX
   getLeftSprites), inside that window; the dots it uncovers show the
   border. A 512-wide line moves two dots per step. The value in R#18 when
   the line is scanned applies (openMSX takes it from the next line). *)
let adjust_line t y =
  let width = line_width t in
  let shift = horizontal_adjust t * (width / 256) in
  if shift <> 0 && not (blanked t) then begin
    let base = y * stride in
    let raw = Array.map (fun v -> packed (grb_to_rgb v)) t.palette in
    let kept = (width - abs shift) * 3 in
    if shift > 0 then begin
      Bytes.blit t.raster base t.raster (base + (shift * 3)) kept;
      for x = 0 to shift - 1 do put_rgb t.raster base x (border_rgb t raw x) done
    end
    else begin
      Bytes.blit t.raster (base - (shift * 3)) t.raster base kept;
      for x = width + shift to width - 1 do put_rgb t.raster base x (border_rgb t raw x) done
    end
  end

let render_line t y =
  scan_line t y;
  adjust_line t y

(* A line scanned at the other width is resized: a 256-wide line doubles
   each dot into a 512-wide frame, a 512-wide line keeps the left pixel of
   each pair in a 256-wide frame. The latched vertical adjust moves the
   display down ([v_shift] > 0) or up inside the window; the rows it
   uncovers show the border. *)
let present t =
  let w, h = scan_dims t in
  let raw = Array.map (fun v -> packed (grb_to_rgb v)) t.palette in
  for y = 0 to h - 1 do
    let line = y - t.v_shift in
    let src = line * stride and dst = y * w * 3 in
    if line < 0 || line >= h then
      for x = 0 to w - 1 do put_rgb t.shown dst x (border_rgb t raw x) done
    else
    let wide = Bytes.get t.raster_wide line = '\001' in
    if wide = (w = max_width) then Bytes.blit t.raster src t.shown dst (w * 3)
    else if w = max_width then
      for x = 0 to 255 do
        Bytes.blit t.raster (src + (x * 3)) t.shown (dst + (x * 6)) 3;
        Bytes.blit t.raster (src + (x * 3)) t.shown (dst + (x * 6) + 3) 3
      done
    else
      for x = 0 to 255 do
        Bytes.blit t.raster (src + (x * 6)) t.shown (dst + (x * 3)) 3
      done
  done;
  t.shown_w <- w;
  t.shown_h <- h

(* The deferred bits written so far take effect. A mode outside GRAPHIC
   4-7 ends a running command (openMSX updateDisplayMode aborts it). *)
let take_deferred t =
  let take r written =
    let m = deferred_bits r in
    t.regs.(r) <- (t.regs.(r) land lnot m) lor (written land m)
  in
  take 0 t.written_r0;
  take 1 t.written_r1;
  take 8 t.written_r8;
  if t.cmd_ce && not (commands_enabled t) then begin
    t.tx <- None;
    end_command t
  end

(* The vertical event, if any, on the line the beam has just entered. They
   act on line boundaries; openMSX starts the frame 14 cycles and resets
   the counter 39 cycles into these lines. *)
let next_vertical t =
  match t.vertical with
  | Before_frame_start ->
    if t.line = t.frame_start then begin
      t.even_odd <- not t.even_odd;
      t.pal <- t.regs.(9) land 0x02 <> 0;
      step_blink t;
      t.vertical <- Before_reset (t.line + reset_delay t)
    end
  | Before_reset at ->
    if t.line = at then begin
      t.v_shift <- vertical_adjust t;
      t.vertical <- Before_display (t.line + lines_to_display t)
    end
  | Before_display at ->
    if t.line = at then begin
      t.frame_start <- t.frame_start + lines_per_frame t - at;
      t.line <- 0;
      t.vertical <- Before_frame_start
    end

let advance t ~cycles =
  let hit = ref false in
  let remaining = ref cycles in
  while !remaining > 0 do
    let c = t.cycle_in_line in
    let start = line_start_cycle t in
    (* The slot table holds between the line start, the display edge and
       the line end. *)
    let until =
      if c < start then start
      else if c < display_edge_cycle then display_edge_cycle
      else cycles_per_line
    in
    let step = min !remaining (until - c) in
    let slots = slot_table t in
    t.cycle_in_line <- c + step;
    remaining := !remaining - step;
    run_engine t slots ~ticks:(step * ticks_per_cycle);
    if t.cycle_in_line = start then take_deferred t;
    (* VScan (openMSX execVScan): the display ends at the display edge of
       the line below the last display line, compared against R#9 as it is
       then, so a 212-line switch after line 192 ends it again at 212. *)
    if t.cycle_in_line = display_edge_cycle && t.line = visible_lines t then begin
      t.status0 <- t.status0 lor 0x80;
      t.int_pending <- true;
      hit := true
    end;
    if t.cycle_in_line = cycles_per_line then begin
      t.cycle_in_line <- 0;
      let finished = t.line in
      if finished < visible_lines t then render_line t finished;
      (* 라인 인터럽트: 빔이 줄 (R#19 - R#23) mod 256 의 오른쪽 끝을 지날 때
         S#1 bit0 — openMSX scheduleHScan. R#23 으로 스크롤해도 분할 위치는
         화면에 고정된다. 줄 카운터는 표시 줄 0 부터 다음 카운터 리셋
         직전까지 세므로 아래·위 테두리 줄도 맞으면
         걸린다 (scheduleHScan: 리셋 뒤의 일치는 "never"). FH 는 IE1(R#0
         bit4) 이 켜져 있을 때만 래치된다(openMSX execHScan: "if
         (controlRegs[0] & 0x10)"). IE1 없이 세우면 IE1=0 인 게임이 매
         프레임 S#1 읽기에서 1 을 본다. *)
      if t.regs.(0) land 0x10 <> 0
         && counting_display t
         && finished = (t.regs.(19) - t.regs.(23)) land 0xff
      then begin
        t.status1 <- t.status1 lor 1;
        t.int_pending <- true;
        hit := true
      end;
      t.line <- t.line + 1;
      (* The last display line is drawn: publish the frame. The software
         cannot see this point; S#0 F follows 48 cycles later. *)
      if t.line = visible_lines t then present t;
      next_vertical t;
      if t.line < visible_lines t then check_sprites t ~display_line:t.line
    end
  done;
  !hit

let int_active t =
  let ie0 = t.regs.(1) land 0x20 <> 0 and ie1 = t.regs.(0) land 0x10 <> 0 in
  (t.status0 land 0x80 <> 0 && ie0) || (t.status1 land 1 <> 0 && ie1)

let vram t = t.vram
let tx_state t = (Option.is_some t.tx, !tx_count, t.tx_ny, t.tx_anx, t.tx_dy)
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

let vram_read t addr = Char.code (Bytes.get t.vram (addr land 0x1ffff))

let frame_dims t = (t.shown_w, t.shown_h)
let frame_rgb t = Bytes.sub_string t.shown 0 (t.shown_w * t.shown_h * 3)

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
  State_codec.put_bool w (Option.is_some t.tx);
  State_codec.put_bool w (t.tx = Some Hmmc);
  State_codec.put_bool w t.cmd_tr;
  State_codec.put_bool w t.tx_pending;
  (* v4: sprite collision coordinates (S#3-S#6). *)
  State_codec.put_int w t.collision_x;
  State_codec.put_int w t.collision_y;
  (* v5: what the screen shows — the published frame and the lines already
     scanned in this one — so a restored machine shows the same pixels. *)
  State_codec.put_bytes w t.raster;
  State_codec.put_bytes w t.raster_wide;
  State_codec.put_int w t.shown_w;
  State_codec.put_int w t.shown_h;
  State_codec.put_bytes w (Bytes.sub t.shown 0 (t.shown_w * t.shown_h * 3));
  (* v6: LMCM (the v1-v5 pair above cannot name it), BD and ASX. *)
  State_codec.put_bool w (t.tx = Some Lmcm);
  State_codec.put_bool w t.cmd_bd;
  State_codec.put_int w t.cmd_asx;
  (* v7: S#2 EO and the latched 50/60 Hz frame length. *)
  State_codec.put_bool w t.even_odd;
  State_codec.put_bool w t.pal;
  ()

(* Fields added after v9. Msx writes them after the sound chips, so older
   payloads stay prefixes of newer ones: v10 R#13 blinking, v11 the
   latched vertical adjust. *)
let write_appended w t =
  State_codec.put_bool w t.blink_on;
  State_codec.put_int w t.blink_count;
  State_codec.put_int w t.v_shift;
  State_codec.put_int w t.written_r0;
  State_codec.put_int w t.written_r1;
  State_codec.put_int w t.written_r8;
  (* v13: a running command's progress. *)
  List.iter (State_codec.put_int w)
    [ t.cmd_wait; t.cmd_phase; t.cmd_adx; t.cmd_anx; t.cmd_src; t.cmd_dst;
      Option.value t.cmd_steal ~default:(-1) ];
  (* v14: the frame start and the event ahead of the line counter. *)
  State_codec.put_int w t.frame_start;
  (match t.vertical with
   | Before_frame_start -> State_codec.put_int w 0; State_codec.put_int w 0
   | Before_reset at -> State_codec.put_int w 1; State_codec.put_int w at
   | Before_display at -> State_codec.put_int w 2; State_codec.put_int w at)

(* States before v14 kept display line 0 [lines_per_frame] lines apart and
   flipped EO 7 lines late. Their line counts the same way, so the frame
   start and reset follow from the registers; display line 0 stays at the
   wrap those states expected. *)
let derive_vertical t =
  let delay = reset_delay t in
  t.frame_start <- lines_per_frame t - delay - lines_to_display t;
  t.vertical <-
    (if t.line < t.frame_start then Before_frame_start
     else if t.line < t.frame_start + delay then Before_reset (t.frame_start + delay)
     else Before_display (lines_per_frame t))

(* reset_delay is 8 to 23 lines and lines_to_display 17 to 54. *)
let vertical_consistent t =
  match t.vertical with
  | Before_frame_start -> t.line < t.frame_start
  | Before_reset at ->
    t.frame_start <= t.line && t.line < at && at <= t.frame_start + max_reset_delay
  | Before_display at ->
    t.frame_start < t.line && t.line < at
    && at <= t.frame_start + max_reset_delay + max_lines_to_display

(* Older states keep blinking off, take the vertical adjust from R#18,
   which read_state has restored, and have no deferred bits waiting. *)
let read_appended r t =
  if State_codec.version r >= 10 then begin
    t.blink_on <- State_codec.get_bool r;
    t.blink_count <- State_codec.get_int r ~min:0 ~max:150
  end;
  t.v_shift <-
    (if State_codec.version r >= 11 then State_codec.get_int r ~min:(-7) ~max:8
     else vertical_adjust t);
  if State_codec.version r >= 12 then begin
    t.written_r0 <- State_codec.get_int r ~min:0 ~max:255;
    t.written_r1 <- State_codec.get_int r ~min:0 ~max:255;
    t.written_r8 <- State_codec.get_int r ~min:0 ~max:255
  end
  else begin
    t.written_r0 <- t.regs.(0);
    t.written_r1 <- t.regs.(1);
    t.written_r8 <- t.regs.(8)
  end;
  if State_codec.version r >= 13 then begin
    let int ~min ~max = State_codec.get_int r ~min ~max in
    t.cmd_wait <- int ~min:0 ~max:(2 * ticks_per_line);
    t.cmd_phase <- int ~min:0 ~max:2;
    t.cmd_adx <- int ~min:(-2048) ~max:2559;
    t.cmd_anx <- int ~min:0 ~max:1024;
    t.cmd_src <- int ~min:0 ~max:255;
    t.cmd_dst <- int ~min:0 ~max:255;
    let steal = int ~min:(-1) ~max:(2 * ticks_per_line) in
    t.cmd_steal <- (if steal < 0 then None else Some steal)
  end
  (* Before v13 every command but a transfer ended as it started. *)
  else if engine_running t then State_codec.fail "invalid VDP command state";
  if State_codec.version r >= 14 then begin
    t.frame_start <- State_codec.get_int r ~min:0 ~max:max_counter_line;
    let tag = State_codec.get_int r ~min:0 ~max:2 in
    let at = State_codec.get_int r ~min:0 ~max:max_counter_line in
    t.vertical <-
      (match tag with
       | 0 -> Before_frame_start
       | 1 -> Before_reset at
       | 2 -> Before_display at
       | _ -> State_codec.fail "invalid VDP vertical state")
  end
  else if t.line < lines_per_frame t then derive_vertical t
  else State_codec.fail "invalid VDP line";
  if not (vertical_consistent t) then State_codec.fail "invalid VDP vertical state"

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
  t.line <- State_codec.get_int r ~min:(0) ~max:max_counter_line;
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
  let active = State_codec.get_bool r in
  let hm = State_codec.get_bool r in
  t.tx <- (if not active then None else if hm then Some Hmmc else Some Lmmc);
  t.cmd_tr <- State_codec.get_bool r;
  t.tx_pending <- State_codec.get_bool r;
  if State_codec.version r >= 4 then begin
    t.collision_x <- State_codec.get_int r ~min:0 ~max:0x1ff;
    t.collision_y <- State_codec.get_int r ~min:0 ~max:0x3ff
  end;
  if State_codec.version r >= 5 then begin
    let same_size name src dst =
      if Bytes.length src <> Bytes.length dst then State_codec.fail ("invalid " ^ name);
      Bytes.blit src 0 dst 0 (Bytes.length src)
    in
    same_size "VDP raster" (State_codec.get_bytes r) t.raster;
    let wide = State_codec.get_bytes r in
    if Bytes.exists (fun c -> c <> '\000' && c <> '\001') wide then
      State_codec.fail "invalid VDP raster widths";
    same_size "VDP raster widths" wide t.raster_wide;
    let w = State_codec.get_int r ~min:256 ~max:512 in
    let h = State_codec.get_int r ~min:192 ~max:212 in
    if (w <> 256 && w <> 512) || (h <> 192 && h <> 212) then
      State_codec.fail "invalid VDP frame size";
    let shown = State_codec.get_bytes r in
    if Bytes.length shown <> w * h * 3 then State_codec.fail "invalid VDP frame";
    Bytes.blit shown 0 t.shown 0 (Bytes.length shown);
    t.shown_w <- w;
    t.shown_h <- h
  end;
  if State_codec.version r >= 6 then begin
    let lmcm = State_codec.get_bool r in
    (match t.tx, lmcm with
     | (None | Some (Lmmc | Hmmc | Lmcm)), false -> ()
     | Some Lmmc, true -> t.tx <- Some Lmcm
     | (None | Some (Hmmc | Lmcm)), true -> State_codec.fail "invalid VDP LMCM state");
    t.cmd_bd <- State_codec.get_bool r;
    t.cmd_asx <- State_codec.get_int r ~min:0 ~max:0x3ff
  end;
  if State_codec.version r >= 7 then begin
    t.even_odd <- State_codec.get_bool r;
    t.pal <- State_codec.get_bool r
  end
  else begin
    (* States before v7 did not keep EO or the 50 Hz latch: they read as
       EO = 0 at 60 Hz, as they always have, not as a power-on. *)
    t.even_odd <- false;
    t.pal <- false
  end;
  if Option.is_some t.tx && (t.tx_nx = 0 || t.tx_anx = 0 || t.tx_anx > t.tx_nx || t.tx_ny = 0) then
    State_codec.fail "invalid active VDP transfer"
