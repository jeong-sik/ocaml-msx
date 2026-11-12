(* V9938 의 TMS9918 호환 표면. 모드 비트: M3 = R0 bit1, M2 = R1 bit3,
   M1 = R1 bit4 (텍스트). 테이블 베이스:
   TEXT   NT=R2<<10, PT=R4<<8,  색 = R7 (FG<<4|BG)
   G1     NT=R2<<10, PT=R4<<11, CT=R3<<6 (타일/8 바이트)
   G2     NT=R2<<10, PT=R4<<11, CT=R3<<6 — 어드레싱에 0x800/0x40 미러
   팔레트 쓰기: 0x9A 인덱스, 0x9B 두 번 — 첫 (R<<4|B), 둘째 (G<<4).
   VBlank: 라인 192 진입에서 status0 bit7 과 INT. IE = R1 bit5. *)

(* A CPU-synchronised transfer: LMMC/HMMC take one dot or byte per R#44
   write, LMCM hands one dot per S#7 read. *)
type transfer = Lmmc | Hmmc | Lmcm

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
     getBorderX: the hardware returns ASX whatever command ran). *)
  mutable cmd_asx : int;
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

let lines_per_frame = 262
let cycles_per_line = 228
let visible_lines t = if t.regs.(9) land 0x80 = 0 then 192 else 212

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
    collision_x = 0;
    collision_y = 0;
    line = 0;
    cycle_in_line = 0;
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
    wlog = Array.make 65536 (0, 0, 0);
    wlog_i = 0;
    cmd_log = [];
    raster = Bytes.make (stride * max_lines) '\000';
    raster_wide = Bytes.make max_lines '\000';
    shown = Bytes.make (stride * max_lines) '\000';
    shown_w = 256;
    shown_h = 192;
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

(* SRCH walks SX along SY until a dot matches CL (EQ=0) or differs from it
   (EQ=1), and sets BD. Leaving the line ends the walk without touching BD.
   ASX keeps the last X either way (openMSX executeSrch). *)
let search t =
  let arg = t.regs.(45) in
  let cl = t.regs.(44) land pixel_mask t and ppl = (bitmap_layout t).width in
  let tx = if arg land arg_dix <> 0 then -1 else 1 in
  let eq = arg land arg_eq <> 0 and mxs = arg land arg_mxs <> 0 in
  let sy = reg_y t 34 35 in
  let rec walk asx =
    if (cmd_point t mxs asx sy = cl) <> eq then begin
      t.cmd_bd <- true;
      asx
    end else
      let asx = asx + tx in
      if asx land ppl <> 0 then asx else walk asx
  in
  t.cmd_asx <- walk (reg16 t 32 33) land 0x3ff

(* LINE draws NX + 1 dots from (DX, DY): NX along the major axis (ARG MAJ),
   NY along the minor one. ASX is the Bresenham accumulator and keeps its
   last value for S#8/S#9. The order of the end tests follows openMSX
   executeLine ("confirmed on real HW"): leaving the line ends after the
   dot is drawn, and leaving the top edge ends the command while the bottom
   edge wraps. *)
let draw_line t =
  let arg = t.regs.(45) and op = t.regs.(46) land 0x0f in
  let cl = t.regs.(44) and ppl = (bitmap_layout t).width in
  let tx = if arg land arg_dix <> 0 then -1 else 1 in
  let ty = if arg land arg_diy <> 0 then -1 else 1 in
  let mxd = arg land arg_mxd <> 0 and x_major = arg land arg_maj = 0 in
  let nx = reg_nx t and ny = reg_ny t in
  let next asx = (asx - ny) land 0x3ff in
  let rec dot ~asx ~adx ~dy ~anx =
    pset t mxd adx dy op cl;
    if x_major then begin
      let adx = adx + tx in
      if anx = nx || adx land ppl <> 0 then asx
      else if asx < ny then begin
        let asx = asx + nx and dy = dy + ty in
        if ty < 0 && dy < 0 then asx
        else dot ~asx:(next asx) ~adx ~dy ~anx:(anx + 1)
      end
      else dot ~asx:(next asx) ~adx ~dy ~anx:(anx + 1)
    end else begin
      let dy = dy + ty in
      if ty < 0 && dy < 0 then asx
      else
        let asx, adx = if asx < ny then (asx + nx, adx + tx) else (asx, adx) in
        let asx = next asx in
        if anx = nx || adx land ppl <> 0 then asx
        else dot ~asx ~adx ~dy ~anx:(anx + 1)
    end
  in
  (* openMSX computes (NX - 1) >> 1 unsigned: NX = 0 starts at 0x7FFFFFFF. *)
  let asx = ((nx - 1) land 0xffff_ffff) lsr 1 in
  t.cmd_asx <- dot ~asx ~adx:(reg16 t 36 37) ~dy:(reg_y t 38 39) ~anx:0 land 0x3ff

(* 블록 명령 실행 — 즉시 완료 (CE 는 관찰되지 않는다). *)
let block_command t block =
  let op = t.regs.(46) land 15 in
  let arg = t.regs.(45) in
  let reverse_x = arg land arg_dix <> 0 and reverse_y = arg land arg_diy <> 0 in
  let mxs = arg land arg_mxs <> 0 and mxd = arg land arg_mxd <> 0 in
  let sx = reg16 t 32 33 and sy = reg_y t 34 35 in
  let dx = reg16 t 36 37 and dy = reg_y t 38 39 in
  let byte, copy =
    match block with
    | Lmmv -> (false, false)
    | Lmmm -> (false, true)
    | Hmmv -> (true, false)
    | Hmmm | Ymmm -> (true, true)
  in
  let ymmm = block = Ymmm in
  let nx = if ymmm then 512 else reg_nx t in
  let ny = reg_ny t in
  let src_x = if ymmm then dx else sx in
  let nx =
    let dest = clip_x t ~byte ~reverse:reverse_x ~x:dx ~nx in
    if copy then min dest (clip_x t ~byte ~reverse:reverse_x ~x:src_x ~nx)
    else dest in
  let ny =
    let dest = clip_y ~reverse:reverse_y ~y:dy ~ny in
    if copy then min dest (clip_y ~reverse:reverse_y ~y:sy ~ny) else dest in
  let step_x n = (if reverse_x then -n else n) * (if byte then pixels_per_byte t else 1) in
  let step_y n = if reverse_y then -n else n in
  for row = 0 to ny - 1 do
    for col = 0 to nx - 1 do
      let x = dx + step_x col and y = dy + step_y row in
      if byte then begin
        let value = if copy then
            read_vram t (vram_addr t (if ymmm then mxd else mxs)
              (src_x + step_x col) (sy + step_y row))
          else t.regs.(44) in
        write_vram t (vram_addr t mxd x y) value
      end else begin
        let value = if copy then point t mxs (sx + step_x col) (sy + step_y row)
          else t.regs.(44) in
        pset t mxd x y op value
      end
    done
  done;
  (* LMMM and HMMM leave ASX back at SX (openMSX executeLmmm/executeHmmm
     reset it at each row end); the other block commands do not use it. *)
  match block with
  | Lmmm | Hmmm -> t.cmd_asx <- sx
  | Lmmv | Hmmv | Ymmm -> ()

(* LMMC/HMMC 전송 한 바이트 — 전송 모드에서 R#44(CLR) 쓰기가 곧
   데이터다 (openMSX setCmdReg case 0x0C → executeLmmc/executeHmmc).
   HMMC 는 바이트 단위, LMMC 는 픽셀 단위 pset. *)
let tx_count = ref 0

(* The walking corner reached the end of a row: go to the next row, and
   end the command after the last one. *)
let next_tx_row t ~diy =
  t.tx_dy <- t.tx_dy + (if diy then 1 else -1);
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
  if t.tx_anx = 0 then next_tx_row t ~diy

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
  if t.tx_anx = 0 then next_tx_row t ~diy;
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
  if commands_enabled t then
    match command_of_cmr cmr_v with
    | Stop -> ()
    | Point ->
      t.regs.(44) <-
        cmd_point t (t.regs.(45) land arg_mxs <> 0) (reg16 t 32 33) (reg_y t 34 35)
    | Pset ->
      pset t (t.regs.(45) land arg_mxd <> 0) (reg16 t 36 37) (reg_y t 38 39)
        (t.regs.(46) land 0x0f) t.regs.(44)
    | Srch -> search t
    | Line -> draw_line t
    | Block block -> block_command t block
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

let set_reg t r v =
  t.regs.(r) <- v land 0xff;
  if r = 44 then request_transfer t
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
        let v =
          ((if t.cmd_ce then 1 else 0)
           lor (if t.cmd_tr || transferring then 0x80 else 0)
           lor (if t.cmd_bd then 0x10 else 0))
          lor 0x6e
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

let line_width t = match display_mode t with Graphic5 | Graphic6 -> max_width | _ -> 256

(* The frame a VBlank publishes: bitmap modes keep native width and R#9 LN
   height; character modes are 256×192. *)
let scan_dims t =
  match display_mode t with
  | Graphic4 | Graphic5 | Graphic6 | Graphic7 ->
      ((bitmap_layout t).width, visible_lines t)
  | _ -> (256, 192)

(* [x] < max_width and [y] < max_lines at every call, so the three bytes
   lie inside the raster; the renderer writes ~100K pixels a frame. *)
let put_rgb raster base x v =
  let o = base + (x * 3) in
  Bytes.unsafe_set raster o (Char.unsafe_chr ((v lsr 16) land 0xff));
  Bytes.unsafe_set raster (o + 1) (Char.unsafe_chr ((v lsr 8) land 0xff));
  Bytes.unsafe_set raster (o + 2) (Char.unsafe_chr (v land 0xff))

let render_line t y =
  assert (y >= 0 && y < max_lines);
  let width = line_width t in
  Bytes.set t.raster_wide y (if width = max_width then '\001' else '\000');
  let base = y * stride in
  let raster = t.raster in
  let set x v = if x >= 0 && x < width then put_rgb raster base x v in
  let character_mode = not (mode_s5 t || mode_bitmap_hi t) in
  if blanked t || (character_mode && y >= 192) then
    Bytes.fill t.raster base (width * 3) '\000'
  else begin
    let pal = Array.map (fun v -> packed (grb_to_rgb v)) t.palette in
    let vr addr = Char.code (Bytes.get t.vram (addr land 0x1ffff)) in
    let r = t.regs in
    if not character_mode then begin
      let layout = bitmap_layout t in
      let page = if layout.planar then ((r.(2) lsr 6) land 1) * 256
        else ((r.(2) lsr 5) land 3) * 256 in
      let bytes_per_line = layout.width / layout.pixels_per_byte in
      let row = (page + ((y + r.(23)) land 255)) * bytes_per_line in
      let byte_at i =
        let a = (row + i) land 0x1ffff in
        vr (if layout.planar then ((a land 1) lsl 16) lor (a lsr 1) else a)
      in
      match display_mode t with
      | Graphic7 -> for x = 0 to 255 do set x graphic7_rgb.(byte_at x) done
      | Text1 | Text2 | Multicolor | Graphic1 | Graphic2 | Graphic3 | Graphic4
      | Graphic5 | Graphic6 | Undefined _ ->
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
    else if mode_text t then begin
      (* TEXT1: 40×24, 6×8 셀 — 좌우 8px 는 R7 배경색. *)
      let nt = (r.(2) land 0x7f) lsl 10 in
      let pt = (r.(4) land 0x7) lsl 8 in
      let fg = pal.((r.(7) lsr 4) land 0xf) and bg = pal.(r.(7) land 0xf) in
      for x = 0 to 7 do set x bg; set (255 - x) bg done;
      let row = y / 8 and py = y land 7 in
      for col = 0 to 39 do
        let pat = vr (pt + (vr (nt + (row * 40) + col) * 8) + py) in
        for px = 0 to 5 do
          set (8 + (col * 6) + px) (if (pat lsr (7 - px)) land 1 = 1 then fg else bg)
        done
      done
    end
    else begin
      (* GRAPHIC 3 (SCREEN4) is GRAPHIC 2 tiles under sprite mode 2. *)
      let g2 = mode_g2 t || display_mode t = Graphic3 in
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
      let row = y / 8 and py = y land 7 in
      let third = row lsr 3 in
      for col = 0 to 31 do
        let name = vr (nt + (row * 32) + col) in
        let pat_addr, col_addr =
          if g2 then begin
            let index = (lnot 0 lsl 13) lor (third lsl 11) lor (name lsl 3) lor py in
            (pt_base land index, ct_base land index)
          end
          else
            ( pt_base land ((lnot 0 lsl 11) lor (name lsl 3) lor py),
              ct_base land ((lnot 0 lsl 6) lor (name lsr 3)) )
        in
        let bits = vr pat_addr and col_byte = vr col_addr in
        let fg = pal.((col_byte lsr 4) land 0xf) and bg = pal.(col_byte land 0xf) in
        for px = 0 to 7 do
          set ((col * 8) + px) (if (bits lsr (7 - px)) land 1 = 1 then fg else bg)
        done
      done
    end;
    (* Sprite dots sit on the 256-dot grid. In the 512-wide GRAPHIC 5 a dot
       covers two native pixels and its colour splits into two 2-bit palette
       indices; GRAPHIC 6 repeats one colour across both (openMSX
       SpriteConverter::drawMode2). *)
    match sprites_on t ~display_line:y with
    | None -> ()
    | Some (mode, line) ->
      let put =
        match display_mode t with
        | Graphic5 -> fun x c -> set (2 * x) pal.(c lsr 2); set ((2 * x) + 1) pal.(c land 3)
        | Graphic6 -> fun x c -> set (2 * x) pal.(c); set ((2 * x) + 1) pal.(c)
        | Graphic7 -> fun x c -> set x graphic7_sprite_rgb.(c)
        | Text1 | Text2 | Multicolor | Graphic1 | Graphic2 | Graphic3 | Graphic4
        | Undefined _ -> fun x c -> set x pal.(c)
      in
      Vdp_sprite.draw mode ~regs:t.regs line put
  end

(* A line scanned at the other width is resized: a 256-wide line doubles
   each dot into a 512-wide frame, a 512-wide line keeps the left pixel of
   each pair in a 256-wide frame. *)
let present t =
  let w, h = scan_dims t in
  for y = 0 to h - 1 do
    let src = y * stride and dst = y * w * 3 in
    let wide = Bytes.get t.raster_wide y = '\001' in
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

let advance t ~cycles =
  let hit = ref false in
  t.cycle_in_line <- t.cycle_in_line + cycles;
  while t.cycle_in_line >= cycles_per_line do
    t.cycle_in_line <- t.cycle_in_line - cycles_per_line;
    let finished = t.line in
    if finished < visible_lines t then begin
      render_line t finished;
      (* 라인 인터럽트: 빔이 표시 라인 (R#19 - R#23) mod 256 의 오른쪽 끝을
         지날 때 S#1 bit0 — openMSX scheduleHScan. R#23 으로 스크롤해도
         분할 위치는 화면에 고정된다. FH 는 IE1(R#0 bit4) 이 켜져 있을 때만
         래치된다(openMSX execHScan: "if (controlRegs[0] & 0x10)"). IE1 없이
         세우면 IE1=0 인 게임이 매 프레임 S#1 읽기에서 1 을 본다. *)
      if t.regs.(0) land 0x10 <> 0
         && finished = (t.regs.(19) - t.regs.(23)) land 0xff
      then begin
        t.status1 <- t.status1 lor 1;
        t.int_pending <- true;
        hit := true
      end
    end;
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
      t.regs.(15) <- 0;
      present t
    end;
    if t.line >= lines_per_frame then t.line <- 0;
    if t.line < visible_lines t then check_sprites t ~display_line:t.line
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
  if Option.is_some t.tx && (t.tx_nx = 0 || t.tx_anx = 0 || t.tx_anx > t.tx_nx || t.tx_ny = 0) then
    State_codec.fail "invalid active VDP transfer"
