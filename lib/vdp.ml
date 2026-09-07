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
  (* 포트 쓰기 로그 — 부트 디버깅용 링버퍼. *)
  mutable wlog : (int * int * int) array;
  mutable wlog_i : int;
}

let lines_per_frame = 262
let cycles_per_line = 228
let vblank_line = 192

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
    wlog = Array.make 65536 (0, 0, 0);
    wlog_i = 0;
  }

(* 명령 레지스터 (R#32-46) 해석. 화면 모드에 따라 바이트 단위가
   다르다: P2 는 NonBitmap(SCREEN1-3, 2px/바이트, 128바이트/줄) 와
   G4/G5(1px/바이트, 128/256바이트/줄), G6/G7(512px) 를 구분한다. *)
let bytes_per_line t =
  let m1 = t.regs.(0) land 0x0e and m2 = t.regs.(1) land 0x18 in
  if m1 = 0x06 && m2 = 0x18 then 256 (* G7 *)
  else if m1 = 0x04 && (m2 = 0x10 || m2 = 0x18) then 128 (* G6: 2px/byte *)
  else if m1 = 0x0c then 128 (* G5 *)
  else if m1 = 0x08 && (m2 = 0x10 || m2 = 0x18) then 128 (* G4 *)
  else 128 (* NonBitmap: SCREEN1-3 *)

let reg16 t lo hi = (t.regs.(hi) land 1 lsl 8) lor t.regs.(lo)

let vram_addr t bank x y =
  let bpl = bytes_per_line t in
  ((if bank then 0x10000 else 0) + y * bpl + (x * bpl) / 256) land 0x1ffff

let logical op a c =
  match op with
  | 0 -> c
  | 1 -> a land c
  | 2 -> a lor c
  | 3 -> a lxor c
  | 4 -> lnot c land 0xff
  | _ -> c

(* 명령 실행 — 즉시 완료 (CE 는 관찰되지 않는다). HMMC/LMCM 은 CPU
   동기가 필요해 P2 후반으로 미룬다. *)
let exec_command t =
  let cmr = t.regs.(46) in
  let cmd = cmr lsr 4 and op = cmr land 0x0f in
  let arg = t.regs.(45) in
  let dix = arg land 0x04 = 0 and diy = arg land 0x08 = 0 in
  let mxs = arg land 0x10 <> 0 and mxd = arg land 0x20 <> 0 in
  let clr = t.regs.(44) in
  let sx = reg16 t 32 33 and sy = t.regs.(34) land 0xff + ((t.regs.(35) land 3) lsl 8) in
  let dx = reg16 t 36 37 and dy = t.regs.(38) land 0xff + ((t.regs.(39) land 3) lsl 8) in
  let nx = max 1 (reg16 t 40 41) and ny = max 1 (reg16 t 42 43) in
  let rd a = Char.code (Bytes.get t.vram a) in
  let wr a v = Bytes.set t.vram a (Char.chr (v land 0xff)) in
  let step_x n = if dix then n else -n in
  let step_y n = if diy then n else -n in
  match cmd with
  | 0x8 ->
    (* LMMV: 논리 채우기 — 바이트 단위 (NonBitmap 은 2px). *)
    for row = 0 to ny - 1 do
      for col = 0 to nx - 1 do
        let a = vram_addr t mxd (dx + step_x col) (dy + step_y row) in
        wr a (logical op (rd a) clr)
      done
    done
  | 0xC ->
    (* HMMV: 고속 채우기. *)
    for row = 0 to ny - 1 do
      for col = 0 to nx - 1 do
        wr (vram_addr t mxd (dx + step_x col) (dy + step_y row)) clr
      done
    done
  | 0xD ->
    (* HMMM: 고속 블록 복사. *)
    for row = 0 to ny - 1 do
      for col = 0 to nx - 1 do
        let src = vram_addr t mxs (sx + step_x col) (sy + step_y row) in
        let dst = vram_addr t mxd (dx + step_x col) (dy + step_y row) in
        wr dst (rd src)
      done
    done
  | 0xE ->
    (* YMMM: 세로 복사 (SX 무시, NX=줄 전체). *)
    let bpl = bytes_per_line t in
    for row = 0 to ny - 1 do
      for b = 0 to bpl - 1 do
        let src = vram_addr t mxs (b * 256 / bpl) (sy + step_y row) in
        let dst = vram_addr t mxd (b * 256 / bpl) (dy + step_y row) in
        wr dst (rd src)
      done
    done
  | 0x9 ->
    (* LMMM: 논리 블록 복사. *)
    for row = 0 to ny - 1 do
      for col = 0 to nx - 1 do
        let src = vram_addr t mxs (sx + step_x col) (sy + step_y row) in
        let dst = vram_addr t mxd (dx + step_x col) (dy + step_y row) in
        wr dst (logical op (rd dst) (rd src))
      done
    done
  | _ -> () (* STOP/POINT/PSET/SRCH/LINE/HMMC/LMCM — P2 후반 *)

let set_reg t r v =
  t.regs.(r) <- v land 0xff;
  if r = 46 then exec_command t

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

let io_write t ~port v =
  log_w t (port land 0xff) t.addr v;
  match port with
  | 0x98 ->
    (* CPU 어드레스 = R#14<<14 | 래치. 래치 랩에서 R#14 가 오른다
       (V9938 — openMSX executeCpuVramAccess). *)
    let full v = ((t.regs.(14) land 7) lsl 14) lor (v land 0x3fff) in
    Bytes.set t.vram (full t.addr) (Char.chr (v land 0xff));
    if t.addr = 0x3fff then t.regs.(14) <- (t.regs.(14) + 1) land 7;
    t.addr <- (t.addr + 1) land 0x3fff;
    t.read_buf <- Char.code (Bytes.get t.vram (full t.addr))
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
        if not t.write_mode then
          t.read_buf <-
            Char.code
              (Bytes.get t.vram
                 (((t.regs.(14) land 7) lsl 14) lor t.addr))
      end;
      t.latch_first <- true
    end
  | 0x9A ->
    (* 팔레트 데이터: 두 번 연속 쓰기 — 첫 (R<<4|B), 둘째 (G<<4).
       인덱스는 R#16 이 고르고 자동증가한다. *)
    (match t.pal_first with
     | None -> t.pal_first <- Some (v land 0xff)
     | Some lo ->
       let r = (lo lsr 4) land 7 and b = lo land 7 in
       let g = (v lsr 4) land 7 in
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
    t.read_buf <-
      Char.code
        (Bytes.get t.vram (((t.regs.(14) land 7) lsl 14) lor t.addr));
    if t.addr = 0x3fff then t.regs.(14) <- (t.regs.(14) + 1) land 7;
    t.addr <- (t.addr + 1) land 0x3fff;
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
      | 2 -> (if t.cmd_ce then 1 else 0) lor 0xfe
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
    if t.line = vblank_line then begin
      t.status0 <- t.status0 lor 0x80;
      t.int_pending <- true;
      hit := true
    end;
    (* 라인 인터럽트: 표시 라인이 R#19 와 같아질 때 S#1 bit0. *)
    if t.line < vblank_line && t.line = t.regs.(19) then begin
      t.status1 <- t.status1 lor 1;
      t.int_pending <- true;
      hit := true
    end;
    if t.line >= lines_per_frame then t.line <- 0
  done;
  !hit

let int_active t =
  let ie0 = t.regs.(1) land 0x20 <> 0 and ie1 = t.regs.(1) land 0x10 <> 0 in
  (t.status0 land 0x80 <> 0 && ie0) || (t.status1 land 1 <> 0 && ie1)

let blanked t = t.regs.(1) land 0x40 = 0
let vram t = t.vram
let regs t = t.regs

let write_log t =
  let n = min t.wlog_i 65536 in
  let out = ref [] in
  for k = 1 to n do
    out := t.wlog.((t.wlog_i - k) land 65535) :: !out
  done;
  !out

(* 모드 판정. M2(멀티컬러) 는 아직 없다. *)
let mode_text t = t.regs.(1) land 0x10 <> 0
let mode_g2 t = not (mode_text t) && t.regs.(0) land 0x02 <> 0

let frame_rgb t =
  let w = 256 and h = 192 in
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
    if mode_text t then begin
      (* TEXT1: 40×24, 6×8 셀 — 좌우 8px 는 R7 배경색. *)
      let nt = (r.(2) land 0xf) lsl 10 in
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
      let nt = (r.(2) land 0xf) lsl 10 in
      let pt = (r.(4) land 0x7) lsl 11 in
      let ct = (r.(3) land 0xff) lsl 6 in
      for row = 0 to 23 do
        for col = 0 to 31 do
          let name = vr (nt + row * 32 + col) in
          let pat_base, col_byte =
            if g2 then
              ( pt
                + ((name lsr 6) * 0x800)
                + ((name land 0x3f) * 8),
                vr (ct + ((name lsr 6) * 0x800) + ((name land 0x3f) * 8)) )
            else (pt + name * 8, vr (ct + (name lsr 3)))
          in
          let fg = (col_byte lsr 4) land 0xf and bg = col_byte land 0xf in
          for py = 0 to 7 do
            let bits = vr (pat_base + py) in
            for px = 0 to 7 do
              let x = col * 8 + px in
              let y = row * 8 + py in
              if (bits lsr (7 - px)) land 1 = 1 then put x y fg else put x y bg
            done
          done
        done
      done
    end;
    Bytes.to_string img
  end
