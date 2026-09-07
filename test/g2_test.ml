(* SCREEN 2 (GRAPHIC 2) 배경 렌더 증명. 레지스터·VRAM 을 전부 포트 경로로
   쓴다. 기대값의 정본은 openMSX CharacterConverter::renderGraphic2 와
   VDP::updateColorBase/updatePatternBase (주소 = base land index). *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let set_reg t r v =
  Vdp.io_write t ~port:0x99 v;
  Vdp.io_write t ~port:0x99 (0x80 lor r)

let vram_w t a v =
  Vdp.io_write t ~port:0x99 (a land 0xff);
  Vdp.io_write t ~port:0x99 (0x40 lor ((a lsr 8) land 0x3f));
  Vdp.io_write t ~port:0x98 v

let px t x y =
  let rgb = Vdp.frame_rgb t in
  let i = ((y * 256) + x) * 3 in
  (Char.code rgb.[i], Char.code rgb.[i + 1], Char.code rgb.[i + 2])

let is_col t x y c =
  let r1, g1, b1 = px t x y in
  let r2, g2, b2 = Vdp.palette_rgb t c in
  r1 = r2 && g1 = g2 && b1 = b2

(* MSX BASIC SCREEN 2 관용 배치: NT 0x1800 (R#2=06), CT 0x2000 (R#3=FF),
   PGT 0x0000 (R#4=03), SAT 0x1B00 (R#5=36, 비워 둔다), SPT 0x3800. *)
let nt = 0x1800
let ct = 0x2000
let pgt = 0x0000

let screen2 t =
  set_reg t 0 0x02;
  set_reg t 1 0xc0;
  set_reg t 2 0x06;
  set_reg t 3 0xff;
  set_reg t 4 0x03;
  set_reg t 5 0x36;
  set_reg t 6 0x07;
  vram_w t 0x1b00 0xd0

(* third 의 타일 n, 행 py 의 패턴/컬러 바이트. *)
let pat_w t third n py v = vram_w t (pgt + (third * 0x800) + (n * 8) + py) v
let col_w t third n py v = vram_w t (ct + (third * 0x800) + (n * 8) + py) v
let name_w t row col n = vram_w t (nt + (row * 32) + col) n

let () =
  (* 1. 컬러 바이트는 행마다 다르다 — 타일 1 의 행 py 는 전경색 py+1. *)
  let t = Vdp.create () in
  screen2 t;
  for py = 0 to 7 do
    pat_w t 0 1 py 0xff;
    col_w t 0 1 py ((py + 1) lsl 4)
  done;
  name_w t 0 0 1;
  for py = 0 to 7 do
    check (Printf.sprintf "row color: (0,%d) is color %d" py (py + 1)) (is_col t 0 py (py + 1));
    check (Printf.sprintf "row color: (7,%d) is color %d" py (py + 1)) (is_col t 7 py (py + 1))
  done;
  (* 배경 니블: 패턴 비트 0 인 픽셀은 하위 니블 색. *)
  for py = 0 to 7 do
    pat_w t 0 2 py 0xf0;
    col_w t 0 2 py 0x1f
  done;
  name_w t 0 1 2;
  check "bg nibble: (8,3) fg 1" (is_col t 8 3 1);
  check "bg nibble: (12,3) bg 15" (is_col t 12 3 15)

let () =
  (* 2. 세 분할은 화면 행/8 이 고른다 — name 의 상위 비트가 아니다. *)
  let t = Vdp.create () in
  screen2 t;
  for py = 0 to 7 do
    pat_w t 0 1 py 0xff;
    col_w t 0 1 py 0x10;
    pat_w t 1 1 py 0xf0;
    col_w t 1 1 py 0x50;
    pat_w t 2 7 py 0xaa;
    col_w t 2 7 py 0x9c
  done;
  name_w t 8 0 1;
  name_w t 16 5 7;
  check "third 1: (0,64) fg 5" (is_col t 0 64 5);
  check "third 1: (7,64) bg 0" (is_col t 7 64 0);
  check "third 2: (40,128) fg 9" (is_col t 40 128 9);
  check "third 2: (41,128) bg 12" (is_col t 41 128 12);
  (* 같은 name 1 이 third 0 에서는 third 0 의 패턴/컬러. *)
  name_w t 0 0 1;
  check "third 0: (7,0) fg 1" (is_col t 7 0 1)

let () =
  (* 3. R#3 하위 7비트는 컬러 index 비트 6-12 의 AND 마스크. R#3=0x80 이면
     타일 n 은 타일 (n mod 8) 의 컬러(third 0) 를 본다. *)
  let t = Vdp.create () in
  screen2 t;
  for py = 0 to 7 do
    pat_w t 0 9 py 0xff;
    col_w t 0 9 py 0x30;
    col_w t 0 1 py 0x60
  done;
  name_w t 0 0 9;
  check "R3=FF: tile 9 reads its own color 3" (is_col t 0 0 3);
  set_reg t 3 0x80;
  check "R3=80: tile 9 reads tile 1's color 6" (is_col t 0 0 6)

let () =
  (* 4. R#4 하위 2비트는 패턴 index 비트 11-12 의 AND 마스크. R#4=0 이면
     third 1 의 타일이 third 0 의 패턴을 쓴다 (컬러는 그대로 third 1). *)
  let t = Vdp.create () in
  screen2 t;
  for py = 0 to 7 do
    pat_w t 0 1 py 0xff;
    pat_w t 1 1 py 0xf0;
    col_w t 1 1 py 0x50
  done;
  name_w t 8 0 1;
  check "R4=03: third 1 pattern F0 -> (7,64) bg 0" (is_col t 7 64 0);
  set_reg t 4 0x00;
  check "R4=00: third 1 borrows third 0 pattern FF -> (7,64) fg 5" (is_col t 7 64 5)

let () =
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "g2_test: all pass"
