(* 라인 래스터 증명. 빔이 라인을 벗어날 때의 상태로 그 라인을 그리고,
   VBlank 가 프레임을 확정한다. 포트 경로로만 쓰고 advance 로 라인을
   진행한 뒤 frame_rgb 픽셀로 판정한다.

   기대값의 출처: openMSX VDP.cc scheduleHScan (라인 인터럽트는 표시 라인
   (R#19 - R#23) & 0xFF 의 오른쪽 테두리), PixelRenderer 의 라인 단위
   동기화(레지스터 변경은 빔이 지난 라인부터 보인다). *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let reg t r v =
  Vdp.io_write t ~port:0x99 v;
  Vdp.io_write t ~port:0x99 (0x80 lor r)

let seek t a =
  reg t 14 (a lsr 14);
  Vdp.io_write t ~port:0x99 (a land 0xff);
  Vdp.io_write t ~port:0x99 (0x40 lor ((a lsr 8) land 0x3f))

let palette t i (r, g, b) =
  reg t 16 i;
  Vdp.io_write t ~port:0x9a ((r lsl 4) lor b);
  Vdp.io_write t ~port:0x9a g

let status t n =
  reg t 15 n;
  let v = Vdp.io_read t ~port:0x99 in
  reg t 15 0;
  v

let lines t n = ignore (Vdp.advance t ~cycles:(228 * n))
let line_now t = fst (Vdp.line_now t)

(* Run from the current line into VBlank, so the frame just scanned is
   published. *)
let finish t =
  let vblank () = if (Vdp.regs t).(9) land 0x80 = 0 then 192 else 212 in
  let rec go () = if line_now t <> vblank () then (lines t 1; go ()) in
  lines t 1;
  go ()

let px t x y =
  let w, _ = Vdp.frame_dims t in
  let rgb = Vdp.frame_rgb t in
  let i = ((y * w) + x) * 3 in
  (Char.code rgb.[i], Char.code rgb.[i + 1], Char.code rgb.[i + 2])

let white = (255, 255, 255)
let red = (255, 0, 0)

(* SCREEN5 (R#0=0x06), display on, page 0 (R#2 = 0x1F: bits 4-0 set so
   every line shows). Rows [0, split) hold colour 1, the rest colour 2;
   colour 1 is white and 2 red. *)
let screen5 ?(split = 256) () =
  let t = Vdp.create () in
  reg t 0 0x06;
  reg t 1 0x40;
  reg t 2 0x1f;
  palette t 1 (7, 7, 7);
  palette t 2 (7, 0, 0);
  (* One seek a row: the mode is not in effect before the line start, and
     outside GRAPHIC 4-7 the address counter does not carry into R#14. *)
  for row = 0 to 255 do
    let c = if row < split then 0x11 else 0x22 in
    seek t (row * 128);
    for _ = 0 to 127 do Vdp.io_write t ~port:0x98 c done
  done;
  t

let () =
  (* 1. R#23 을 화면 중간에 바꾸면 그 뒤 라인만 스크롤된다 *)
  let t = screen5 ~split:100 () in
  lines t 100;
  reg t 23 50;
  finish t;
  check "scroll split: line 60 keeps scroll 0 (row 60 white)" (px t 0 60 = white);
  check "scroll split: line 99 keeps scroll 0" (px t 0 99 = white);
  check "scroll split: line 100 shows row 150 (red)" (px t 0 100 = red)

let () =
  (* 2. 팔레트를 화면 중간에 바꾸면 그 뒤 라인만 새 색이다 *)
  let t = screen5 () in
  lines t 100;
  palette t 1 (7, 0, 0);
  finish t;
  check "palette split: line 99 white" (px t 0 99 = white);
  check "palette split: line 100 red" (px t 0 100 = red)

let () =
  (* 3. 라인 인터럽트는 표시 라인 (R#19 - R#23) 을 다 그린 뒤에 선다 *)
  let t = screen5 () in
  reg t 0 0x16;
  reg t 19 100;
  reg t 23 10;
  lines t 90;
  check "line int: not before line 90 finishes" (status t 1 land 1 = 0);
  lines t 1;
  check "line int: R#19 - R#23 = 90" (status t 1 land 1 = 1);
  let t = screen5 () in
  reg t 0 0x16;
  reg t 19 0;
  lines t 1;
  check "line int: R#19 = 0 fires after line 0" (status t 1 land 1 = 1)

let () =
  (* 4. 게임식 분할: FH 를 보면 R#23 을 바꾼다. 분할 아래만 스크롤된다 *)
  let t = screen5 ~split:100 () in
  reg t 0 0x16;
  reg t 19 79;
  let rec scan () =
    if line_now t < 191 then begin
      lines t 1;
      if Vdp.int_active t && status t 1 land 1 = 1 then reg t 23 50;
      scan ()
    end
  in
  scan ();
  finish t;
  check "split screen: line 79 above the split" (px t 0 79 = white);
  check "split screen: line 80 starts row 130" (px t 0 80 = red)

let () =
  (* 5. frame_dims 는 VBlank 가 확정한 프레임을 따른다 *)
  let t = Vdp.create () in
  check "fresh frame is 256x192" (Vdp.frame_dims t = (256, 192));
  check "fresh frame is black"
    (String.for_all (fun c -> c = '\000') (Vdp.frame_rgb t));
  reg t 0 0x0a;
  reg t 1 0x40;
  reg t 9 0x80;
  check "mode change waits for VBlank" (Vdp.frame_dims t = (256, 192));
  finish t;
  check "SCREEN7 212 published" (Vdp.frame_dims t = (512, 212));
  check "RGB length follows published dims"
    (String.length (Vdp.frame_rgb t) = 512 * 212 * 3)

let () =
  (* 6. 폭이 다른 라인은 확정 프레임 폭에 맞춘다: 256 라인은 두 배로 *)
  let t = screen5 () in
  seek t 0;
  Vdp.io_write t ~port:0x98 0x12;
  lines t 100;
  reg t 0 0x08; (* SCREEN6, 512 wide *)
  finish t;
  check "mixed width: frame is 512 wide" (fst (Vdp.frame_dims t) = 512);
  check "mixed width: 256 line doubles dot 0" (px t 0 0 = white && px t 1 0 = white);
  check "mixed width: 256 line doubles dot 1" (px t 2 0 = red && px t 3 0 = red)

let () =
  (* 7. BLANK 을 화면 중간에 끄면 그 뒤 라인은 검다 *)
  let t = screen5 () in
  lines t 100;
  reg t 1 0x00;
  finish t;
  check "blank split: line 99 drawn" (px t 0 99 = white);
  check "blank split: line 100 black" (px t 0 100 = (0, 0, 0))

let () =
  if !failures > 0 then begin
    Printf.eprintf "raster_test: %d failure(s)\n%!" !failures;
    exit 1
  end;
  print_endline "raster_test: all pass"
