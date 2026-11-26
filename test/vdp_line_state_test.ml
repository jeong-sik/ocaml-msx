(* 줄마다 그 줄의 표시가 시작될 때의 레지스터·팔레트로 화면을 그린다.
   표시 시작은 줄 안 43 사이클(openMSX VDP::getLeftSprites = 100+102+56 틱,
   6틱 = 1 Z80 사이클), 줄 인터럽트는 같은 줄의 214 사이클(getRightBorder =
   1282 틱)에서 걸린다. 같은 구성을 카트 ROM(줄 인터럽트 핸들러가 NOP p 개 뒤에
   R#7 을 쓴다)으로 openMSX 와 이 에뮬레이터에서 돌려 R#7 이 처음 바뀐 줄을
   견줬다: 왼쪽 가장자리 열에서 p = 33, 79, 124 에서 줄이 바뀌는 것이 두
   쪽 모두 같았다. 여기 시험은 그 두 상수와 줄 묶음 그리기를 Vdp 만으로 고정한다. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let set_reg t r v =
  Vdp.io_write t ~port:0x99 v;
  Vdp.io_write t ~port:0x99 (0x80 lor r)

let cycles_per_line = 228
let cycles_per_frame = 262 * cycles_per_line

let advance t n = ignore (Vdp.advance t ~cycles:n)

(* SCREEN5 가득 컬러 0: TP=0 이라 모든 픽셀이 R#7 하위 4비트의 백드롭 색이다. *)
let g4 () =
  let t = Vdp.create () in
  List.iter (fun (r, v) -> set_reg t r v)
    [ (0, 0x06); (1, 0x40); (2, 0x1f); (7, 0x0a); (8, 0x00); (9, 0x80) ];
  t

let pixel t x y =
  let w, _ = Vdp.frame_dims t in
  let rgb = Vdp.frame_rgb t in
  let i = ((y * w) + x) * 3 in
  (Char.code rgb.[i], Char.code rgb.[i + 1], Char.code rgb.[i + 2])

let col t c = Vdp.palette_rgb t c

(* 줄 [line] 의 [cycle] 사이클에 R#7 = [v] 를 쓰고 프레임 끝까지 돌린다. *)
let write_r7_at ~line ~cycle v =
  let t = g4 () in
  advance t ((line * cycles_per_line) + cycle);
  set_reg t 7 v;
  advance t (cycles_per_frame - ((line * cycles_per_line) + cycle));
  t

let () =
  let t = write_r7_at ~line:50 ~cycle:42 0x05 in
  check "a write before cycle 43 reaches the same line" (pixel t 0 49 = col t 10);
  check "R#7 = 5 from line 50" (pixel t 0 50 = col t 5 && pixel t 255 100 = col t 5);
  let t = write_r7_at ~line:50 ~cycle:43 0x05 in
  check "a write at cycle 43 misses the line" (pixel t 0 50 = col t 10);
  check "R#7 = 5 from the next line" (pixel t 0 51 = col t 5);
  (* 프레임 끝(줄 0 의 시작)에서 읽은 화면은 이번 프레임이 그린 값이다: 블랭킹
     쪽에서 바꾼 값(다음 프레임 몫)이 아니다. *)
  let t = g4 () in
  advance t (230 * cycles_per_line);
  set_reg t 7 0x05;
  advance t (cycles_per_frame - (230 * cycles_per_line));
  check "a write during vertical blanking is not in this frame"
    (pixel t 0 0 = col t 10 && pixel t 0 211 = col t 10);
  (* 팔레트도 줄마다. *)
  let t = g4 () in
  set_reg t 7 0x01;
  set_reg t 16 1;
  advance t ((80 * cycles_per_line) + 10);
  (* 항목 1~15, 0 순서로 16개를 다시 쓴다: 인덱스가 한 바퀴 돌아 R#16 은
     같고 항목 1 만 빨강이 된다. *)
  set_reg t 16 1;
  for entry = 1 to 16 do
    if entry = 1 then begin
      Vdp.io_write t ~port:0x9a 0x70;
      Vdp.io_write t ~port:0x9a 0x00
    end else begin
      Vdp.io_write t ~port:0x9a 0x00;
      Vdp.io_write t ~port:0x9a 0x00
    end
  done;
  advance t (cycles_per_frame - ((80 * cycles_per_line) + 10));
  check "palette entry 1 before the change" (pixel t 0 79 = (0, 0, 0));
  check "palette entry 1 after the change" (pixel t 0 80 = (255, 0, 0));
  (* 줄이 안 바뀌면 줄 묶음 하나로 그린 것과 같다. *)
  let t = g4 () in
  advance t cycles_per_frame;
  check "no raster change draws the whole frame with one value"
    (pixel t 0 0 = col t 10 && pixel t 255 211 = col t 10);
  (* 모드 비트는 첫 줄 값을 따른다: 중간에 R#0 을 바꿔도 크기가 안 변한다. *)
  let t = g4 () in
  advance t (100 * cycles_per_line);
  set_reg t 0 0x08;
  advance t (cycles_per_frame - (100 * cycles_per_line));
  check "a mode change mid-frame keeps the frame size"
    (Vdp.frame_dims t = (256, 212) && String.length (Vdp.frame_rgb t) = 256 * 212 * 3);
  check "lines after a mode change still draw in the first line's mode"
    (pixel t 0 50 = col t 10 && pixel t 0 150 = col t 10);
  (* 줄 인터럽트: R#19 줄의 214 사이클, 비교는 R#19 - R#23. *)
  let t = g4 () in
  set_reg t 0 0x16; set_reg t 19 100;
  advance t ((100 * cycles_per_line) + 213);
  check "line interrupt: not before cycle 214" (not (Vdp.int_active t));
  advance t 1;
  check "line interrupt: at cycle 214" (Vdp.int_active t);
  let t = g4 () in
  set_reg t 0 0x16; set_reg t 19 100; set_reg t 23 10;
  advance t ((90 * cycles_per_line) + 214);
  check "line interrupt compares R#19 - R#23" (Vdp.int_active t);
  let t = g4 () in
  set_reg t 0 0x06; set_reg t 19 100;
  advance t cycles_per_frame;
  check "no line interrupt without IE1" (not (Vdp.int_active t));
  (* 상태를 복원하면 지금 값으로 그린다. *)
  let t = write_r7_at ~line:50 ~cycle:42 0x05 in
  let w = State_codec.writer () in
  Vdp.write_state w t;
  let t2 = Vdp.create () in
  Vdp.read_state (State_codec.reader (State_codec.finish w)) t2;
  check "restored VDP draws every line with the current R#7"
    (pixel t2 0 0 = col t2 5 && pixel t2 0 211 = col t2 5);
  if !failures > 0 then begin
    Printf.eprintf "vdp_line_state_test: %d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "vdp_line_state_test: all pass"
