(* 걸어다니는 뼈대: 코어 프레임을 터미널 half-block(▀) 모자이크로 찍는다.
   masc TUI 탭이 할 일의 최소판이다 — frame_rgb 를 받아 셀 그리드로 줄이고,
   다음 프레임을 step 으로 당긴다. *)

let esc = "\027"

let tty_size () =
  try
    let ic = Unix.open_process_in "stty size" in
    let line = input_line ic in
    let (_ : Unix.process_status) = Unix.close_process_in ic in
    match String.index_opt line ' ' with
    | Some i ->
        ( int_of_string (String.sub line 0 i),
          int_of_string (String.sub line (i + 1) (String.length line - i - 1)) )
    | None -> (80, 24)
  with _ -> (80, 24)

(* 네이티브 w×h 를 cols×prows 픽셀 그리드로 최근접 샘플링. *)
let downsample cols prows (w, h) rgb =
  Array.init (cols * prows) (fun i ->
      let px = i mod cols and py = i / cols in
      let x = min (w - 1) (px * w / cols) in
      let y = min (h - 1) (py * h / prows) in
      let base = (y * w + x) * 3 in
      ( Char.code rgb.[base],
        Char.code rgb.[base + 1],
        Char.code rgb.[base + 2] ))

let mosaic_line buf cols y pixels =
  Buffer.clear buf;
  for x = 0 to cols - 1 do
    let (r1, g1, b1) = pixels.((y * 2) * cols + x) in
    let (r2, g2, b2) = pixels.((y * 2 + 1) * cols + x) in
    Buffer.add_string buf
      (Printf.sprintf "\027[38;2;%d;%d;%dm\027[48;2;%d;%d;%dm\xe2\x96\x80" r1 g1 b1 r2 g2 b2)
  done;
  Buffer.add_string buf "\027[0m\n"

let () =
  let frames = ref 0 in
  Arg.parse
    [ ("--frames", Arg.Int (fun n -> frames := n), "N  N 프레임 뒤 종료 (기본: 무한)") ]
    (fun _ -> ())
    "msx_demo — ocaml-msx 스텁 데모";
  let t = Msx.create ~machine:{ ram_kb = 64; vram_kb = 128; roms = [] } in
  Msx.load_cartridge t "\x00\x01\x02\x03";
  (* 스텁: 카트리지 자리만 확인 *)
  let c, r = tty_size () in
  let cols = max 20 (min (c - 2) 128) in
  let trows = max 10 (min (r - 2) 63) in
  let prows = trows * 2 in
  let w, h = Msx.frame_dims t in
  let buf = Buffer.create (cols * 48) in
  print_string (esc ^ "[?25l" ^ esc ^ "[2J");
  let n = ref 0 in
  let continue_loop () = !frames = 0 || !n < !frames in
  while continue_loop () do
    Msx.step t ~frames:1;
    let rgb = Msx.frame_rgb t in
    let pixels = downsample cols prows (w, h) rgb in
    output_string stdout (esc ^ "[H");
    for y = 0 to trows - 1 do
      mosaic_line buf cols y pixels;
      print_string (Buffer.contents buf)
    done;
    flush stdout;
    incr n;
    if !frames = 0 then Unix.sleepf 0.066
  done;
  print_string (esc ^ "[0m" ^ esc ^ "[?25h\n")
