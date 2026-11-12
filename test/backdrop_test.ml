(* 색 0 투명(R#7 배경색)과 화면 끔(blank)의 계약 증명. 포트 경로로 쓰고
   VBlank 가 확정한 frame_rgb 픽셀로 판정한다.

   기대값의 출처는 openMSX SDLRasterizer.cc 다.
   - precalcColorIndex0: TP(R#8 bit5)=0 이면 배경의 색 0 은 R#7 bits 3-0 이
     가리키는 팔레트 색이다. TEXT1·TEXT2·MULTICOLOR 는 TP 를 무시하고 늘
     투명하다(openMSX #2218, V9958 실측). GRAPHIC7 은 늘 불투명하다.
     GRAPHIC5 는 짝수 점이 bits 3-2, 홀수 점이 bits 1-0 을 쓴다.
   - getBorderColors: 화면이 꺼진 줄은 테두리 색으로 채운다. GRAPHIC7 은
     R#7 전체를 GGGRRRBB 로, GRAPHIC5 는 짝수·홀수 점을 나눠 읽는다.
   색이 서로 겹치지 않게 팔레트 0 = 빨강, 1 = 초록, 2 = 노랑, 3 = 청록,
   4 = 파랑으로 둔다. *)

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

let poke t a v = seek t a; Vdp.io_write t ~port:0x98 v

let palette t i (r, g, b) =
  reg t 16 i;
  Vdp.io_write t ~port:0x9a ((r lsl 4) lor b);
  Vdp.io_write t ~port:0x9a g

let rgb (r, g, b) = (r * 255 / 7, g * 255 / 7, b * 255 / 7)
let red = (7, 0, 0)
let green = (0, 7, 0)
let yellow = (7, 7, 0)
let cyan = (0, 7, 7)
let blue = (0, 0, 7)

(* A VDP in the mode R#0/R#1 give, with the five palette colours above. *)
let vdp ~r0 ~r1 ?(r7 = 0) ?(r8 = 0) () =
  let t = Vdp.create () in
  List.iteri (fun i c -> palette t i c) [ red; green; yellow; cyan; blue ];
  reg t 0 r0;
  reg t 1 r1;
  reg t 7 r7;
  reg t 8 r8;
  t

(* Scan the 192 display lines; the VBlank publishes them. *)
let frame t = ignore (Vdp.advance t ~cycles:(228 * 192))

let px t x y =
  let w, _ = Vdp.frame_dims t in
  let s = Vdp.frame_rgb t in
  let i = ((y * w) + x) * 3 in
  (Char.code s.[i], Char.code s.[i + 1], Char.code s.[i + 2])

let screen5 = 0x06
let screen6 = 0x08
let screen8 = 0x0e

let () =
  (* GRAPHIC4 (SCREEN5): dot 0 holds colour 0, dot 1 colour 3 *)
  let g4 ?r8 () =
    let t = vdp ~r0:screen5 ~r1:0x40 ~r7:4 ?r8 () in
    poke t 0 0x03;
    frame t;
    t
  in
  let t = g4 () in
  check "G4: colour 0 shows the R#7 backdrop" (px t 0 0 = rgb blue);
  check "G4: other colours keep their palette entry" (px t 1 0 = rgb cyan);
  let t = g4 ~r8:0x20 () in
  check "G4 with TP: colour 0 is palette 0" (px t 0 0 = rgb red);

  (* GRAPHIC7 (SCREEN8): colour 0 stays opaque whatever R#7 and TP say *)
  let t = vdp ~r0:screen8 ~r1:0x40 ~r7:0xff () in
  frame t;
  check "G7: colour 0 is the fixed black" (px t 0 0 = (0, 0, 0));

  (* GRAPHIC5 (SCREEN6): R#7 = 0b10_01, even dots palette 2, odd palette 1 *)
  let t = vdp ~r0:screen6 ~r1:0x40 ~r7:0x09 () in
  frame t;
  check "G5: even dot takes R#7 bits 3-2" (px t 0 0 = rgb yellow);
  check "G5: odd dot takes R#7 bits 1-0" (px t 1 0 = rgb green);
  let t = vdp ~r0:screen6 ~r1:0x40 ~r7:0x09 ~r8:0x20 () in
  frame t;
  check "G5 with TP: both dots are palette 0" (px t 0 0 = rgb red && px t 1 0 = rgb red);

  (* GRAPHIC2 (SCREEN2): an all-zero colour table gives background 0 *)
  let g2 ?r8 () =
    let t = vdp ~r0:0x02 ~r1:0x40 ~r7:4 ?r8 () in
    reg t 2 0x06;
    reg t 3 0xff;
    reg t 4 0x03;
    frame t;
    t
  in
  check "G2: background 0 shows the backdrop" (px (g2 ()) 0 0 = rgb blue);
  check "G2 with TP: background 0 is palette 0" (px (g2 ~r8:0x20 ()) 0 0 = rgb red);

  (* TEXT1: the pattern table sits at R#4 << 11 (openMSX
     VDP::updatePatternBase), so R#4 = 1 puts it at 0x800. Character 0's
     first row has its leftmost dot set; that dot is x = 9, after the
     9-dot left border of the V99x8 text modes (openMSX getLeftSprites).
     TEXT1 ignores TP: a colour-0 foreground still shows the backdrop. *)
  let text ~r7 ?r8 () =
    let t = vdp ~r0:0x00 ~r1:0x50 ~r7 ?r8 () in
    reg t 2 0x00;
    reg t 4 0x01;
    poke t 0x800 0x80;
    frame t;
    t
  in
  let t = text ~r7:0x43 () in
  check "TEXT1: pattern table at R#4 << 11" (px t 9 0 = rgb blue);
  check "TEXT1: clear dots take R#7 bits 3-0" (px t 10 0 = rgb cyan);
  check "TEXT1: left border is the backdrop" (px t 8 0 = rgb cyan);
  check "TEXT1 with TP: colour-0 foreground shows the backdrop"
    (px (text ~r7:0x03 ~r8:0x20 ()) 9 0 = rgb cyan);

  (* A blanked display fills the line with the border colour *)
  let t = vdp ~r0:screen5 ~r1:0x00 ~r7:4 () in
  poke t 0 0x33;
  frame t;
  check "blank G4: border colour, not the VRAM dots" (px t 0 0 = rgb blue && px t 1 0 = rgb blue);
  let t = vdp ~r0:screen8 ~r1:0x00 ~r7:0xe0 () in
  frame t;
  check "blank G7: R#7 as GGGRRRBB" (px t 0 0 = (0, 255, 0));
  let t = vdp ~r0:screen6 ~r1:0x00 ~r7:0x09 () in
  frame t;
  check "blank G5: even/odd border dots" (px t 0 0 = rgb yellow && px t 1 0 = rgb green);
  let t = vdp ~r0:screen5 ~r1:0x00 ~r7:4 ~r8:0x20 () in
  frame t;
  check "blank G4 with TP: the border ignores TP" (px t 0 0 = rgb blue);

  if !failures > 0 then exit 1;
  print_endline "backdrop_test: all pass"
