(* SCREEN6(Graphic5) 스프라이트: 4색 모드라 색 코드가 둘로 쪼개져 점 하나가
   512폭 픽셀 둘이 된다. 왼쪽 픽셀은 팔레트[코드 >> 2], 오른쪽은 팔레트[코드 & 3]
   이다 (openMSX SpriteConverter::drawMode2, MODE == GRAPHIC5). SCREEN7 은 같은
   색 둘이라 따로 두지 않는다. 기대 RGB 는 팔레트 항목을 직접 정해 손으로 적었다. *)

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
  set_reg t 14 (a lsr 14);
  Vdp.io_write t ~port:0x99 (a land 0xff);
  Vdp.io_write t ~port:0x99 (0x40 lor ((a lsr 8) land 0x3f));
  Vdp.io_write t ~port:0x98 v

let set_palette t i ~r ~g ~b =
  set_reg t 16 i;
  Vdp.io_write t ~port:0x9a ((r lsl 4) lor b);
  Vdp.io_write t ~port:0x9a g

let pixel t x y =
  let w, _ = Vdp.frame_dims t in
  let rgb = Vdp.frame_rgb t in
  let i = (y * w + x) * 3 in
  (Char.code rgb.[i], Char.code rgb.[i + 1], Char.code rgb.[i + 2])

let white = (255, 255, 255)
let red = (255, 0, 0)
let green = (0, 255, 0)
let blue = (0, 0, 255)

(* 8×8 스프라이트 하나(패턴 0 = 꽉 찬 8×8)를 (y=100, x=100)에 둔다. 512폭에서
   x 는 200..215 픽셀이다. 배경은 팔레트 3(파랑)으로 칠한다. 팔레트는 0 = 흰색,
   1 = 빨강, 2 = 초록, 3 = 파랑. 표는 그리는 줄(101..108)과 겹치지 않는다. *)
let scene ?(r0 = 0x08) ~color ~tp () =
  let t = Vdp.create () in
  List.iter (fun (r, v) -> set_reg t r v)
    [ (0, r0); (1, 0x40); (2, 0x1f); (3, 0xff); (4, 3); (5, 0x3f); (6, 7);
      (7, 0); (8, if tp then 0x20 else 0) ];
  set_palette t 0 ~r:7 ~g:7 ~b:7;
  set_palette t 1 ~r:7 ~g:0 ~b:0;
  set_palette t 2 ~r:0 ~g:7 ~b:0;
  set_palette t 3 ~r:0 ~g:0 ~b:7;
  for k = 0 to 7 do vram_w t (0x3800 + k) 0xff done;
  for r = 0 to 7 do vram_w t (0x1c00 + r) color done;
  List.iteri (fun k v -> vram_w t (0x1e00 + k) v) [ 100; 100; 0; 0 ];
  vram_w t 0x1e04 0xd8;
  for dy = 1 to 8 do
    for k = 0 to 15 do vram_w t (((100 + dy) * 128) + 44 + k) 0xff done
  done;
  t

let () =
  let t = scene ~color:10 ~tp:false () in
  check "code 10 = 2,2: left pixel is palette 2"
    (pixel t 200 104 = green);
  check "code 10 = 2,2: right pixel is palette 2"
    (pixel t 201 104 = green);
  let t = scene ~color:9 ~tp:false () in
  check "code 9 = 2,1: left pixel is palette 2" (pixel t 200 104 = green);
  check "code 9 = 2,1: right pixel is palette 1" (pixel t 201 104 = red);
  check "code 9: the next sprite dot repeats the pair"
    (pixel t 202 104 = green && pixel t 203 104 = red);
  check "code 9: the last sprite dot is pixels 214 and 215"
    (pixel t 214 104 = green && pixel t 215 104 = red);
  (* 코드의 아랫 2비트만 쓴다: 6 = 1,2 이고 12 = 3,0 이다. *)
  let t = scene ~color:6 ~tp:false () in
  check "code 6 = 1,2: left palette 1, right palette 2"
    (pixel t 200 104 = red && pixel t 201 104 = green);
  let t = scene ~color:12 ~tp:false () in
  check "code 12 = 3,0: left palette 3, right palette 0"
    (pixel t 200 104 = blue && pixel t 201 104 = white);
  let t = scene ~color:1 ~tp:false () in
  check "code 1 = 0,1: left pixel is palette 0" (pixel t 200 104 = white);
  check "code 1 = 0,1: right pixel is palette 1" (pixel t 201 104 = red);
  let t = scene ~color:0 ~tp:false () in
  check "code 0 with TP=0 is transparent: the background shows"
    (pixel t 200 104 = blue && pixel t 201 104 = blue);
  let t = scene ~color:0 ~tp:true () in
  check "code 0 with TP=1 is opaque: palette 0 on both pixels"
    (pixel t 200 104 = white && pixel t 201 104 = white);
  let t = scene ~color:9 ~tp:false () in
  check "pixels left and right of the sprite keep the background"
    (pixel t 199 104 = blue && pixel t 216 104 = blue);
  check "rows above and below the sprite are untouched (palette 0)"
    (pixel t 200 100 = white && pixel t 200 109 = white);
  (* SCREEN7 은 같은 색 둘이다: 팔레트 9 를 노랑으로 바꾼 코드 9. *)
  let t = scene ~r0:0x0a ~color:9 ~tp:false () in
  set_palette t 9 ~r:7 ~g:7 ~b:0;
  check "SCREEN7 code 9: both pixels use palette 9"
    (pixel t 200 104 = (255, 255, 0) && pixel t 201 104 = (255, 255, 0));
  if !failures > 0 then begin
    Printf.eprintf "g5_sprite_test: %d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "g5_sprite_test: all pass"
