(* SCREEN8(Graphic7) 스프라이트: 팔레트 레지스터가 아니라 고정 16색을 쓰고
   (openMSX Renderer::GRAPHIC7_SPRITE_PALETTE), 컬러 0 은 TP=0 일 때 투명이다.
   배경 바이트(GGGRRRBB)의 파랑 2비트는 0, 2, 4, 7 (/7)로 넓어진다
   (openMSX SDLRasterizer PALETTE256). 기대 RGB 는 그 표에서 손으로 계산했다:
   n * 255 / 7 = 0, 36, 72, 109, 145, 182, 218, 255. *)

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

let pixel t x y =
  let w, _ = Vdp.frame_dims t in
  let rgb = Vdp.frame_rgb t in
  let i = (y * w + x) * 3 in
  (Char.code rgb.[i], Char.code rgb.[i + 1], Char.code rgb.[i + 2])

(* 8×8 스프라이트 하나(패턴 0 = 꽉 찬 8×8)를 (y=100, x=100)에 둔다. 표 위치는
   그리는 줄(101..108)과 겹치지 않는 VRAM 에 있다. [tp] 는 R#8 bit5. *)
let scene ~color ~tp ~background =
  let t = Vdp.create () in
  List.iter (fun (r, v) -> set_reg t r v)
    [ (0, 0x0e); (1, 0x40); (2, 0x1f); (3, 0xff); (4, 3); (5, 0x3f); (6, 7);
      (7, 0); (8, if tp then 0x20 else 0) ];
  for k = 0 to 7 do vram_w t (0x3800 + k) 0xff done;
  for r = 0 to 7 do vram_w t (0x1c00 + r) color done;
  List.iteri (fun k v -> vram_w t (0x1e00 + k) v) [ 100; 100; 0; 0 ];
  vram_w t (0x1e04) 0xd8;
  for dy = 1 to 8 do vram_w t ((100 + dy) * 256 + 103) background done;
  t

let () =
  let t = scene ~color:9 ~tp:false ~background:0 in
  (* 팔레트 9 를 빨강으로 바꿔도 스프라이트는 고정 색이다. *)
  set_reg t 16 9; Vdp.io_write t ~port:0x9a 0x70; Vdp.io_write t ~port:0x9a 0x00;
  check "sprite colour 9 is the fixed blue, not palette register 9"
    (pixel t 103 104 = (0, 0, 255));
  let t = scene ~color:8 ~tp:false ~background:0 in
  check "sprite colour 8 is the fixed 0x472 (R7 G4 B2)"
    (pixel t 103 104 = (255, 145, 72));
  let t = scene ~color:0 ~tp:false ~background:0xe0 in
  check "colour 0 with TP=0 is transparent: the background shows"
    (pixel t 103 104 = (0, 255, 0));
  let t = scene ~color:0 ~tp:true ~background:0xe0 in
  check "colour 0 with TP=1 is opaque: fixed palette 0, black"
    (pixel t 103 104 = (0, 0, 0));
  let t = scene ~color:9 ~tp:false ~background:0xe0 in
  check "pixels outside the sprite keep the background"
    (pixel t 103 120 = (0, 0, 0) && pixel t 99 104 = (0, 0, 0));
  (* 배경의 파랑 2비트. *)
  let t = Vdp.create () in
  List.iter (fun (r, v) -> set_reg t r v) [ (0, 0x0e); (1, 0x40); (2, 0x1f) ];
  List.iteri (fun k v -> vram_w t (0x4000 + k) v) [ 1; 2; 3; 0 ];
  check "SCREEN8 blue 1 is 2/7" (pixel t 0 64 = (0, 0, 72));
  check "SCREEN8 blue 2 is 4/7" (pixel t 1 64 = (0, 0, 145));
  check "SCREEN8 blue 3 is 7/7" (pixel t 2 64 = (0, 0, 255));
  if !failures > 0 then begin
    Printf.eprintf "g7_sprite_test: %d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "g7_sprite_test: all pass"
