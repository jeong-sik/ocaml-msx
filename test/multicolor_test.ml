(* SCREEN3(MULTICOLOR): 이름 하나가 패턴 8바이트를 고르고, 블록 줄
   (y >> 2) mod 8 번째 바이트의 윗 니블이 왼쪽 4×4 블록, 아랫 니블이 오른쪽
   블록의 색이다 (openMSX CharacterConverter::renderMulti). 같은 BASIC 프로그램
   (SCREEN 3 에 상자·원·글자)을 실제 BIOS 로 openMSX 와 우리 쪽에서 돌려 화면
   전체가 같은 것도 확인했다. 컬러 0 은 TP=0 이면 R#7 하위 4비트의 백드롭 색. *)

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

let is_col t x y c =
  let w, _ = Vdp.frame_dims t in
  let rgb = Vdp.frame_rgb t in
  let i = ((y * w) + x) * 3 in
  (Char.code rgb.[i], Char.code rgb.[i + 1], Char.code rgb.[i + 2])
  = Vdp.palette_rgb t c

let () =
  let t = Vdp.create () in
  (* R#1 = 0x48: 화면 켬 + M2. 이름표 0x0800, 패턴표 0x0000, 백드롭 6. *)
  List.iter (fun (r, v) -> set_reg t r v)
    [ (0, 0x00); (1, 0x48); (2, 0x02); (4, 0x00); (7, 0x06); (8, 0x00) ];
  (* 패턴 1 의 바이트 0..3. *)
  List.iteri (fun k v -> vram_w t (8 + k) v) [ 0x1f; 0x23; 0x45; 0x67 ];
  (* 패턴 2 의 바이트 0..1. 패턴 3 은 비어 있다. *)
  List.iteri (fun k v -> vram_w t (16 + k) v) [ 0xab; 0xcd ];
  (* 이름표: (줄 0, 칸 0), (줄 1, 칸 0), (줄 4, 칸 0) 이 패턴 1. *)
  List.iter (fun (row, col) -> vram_w t (0x800 + (row * 32) + col) 1)
    [ (0, 0); (1, 0); (4, 0) ];
  vram_w t (0x800 + (4 * 32) + 1) 2;
  check "block (0,0): left top is colour 1" (is_col t 0 0 1);
  check "block (0,0): left top covers 4x4" (is_col t 3 3 1);
  check "block (0,0): right top is colour 15" (is_col t 4 0 15);
  check "block (0,0): right top covers 4x4" (is_col t 7 3 15);
  check "second block row: left is colour 2" (is_col t 0 4 2);
  check "second block row: right is colour 3" (is_col t 4 7 3);
  check "name table row 1 uses bytes 2 and 3: colour 4/5" (is_col t 0 8 4 && is_col t 4 8 5);
  check "name table row 1 second half: colour 6/7" (is_col t 0 12 6 && is_col t 4 15 7);
  check "name table row 4 wraps to byte 0 (y 32): colour 1/15"
    (is_col t 0 32 1 && is_col t 4 32 15);
  check "name table row 4 wraps to byte 1 (y 36): colour 2/3"
    (is_col t 0 36 2 && is_col t 4 36 3);
  check "name table row 4, pattern 2: byte 0 is colour 10/11, byte 1 is 12/13"
    (is_col t 8 32 10 && is_col t 12 32 11 && is_col t 8 36 12 && is_col t 12 36 13);
  check "name 0 has an empty pattern: colour 0 shows the backdrop 6" (is_col t 8 0 6);
  if !failures > 0 then begin
    Printf.eprintf "multicolor_test: %d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "multicolor_test: all pass"
