(* VDP 명령 POINT/SRCH/LINE/LMCM/STOP 과 S#2 BD·S#7·S#8·S#9 의 계약 증명.
   포트 경로(0x99 레지스터, 0x9B 간접 레지스터, 0x99 상태 읽기)로만 쓰고
   VRAM 바이트로 판정한다.

   기대값의 출처는 openMSX VDPCmdEngine.cc 다: executePoint, executeSrch
   ("this does NOT reset the BD flag"), executeLine (끝 검사 순서 "confirmed
   on real HW", 위쪽 경계는 멈추고 아래쪽은 감싼다), startLmcm/executeLmcm,
   resetColor, getBorderX, executeCommand (V9938 명령은 SCREEN 5-8 에서만).
   LINE 의 점 목록은 executeLine 의 Bresenham 누산기를 손으로 따라가 얻었다. *)

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
let raw t a = Char.code (Bytes.get (Vdp.vram t) a)

let status t n =
  reg t 15 n;
  let v = Vdp.io_read t ~port:0x99 in
  reg t 15 0;
  v

let ce t = status t 2 land 0x01 <> 0
let tr t = status t 2 land 0x80 <> 0
let bd t = status t 2 land 0x10 <> 0
let border_x t = status t 8 lor ((status t 9 land 1) lsl 8)

(* The display mode, blank and sprite-disable bits take effect where the
   next line starts (openMSX syncAtNextLine); the CPU's planar addressing
   and the command engine follow the mode from then. One line always
   passes one line start. *)
let settle t = ignore (Vdp.advance t ~cycles:228)

(* R#0 values: SCREEN5 0x06, SCREEN6 0x08, SCREEN7 0x0a, SCREEN8 0x0e. *)
let fresh r0 =
  let t = Vdp.create () in
  reg t 0 r0;
  reg t 1 0x40;
  settle t;
  t

(* SCREEN5 dot: two per byte, the even X in the high nibble. *)
let dot5 t x y =
  let b = raw t ((y * 128) + (x / 2)) in
  if x land 1 = 0 then b lsr 4 else b land 0x0f

let set5 t x y c =
  let a = (y * 128) + (x / 2) in
  let b = raw t a in
  poke t a (if x land 1 = 0 then (b land 0x0f) lor (c lsl 4) else (b land 0xf0) lor c)

(* R#32-R#46 through the R#17 auto-increment port, CMR last. *)
let command t ?(sx = 0) ?(sy = 0) ?(dx = 0) ?(dy = 0) ?(nx = 0) ?(ny = 0)
    ?(col = 0) ?(arg = 0) cmr =
  reg t 17 32;
  List.iter (Vdp.io_write t ~port:0x9b)
    [ sx land 0xff; sx lsr 8; sy land 0xff; sy lsr 8;
      dx land 0xff; dx lsr 8; dy land 0xff; dy lsr 8;
      nx land 0xff; nx lsr 8; ny land 0xff; ny lsr 8;
      col; arg; cmr ]

let point_cmd = 0x40
let srch = 0x60
let line = 0x70
let lmcm = 0xa0
let lmmc = 0xb0
let hmmv = 0xc0
let arg_maj = 0x01
let arg_eq = 0x02
let arg_dix = 0x04
let arg_diy = 0x08
let arg_mxs = 0x10

let () =
  (* POINT: COL 에 점을 읽고 S#7 이 보여 준다 (executePoint, readColor) *)
  let t = fresh 0x06 in
  poke t ((5 * 128) + 5) 0xa3;
  command t ~sx:10 ~sy:5 point_cmd;
  check "POINT SCREEN5 even dot is the high nibble" (status t 7 = 0x0a);
  command t ~sx:11 ~sy:5 point_cmd;
  check "POINT SCREEN5 odd dot is the low nibble" (status t 7 = 0x03);
  check "POINT ends at once: CE and TR clear" (not (ce t) && not (tr t));
  command t ~sx:11 ~sy:5 ~arg:arg_mxs point_cmd;
  check "POINT from absent expansion VRAM reads 0xFF unmasked" (status t 7 = 0xff);
  let t = fresh 0x0e in
  poke t ((2 * 256) + 3) 0xc5;
  command t ~sx:3 ~sy:2 point_cmd;
  check "POINT SCREEN8 reads the planar byte" (status t 7 = 0xc5)

let () =
  (* SRCH: 경계색을 찾으면 BD, 못 찾아도 BD 는 그대로, ASX 는 S#8/S#9 *)
  let t = fresh 0x06 in
  set5 t 100 7 5;
  command t ~sx:10 ~sy:7 ~col:5 srch;
  check "SRCH right finds the dot" (bd t && border_x t = 100);
  check "SRCH S#9 keeps bits 1-7 set" (status t 9 lor 1 = 0xff);
  check "reading S#9 clears BD" (not (bd t));
  command t ~sx:200 ~sy:7 ~col:5 ~arg:arg_dix srch;
  check "SRCH left (DIX) finds the dot" (bd t && border_x t = 100);
  command t ~sx:10 ~sy:7 ~col:5 srch;
  command t ~sx:10 ~sy:7 ~col:9 srch;
  check "SRCH not found does not reset BD" (bd t);
  check "SRCH not found stops on X = 256" (border_x t = 256);
  check "SRCH not found S#8" (status t 8 = 0x00);
  check "after S#9 BD is clear" (not (bd t));
  command t ~sx:10 ~sy:7 ~col:9 srch;
  check "SRCH not found leaves a clear BD clear" (not (bd t));
  command t ~sx:5 ~sy:7 ~col:9 ~arg:arg_dix srch;
  check "SRCH left off the edge stops on X = -1" (status t 8 = 0xff && status t 9 = 0xff);
  command t ~sx:10 ~sy:7 ~col:0 ~arg:arg_eq srch;
  check "SRCH EQ=1 stops on the first dot that differs" (bd t && border_x t = 100);
  command t ~sx:10 ~sy:7 ~col:0x15 srch;
  check "SRCH compares CL masked to the dot size" (bd t && border_x t = 100);
  command t ~sx:100 ~sy:7 ~col:5 srch;
  check "SRCH checks SX itself first" (bd t && border_x t = 100);
  let t = fresh 0x08 in
  poke t ((3 * 128) + 75) 0x80;
  command t ~sx:0 ~sy:3 ~col:2 srch;
  check "SRCH SCREEN6 walks 512 dots" (bd t && border_x t = 300)

let () =
  (* LINE: 점 NX+1 개, 장축 MAJ, Bresenham 누산기 ASX *)
  let t = fresh 0x06 in
  command t ~dx:10 ~dy:20 ~nx:10 ~ny:3 ~col:0x0f line;
  let expected =
    [ (10, 20); (11, 20); (12, 21); (13, 21); (14, 21); (15, 22); (16, 22);
      (17, 22); (18, 22); (19, 23); (20, 23) ] in
  let drawn = ref [] in
  for y = 18 to 25 do
    for x = 8 to 23 do
      if dot5 t x y <> 0 then drawn := (x, y) :: !drawn
    done
  done;
  check "LINE X-major draws the Bresenham dots"
    (List.sort compare !drawn = List.sort compare expected);
  check "LINE X-major leaves ASX for S#8" (border_x t = 4);
  check "LINE ends at once" (not (ce t));
  let t = fresh 0x06 in
  command t ~dx:50 ~dy:50 ~nx:4 ~ny:2 ~col:0x0f ~arg:arg_maj line;
  let expected = [ (50, 50); (51, 51); (51, 52); (52, 53); (52, 54) ] in
  let drawn = ref [] in
  for y = 48 to 57 do
    for x = 48 to 55 do
      if dot5 t x y <> 0 then drawn := (x, y) :: !drawn
    done
  done;
  check "LINE Y-major draws the Bresenham dots"
    (List.sort compare !drawn = List.sort compare expected);
  check "LINE Y-major leaves ASX for S#8" (border_x t = 3);
  let t = fresh 0x06 in
  command t ~dx:2 ~dy:30 ~nx:10 ~ny:0 ~col:0x0f ~arg:arg_dix line;
  check "LINE left stops at the edge"
    (dot5 t 0 30 = 15 && dot5 t 1 30 = 15 && dot5 t 2 30 = 15
     && dot5 t 255 30 = 0 && dot5 t 254 30 = 0);
  let t = fresh 0x06 in
  command t ~dx:40 ~dy:1 ~nx:5 ~ny:0 ~col:0x0f ~arg:(arg_maj lor arg_diy) line;
  check "LINE up stops at the top edge"
    (dot5 t 40 1 = 15 && dot5 t 40 0 = 15 && dot5 t 40 1023 = 0 && dot5 t 40 1022 = 0);
  let t = fresh 0x06 in
  command t ~dx:60 ~dy:1023 ~nx:2 ~ny:0 ~col:0x0f ~arg:arg_maj line;
  check "LINE down wraps from Y 1023 to 0"
    (dot5 t 60 1023 = 15 && dot5 t 60 0 = 15 && dot5 t 60 1 = 15 && dot5 t 60 2 = 0);
  let t = fresh 0x06 in
  command t ~dx:70 ~dy:5 ~nx:0 ~ny:0 ~col:0x0f line;
  check "LINE NX=0 draws one dot" (dot5 t 70 5 = 15 && dot5 t 71 5 = 0);
  let t = fresh 0x06 in
  set5 t 10 40 0x0a;
  command t ~dx:10 ~dy:40 ~col:0x03 (line lor 0x03);
  check "LINE XOR combines with the dot" (dot5 t 10 40 = 0x09);
  command t ~dx:10 ~dy:40 ~col:0x00 (line lor 0x08);
  check "LINE TIMP skips colour 0" (dot5 t 10 40 = 0x09);
  let t = fresh 0x08 in
  command t ~dx:0 ~dy:0 ~col:0x07 line;
  check "LINE SCREEN6 masks CL to two bits" (raw t 0 = 0xc0);
  let t = fresh 0x0e in
  command t ~dx:0 ~dy:0 ~nx:2 ~col:0xe3 line;
  check "LINE SCREEN8 writes whole bytes"
    (raw t 0 = 0xe3 && raw t 0x10000 = 0xe3 && raw t 1 = 0xe3 && raw t 0x10001 = 0)

let () =
  (* LMCM: S#7 이 점을 하나씩 내주고, 마지막 점을 꺼낸 뒤 CE 가 내려간다 *)
  let t = fresh 0x06 in
  poke t ((10 * 128) + 10) 0x12;
  poke t ((11 * 128) + 10) 0x34;
  command t ~sx:20 ~sy:10 ~nx:2 ~ny:2 lmcm;
  check "LMCM starts with CE and TR" (ce t && tr t);
  let a = status t 7 in
  let b = status t 7 in
  check "LMCM S#8 follows ASX" (status t 8 = 21);
  let c = status t 7 in
  check "LMCM ends once the last dot is fetched" (not (ce t));
  let d = status t 7 in
  check "LMCM hands the dots in order" ([ a; b; c; d ] = [ 1; 2; 3; 4 ]);
  check "LMCM ends with ASX back at SX" (border_x t = 20);
  let t = fresh 0x06 in
  poke t ((10 * 128) + 10) 0x12;
  command t ~sx:21 ~sy:10 ~nx:2 ~ny:1 ~arg:arg_dix lmcm;
  let a = status t 7 in
  let b = status t 7 in
  check "LMCM DIX reads right to left" ([ a; b ] = [ 2; 1 ]);
  let t = fresh 0x06 in
  poke t ((4 * 128) + 127) 0x56;
  poke t ((5 * 128) + 127) 0x78;
  command t ~sx:254 ~sy:4 ~nx:4 ~ny:2 lmcm;
  let a = status t 7 in
  let b = status t 7 in
  let c = status t 7 in
  check "LMCM clips NX at the right edge"
    ([ a; b; c ] = [ 5; 6; 7 ] && not (ce t) && status t 7 = 8);
  let t = fresh 0x0e in
  poke t 0 0x9c;
  command t ~nx:1 ~ny:1 lmcm;
  check "LMCM SCREEN8 hands whole bytes" (not (ce t) && status t 7 = 0x9c);
  (* startLmcm raises its own request: an earlier R#44 write is used up,
     not left for the next LMMC (registers set without touching R#44). *)
  let t = fresh 0x06 in
  poke t 0 0xf0;
  command t ~nx:1 ~ny:1 lmcm;
  reg t 38 3;
  reg t 40 1;
  reg t 42 1;
  reg t 46 lmmc;
  let active, _, _, _, _ = Vdp.tx_state t in
  check "LMCM uses up an earlier R#44 request" (active && dot5 t 0 3 = 0);
  (* TR stays up after the last dot until S#7 is read (resetColor). *)
  let t = fresh 0x06 in
  command t ~nx:1 ~ny:1 lmcm;
  let s2 = status t 2 in
  check "LMCM done keeps TR for the last dot" (s2 land 0x01 = 0 && s2 land 0x80 <> 0);
  let t = fresh 0x06 in
  command t ~nx:1 ~ny:1 lmcm;
  ignore (status t 7);
  check "S#7 read with no command clears TR" (not (tr t))

let () =
  (* Save mid-LMCM; the restored VDP hands out the same remaining dots. *)
  let t = fresh 0x06 in
  poke t ((10 * 128) + 10) 0x12;
  poke t ((11 * 128) + 10) 0x34;
  set5 t 100 7 5;
  command t ~sx:10 ~sy:7 ~col:5 srch;
  command t ~sx:20 ~sy:10 ~nx:2 ~ny:2 lmcm;
  ignore (status t 7);
  let w = State_codec.writer () in
  Vdp.write_state w t;
  let state = State_codec.finish w in
  let r = State_codec.reader state in
  let u = Vdp.create () in
  Vdp.read_state r u;
  State_codec.end_of_input r;
  let rest v = List.map (fun _ -> status v 7) [ 1; 2; 3 ] in
  check "restored LMCM keeps BD" (bd u);
  check "restored LMCM keeps CE" (ce u);
  check "restored LMCM hands the same dots" (rest u = rest t && not (ce u))

let () =
  (* V9938 명령은 SCREEN 5-8 에서만 돈다 (executeCommand, scrMode < 0) *)
  let t = fresh 0x00 in
  command t ~nx:8 ~ny:1 ~col:0x55 hmmv;
  check "HMMV in GRAPHIC1 leaves VRAM" (raw t 0 = 0 && raw t 3 = 0);
  command t ~nx:2 ~ny:1 lmmc;
  reg t 44 0x0f;
  let active, _, _, _, _ = Vdp.tx_state t in
  check "LMMC in GRAPHIC1 starts no transfer" (not active && not (ce t) && raw t 0 = 0)

let () =
  (* STOP 은 진행 중인 전송을 끝낸다 (startAbrt → commandDone) *)
  let t = fresh 0x06 in
  command t ~nx:4 ~ny:1 ~col:0x01 lmmc;
  reg t 44 0x02;
  command t 0x00;
  reg t 44 0x03;
  check "STOP ends LMMC" (not (ce t) && dot5 t 0 0 = 1 && dot5 t 1 0 = 2 && dot5 t 2 0 = 0)

let () =
  (* NX 는 10비트다 (setCmdReg 0x09): 0x204 는 4 가 아니라 516, 한 줄 전체 *)
  let t = fresh 0x06 in
  command t ~nx:0x204 ~ny:1 ~col:0x55 hmmv;
  check "HMMV NX bit 9 counts" (raw t 2 = 0x55 && raw t 127 = 0x55 && raw t 128 = 0);
  (* LMMM leaves ASX back at SX (executeLmmm resets it at each row end) *)
  let t = fresh 0x06 in
  command t ~sx:30 ~sy:0 ~dx:0 ~dy:8 ~nx:3 ~ny:2 0x90;
  check "LMMM leaves ASX at SX" (border_x t = 30)

(* Register write-back: the engine runs on SY, DY and NY themselves and
   leaves them where it stopped (openMSX VDPCmdEngine: "SY += TY; DY += TY;
   --NY" per finished row, DY moved by LINE). The CPU cannot read them
   back, so each case starts a second command by writing only some of
   R#32-R#46 and looks at where it draws. *)
let only t r v = reg t r v
let row_filled t y v = raw t (y * 128) = v && raw t ((y * 128) + 3) = v
let hmmm = 0xd0
let ymmm = 0xe0
let pset = 0x50

let () =
  (* HMMV over rows 10-12 leaves DY = 13, NY = 0. *)
  let t = fresh 0x06 in
  command t ~dx:0 ~dy:10 ~nx:8 ~ny:3 ~col:0x11 hmmv;
  only t 42 2;
  only t 46 hmmv;
  check "HMMV leaves DY on the next row" (row_filled t 13 0x11 && row_filled t 14 0x11);
  check "HMMV continuation stops after the new NY" (raw t (15 * 128) = 0);
  (* NY counted down to 0 means 1024 rows: the continuation fills from
     row 15 through the whole 1024-line space. *)
  only t 46 hmmv;
  check "HMMV leaves NY at 0, read as 1024 rows"
    (row_filled t 15 0x11 && row_filled t 200 0x11 && row_filled t 9 0x11);
  (* DIY up: rows 10, 9, 8, then DY = 7. *)
  let t = fresh 0x06 in
  command t ~dx:0 ~dy:10 ~nx:8 ~ny:3 ~col:0x22 ~arg:arg_diy hmmv;
  only t 42 1;
  only t 46 hmmv;
  check "HMMV DIY leaves DY above the last row" (row_filled t 7 0x22 && raw t (6 * 128) = 0);
  (* Clipped at the top: rows 1 and 0 of NY 5, then DY = -1 = 1023 and
     NY = 3. *)
  let t = fresh 0x06 in
  command t ~dx:0 ~dy:1 ~nx:8 ~ny:5 ~col:0x33 ~arg:arg_diy hmmv;
  check "HMMV DIY clips at row 0" (row_filled t 0 0x33 && row_filled t 1 0x33);
  only t 46 hmmv;
  check "clipped HMMV wraps DY to 1023 and keeps NY - rows"
    (row_filled t 1023 0x33 && row_filled t 1021 0x33 && raw t (1020 * 128) = 0);
  (* HMMM moves SY with DY: rows 0-1 to 100-101, then row 2 to 102. *)
  let t = fresh 0x06 in
  List.iter (fun y -> poke t (y * 128) (0x40 + y)) [ 0; 1; 2; 3 ];
  command t ~sx:0 ~sy:0 ~dx:0 ~dy:100 ~nx:2 ~ny:2 hmmm;
  only t 42 1;
  only t 46 hmmm;
  check "HMMM leaves SY and DY on the next row"
    (raw t (100 * 128) = 0x40 && raw t (101 * 128) = 0x41 && raw t (102 * 128) = 0x42
     && raw t (103 * 128) = 0);
  (* YMMM likewise. *)
  let t = fresh 0x06 in
  List.iter (fun y -> poke t (y * 128) (0x50 + y)) [ 0; 1; 2 ];
  command t ~sy:0 ~dx:0 ~dy:50 ~ny:1 ymmm;
  only t 46 ymmm;
  check "YMMM leaves SY and DY on the next row" (raw t (51 * 128) = 0x51);
  (* LINE, Y major: dots on rows 10-13, then DY = 14 (DY steps before the
     end test). A PSET started with CMR alone lands there. *)
  let t = fresh 0x06 in
  command t ~dx:4 ~dy:10 ~nx:3 ~ny:0 ~col:7 ~arg:arg_maj line;
  only t 44 9;
  only t 46 pset;
  check "LINE Y-major leaves DY one past the last dot"
    (dot5 t 4 13 = 7 && dot5 t 4 14 = 9);
  (* LINE, Y major, DIY up from row 1: dots on rows 1 and 0, then DY steps
     to -1 and the command ends there, so DY = 1023. *)
  let t = fresh 0x06 in
  command t ~dx:4 ~dy:1 ~nx:5 ~ny:0 ~col:7 ~arg:(arg_maj lor arg_diy) line;
  only t 44 9;
  only t 46 pset;
  check "LINE leaving the top edge leaves DY = 1023"
    (dot5 t 4 0 = 7 && dot5 t 4 1023 = 9);
  (* LINE, X major: DY stays on the last dot's row. *)
  let t = fresh 0x06 in
  command t ~dx:4 ~dy:10 ~nx:4 ~ny:2 ~col:7 line;
  only t 44 9;
  only t 36 40;
  only t 46 pset;
  check "LINE X-major leaves DY on the last dot's row" (dot5 t 40 12 = 9);
  (* LMMC: two rows of one dot, then DY = 22. The R#44 written before CMR
     is the first dot. *)
  let t = fresh 0x06 in
  command t ~dx:6 ~dy:20 ~nx:1 ~ny:2 ~col:3 lmmc;
  only t 44 4;
  only t 44 5;
  only t 46 pset;
  check "LMMC leaves DY on the next row"
    (dot5 t 6 20 = 3 && dot5 t 6 21 = 4 && dot5 t 6 22 = 5);
  (* STOP during LMMC keeps the rows already done: 2 of 4, so DY = 42 and
     NY = 2. *)
  let t = fresh 0x06 in
  command t ~dx:0 ~dy:40 ~nx:1 ~ny:4 ~col:1 lmmc;
  only t 44 1;
  only t 46 0x00;
  only t 40 8;
  only t 44 0x66;
  only t 46 hmmv;
  check "STOP during LMMC keeps the finished rows"
    (row_filled t 42 0x66 && row_filled t 43 0x66 && raw t (44 * 128) = 0);
  (* LMCM moves SY: one dot from row 30, then POINT reads row 31. *)
  let t = fresh 0x06 in
  set5 t 2 30 0xa;
  set5 t 2 31 0xb;
  command t ~sx:2 ~sy:30 ~nx:1 ~ny:1 lmcm;
  check "LMCM reads its dot" (status t 7 = 0x0a);
  only t 46 point_cmd;
  check "LMCM leaves SY on the next row" (status t 7 = 0x0b)

let () =
  if !failures > 0 then begin
    Printf.eprintf "cmd_test: %d failure(s)\n%!" !failures;
    exit 1
  end;
  print_endline "cmd_test: all pass"
