(* 블록 명령이 CE(S#2 bit0)를 세워 두는 시간.

   명령 엔진은 VRAM 슬롯이 빌 때만 접근해서 표시가 켜진 활성 줄에서는 느리고
   (스프라이트가 켜지면 더 느리다), 표시가 꺼졌거나 VBlank 줄에서는 빠르다.
   아래 [ticks_per_unit] 은 openMSX(my8250cbios, V9938)에서 SCREEN 5·8 로 같은
   명령을 NX·NY 를 바꿔 가며 돌려 CE 가 서 있던 시간을 잰 기울기다(VDP 틱
   21.477 MHz, 단위는 HMMV·HMMM·YMMM 이 바이트, LMMV·LMMM 이 점, LINE 이 긴 변
   한 걸음). 두 화면 모드의 값은 1% 안에서 같았고, 표시 켜짐/꺼짐 x 스프라이트
   켜짐/꺼짐 네 조건에서 쟀다.

   기대 시간은 줄을 하나씩 걸으며 그 줄의 상태로 비용을 고르는 별도 시뮬레이터
   [expected_cycles] 로 낸다(부동소수점). 구현은 정수 1000분 단위로 같은 일을
   하므로 둘이 1% 안에서 맞아야 한다. 이 시뮬레이터가 시작 위상(활성 줄 끝에서
   시작, VBlank 에서 시작)과 명령 도중 R#1·R#8 을 바꾸는 경우를 가려낸다.

   같은 ROM 을 이 에뮬레이터에 올려 openMSX 의 CE 폴링 횟수와 견주면 224개 측정
   (SCREEN 5·8 x 명령 8종 x 조건 4 x 시작 위상 2 x 크기 2) 가운데 223개가 5% 안이다. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let set_reg t r v =
  Vdp.io_write t ~port:0x99 v;
  Vdp.io_write t ~port:0x99 (0x80 lor r)

let status t n =
  set_reg t 15 n;
  let v = Vdp.io_read t ~port:0x99 in
  set_reg t 15 0;
  v

let ce t = status t 2 land 1 <> 0

type kind = Hmmv | Lmmv | Hmmm | Lmmm | Ymmm | Line
type state = Blank | Display | Sprites

let kinds = [ Hmmv; Lmmv; Hmmm; Lmmm; Ymmm; Line ]
let states = [ Blank; Display; Sprites ]

let kind_name = function
  | Hmmv -> "HMMV" | Lmmv -> "LMMV" | Hmmm -> "HMMM"
  | Lmmm -> "LMMM" | Ymmm -> "YMMM" | Line -> "LINE"

let state_name = function Blank -> "blank" | Display -> "display" | Sprites -> "sprites"

(* openMSX 에서 잰 값. 틱/단위. *)
let ticks_per_unit kind state =
  match kind, state with
  | Hmmv, Blank -> 49.9 | Hmmv, Display -> 62.9 | Hmmv, Sprites -> 65.1
  | Lmmv, Blank -> 98.1 | Lmmv, Display -> 124.1 | Lmmv, Sprites -> 138.7
  | Hmmm, Blank -> 92.0 | Hmmm, Display -> 98.0 | Hmmm, Sprites -> 134.2
  | Lmmm, Blank -> 130.3 | Lmmm, Display -> 132.9 | Lmmm, Sprites -> 152.9
  | Ymmm, Blank -> 65.2 | Ymmm, Display -> 91.0 | Ymmm, Sprites -> 122.0
  | Line, Blank -> 130.4 | Line, Display -> 140.0 | Line, Sprites -> 159.2

let start_ticks = 400.0
let lines_per_frame = 262
let visible = 212

(* 줄 [line] 에서 엔진이 처한 상태. [display] 는 R#1 bit6, [sprites] 는 R#8 bit1 = 0. *)
let state_of ~display ~sprites line =
  if (not display) || line >= visible then Blank
  else if sprites then Sprites
  else Display

(* (line, col) 에서 명령을 시작해 [units] 단위를 마칠 때까지의 Z80 사이클. *)
let expected_cycles ~kind ~units ~line ~col ~display ~sprites =
  let line = ref line and col = ref (float_of_int col) in
  let total = ref 0.0 and wait = ref start_ticks and left = ref (float_of_int units) in
  while !left > 0.0 do
    let avail = (228.0 -. !col) *. 6.0 in
    let waited = min !wait avail in
    wait := !wait -. waited;
    let working = avail -. waited in
    let cost = ticks_per_unit kind (state_of ~display ~sprites !line) in
    let can = working /. cost in
    if can >= !left then begin
      total := !total +. waited +. (!left *. cost);
      left := 0.0
    end else begin
      total := !total +. avail;
      left := !left -. can;
      col := 0.0;
      line := (!line + 1) mod lines_per_frame
    end
  done;
  !total /. 6.0

(* SCREEN 5. R#9 bit7 = 1 이라 활성 줄이 212 개다. *)
let machine ~display ~sprites =
  let t = Vdp.create () in
  List.iter (fun (r, v) -> set_reg t r v)
    [ (0, 0x06); (1, if display then 0x40 else 0x00); (2, 0x1f); (7, 0);
      (8, if sprites then 0x08 else 0x0a); (9, 0x80) ];
  t

let place t ~line ~col = ignore (Vdp.advance t ~cycles:((line * 228) + col))

(* 같은 단위 수를 내는 명령열. G4 는 바이트당 2점이다. NX 는 9비트라 LINE 의 긴 변은
   511 걸음까지다. *)
let units = 1536
let line_steps = 400
let units_of = function Line -> line_steps | Hmmv | Lmmv | Hmmm | Lmmm | Ymmm -> units

let issue t kind =
  let regs sx sy dx dy nx ny col arg cmd =
    List.iter (fun (r, v) -> set_reg t r v)
      [ (32, sx land 255); (33, sx lsr 8); (34, sy land 255); (35, sy lsr 8);
        (36, dx land 255); (37, dx lsr 8); (38, dy land 255); (39, dy lsr 8);
        (40, nx land 255); (41, nx lsr 8); (42, ny land 255); (43, ny lsr 8);
        (44, col); (45, arg); (46, cmd) ] in
  match kind with
  | Hmmv -> regs 0 0 0 0 256 12 0x55 0 0xc0   (* 128 바이트 x 12 줄 *)
  | Lmmv -> regs 0 0 0 0 128 12 5 0 0x80      (* 128 점 x 12 줄 *)
  | Hmmm -> regs 0 0 0 128 256 12 0 0 0xd0
  | Lmmm -> regs 0 0 0 128 128 12 0 0 0x90
  | Ymmm -> regs 0 0 0 128 0 12 0 0 0xe0      (* dx=0 부터 줄 끝까지 128 바이트 x 12 줄 *)
  | Line -> regs 0 0 0 0 line_steps 40 5 0 0x70

(* 명령을 내린 뒤 CE 가 내려갈 때까지의 사이클(1 사이클씩 센다). *)
let cycles_until_idle t kind =
  issue t kind;
  let rec go n =
    if not (ce t) then n
    else if n > 200_000 then n
    else begin ignore (Vdp.advance t ~cycles:1); go (n + 1) end in
  go 0

let near ~tolerance expected actual =
  Float.abs (float_of_int actual -. expected) <= (tolerance *. expected) +. 6.0

let () =
  (* 1. 여섯 명령 x 세 상태. 활성 영역 맨 앞(줄 0)에서 시작해 모두 줄 211 이전에
        끝나는 크기다. *)
  List.iter (fun kind ->
    List.iter (fun state ->
      let t = machine ~display:(state <> Blank) ~sprites:(state = Sprites) in
      let n = cycles_until_idle t kind in
      let e = expected_cycles ~kind ~units:(units_of kind) ~line:0 ~col:0
          ~display:(state <> Blank) ~sprites:(state = Sprites) in
      check (Printf.sprintf "%s %s takes about %.0f cycles (got %d)"
               (kind_name kind) (state_name state) e n)
        (near ~tolerance:0.01 e n)) states) kinds;
  (* 2. PSET·POINT·SRCH 는 CE 를 세우지 않는다. *)
  List.iter (fun cmd ->
    let t = machine ~display:false ~sprites:false in
    set_reg t 46 cmd;
    check (Printf.sprintf "command %02x finishes at once" cmd) (not (ce t)))
    [ 0x40; 0x50; 0x60 ];
  (* 3. 시작 지연: 두 바이트짜리 HMMV 는 400 + 2 x 49.9 틱 = 약 83 사이클. *)
  let t = machine ~display:false ~sprites:false in
  List.iter (fun (r, v) -> set_reg t r v)
    [ (36, 0); (37, 0); (38, 0); (39, 0); (40, 4); (41, 0); (42, 1); (43, 0); (44, 0x55); (45, 0); (46, 0xc0) ];
  ignore (Vdp.advance t ~cycles:70);
  check "a two byte HMMV still holds CE after 70 cycles" (ce t);
  ignore (Vdp.advance t ~cycles:30);
  check "a two byte HMMV has dropped CE after 100 cycles" (not (ce t));
  (* 4. 시작 위상: 활성 줄 끝 5줄 전, VBlank 맨 앞, 프레임 중간 VBlank 에서 시작. *)
  List.iter (fun (line, col, display, sprites) ->
    List.iter (fun kind ->
      let t = machine ~display ~sprites in
      place t ~line ~col;
      let n = cycles_until_idle t kind in
      let e = expected_cycles ~kind ~units ~line ~col ~display ~sprites in
      check (Printf.sprintf "%s from line %d col %d display=%b sprites=%b: %.0f vs %d"
               (kind_name kind) line col display sprites e n)
        (near ~tolerance:0.012 e n)) [ Hmmv; Lmmm; Ymmm ])
    [ (207, 0, true, false); (207, 100, true, true); (212, 0, true, true);
      (240, 0, true, false); (100, 50, false, false) ];
  (* 5. 명령 도중 R#1 을 켜면 그 줄부터 느려진다. 줄 0 에서 표시 꺼짐으로 시작해
        10000 사이클 뒤에 켠다. *)
  let t = machine ~display:false ~sprites:false in
  issue t Hmmv;
  ignore (Vdp.advance t ~cycles:10_000);
  set_reg t 1 0x40;
  let rec drain n = if not (ce t) then n else begin ignore (Vdp.advance t ~cycles:1); drain (n + 1) end in
  let n = 10_000 + drain 0 in
  let done_units = ((10_000.0 *. 6.0) -. start_ticks) /. ticks_per_unit Hmmv Blank in
  let halfway = 10_000.0 +. ((float_of_int units -. done_units) *. ticks_per_unit Hmmv Display /. 6.0) in
  check (Printf.sprintf "HMMV with the display turned on after 10000 cycles takes %.0f cycles (got %d)" halfway n)
    (near ~tolerance:0.01 halfway n);
  (* 6. STOP 은 CE 를 내리고, 새 명령은 돌던 명령을 대신한다. *)
  let t = machine ~display:false ~sprites:false in
  issue t Hmmv;
  ignore (Vdp.advance t ~cycles:200);
  check "HMMV holds CE after 200 cycles" (ce t);
  set_reg t 46 0x00;
  check "STOP drops CE" (not (ce t));
  issue t Hmmv;
  ignore (Vdp.advance t ~cycles:200);
  let replaced = cycles_until_idle t Lmmv in
  let fresh = expected_cycles ~kind:Lmmv ~units ~line:0 ~col:200 ~display:false ~sprites:false in
  check (Printf.sprintf "a new command replaces the running one: %.0f vs %d" fresh replaced)
    (near ~tolerance:0.01 fresh replaced);
  (* 9. 아주 짧은 HMMV(40 바이트)가 줄 211(마지막 활성 줄)과 줄 212(첫 VBlank 줄)에서
        시작하는 경우. 212 에서는 빈 속도, 211 에서는 첫 줄이 표시 속도다. *)
  List.iter (fun line ->
    let t = machine ~display:true ~sprites:true in
    place t ~line ~col:0;
    List.iter (fun (r, v) -> set_reg t r v)
      [ (36, 0); (37, 0); (38, 0); (39, 0); (40, 80); (41, 0); (42, 1); (43, 0); (44, 0x55); (45, 0); (46, 0xc0) ];
    let rec go n = if not (ce t) then n else begin ignore (Vdp.advance t ~cycles:1); go (n + 1) end in
    let n = go 0 in
    let e = expected_cycles ~kind:Hmmv ~units:40 ~line ~col:0 ~display:true ~sprites:true in
    check (Printf.sprintf "a 40 byte HMMV from line %d takes %.0f cycles (got %d)" line e n)
      (near ~tolerance:0.01 e n)) [ 211; 212 ];
  (* 8. 줄 경계를 넘는 큰 advance 한 번(5000 사이클 = 22 줄)이 활성 줄에서 VBlank 로
        넘어가는 지점을 지난다. 앞 12 줄은 표시 속도, 나머지는 빈 속도다. *)
  let t = machine ~display:true ~sprites:false in
  place t ~line:200 ~col:0;
  issue t Hmmv;
  ignore (Vdp.advance t ~cycles:5_000);
  let rec rest n = if not (ce t) then n else begin ignore (Vdp.advance t ~cycles:1); rest (n + 1) end in
  let total = 5_000 + rest 0 in
  let e = expected_cycles ~kind:Hmmv ~units ~line:200 ~col:0 ~display:true ~sprites:false in
  check (Printf.sprintf "one 5000 cycle advance across the end of the active area: %.0f vs %d" e total)
    (near ~tolerance:0.01 e total);
  (* 7. 스냅샷: 명령 도중 저장해서 복원한 VDP 가 같은 사이클에 끝난다. 20 사이클은
        엔진이 아직 시작 지연(400 틱 = 67 사이클) 안에 있을 때, 5000 사이클은 일하는
        도중이다. *)
  List.iter (fun at ->
    let t = machine ~display:true ~sprites:true in
    issue t Lmmm;
    ignore (Vdp.advance t ~cycles:at);
    let w = State_codec.writer () in
    Vdp.write_state w t;
    let restored = Vdp.create () in
    Vdp.read_state (State_codec.reader (State_codec.finish w)) restored;
    check (Printf.sprintf "a VDP restored after %d cycles still holds CE" at) (ce restored);
    let rec until_idle v n = if not (ce v) then n else begin ignore (Vdp.advance v ~cycles:1); until_idle v (n + 1) end in
    let a = until_idle t 0 and b = until_idle restored 0 in
    check (Printf.sprintf "a VDP restored after %d cycles finishes on the same cycle (%d vs %d)" at a b) (a = b))
    [ 20; 5_000 ];
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "vdp_command_timing_test: all pass"
