(* 스프라이트 규칙 — VDP 구조체와 무관한 순수 함수. 화면 렌더러와 S#0 플래그
   계산이 같은 줄 선택과 패턴 기하를 쓰도록 한 곳에 둔다.

   모드 1 (SCREEN 1~3): 속성 4바이트(Y, X, 패턴, 컬러+EC), 0xD0 에서 끝, 한
   줄에 4개. 모드 2 (SCREEN 4~7): 속성 4바이트(Y, X, 패턴, 예약), 0xD8 에서
   끝, 한 줄에 8개, 컬러는 패턴 행마다 한 바이트(컬러 테이블).
   맨 윗줄은 Y+1 이고 줄 번호는 256 으로 감긴다. 16×16 패턴은 번호 하위
   2비트를 버리고 왼쪽 반 바이트 0..15, 오른쪽 반 바이트 16..31 이다.
   위치·플래그 규칙은 openMSX 와 같은 최소 ROM 으로 맞췄다. *)

type mode = Mode1 | Mode2

type env = {
  regs : int array;
  vr : int -> int;  (* 논리 VRAM 주소 → 바이트 *)
  mode : mode;
}

type row = {
  index : int;
  x0 : int;  (* 맨 왼쪽 점의 x. EC 로 음수가 될 수 있다. *)
  bits : int;  (* edge 비트, 맨 왼쪽 점이 최상위 비트 *)
  colour : int;
  cc : bool;  (* 모드 2: 번호가 앞선 스프라이트 픽셀과 OR *)
  ic : bool;  (* 모드 2: 충돌 무시 *)
}

let terminator = function Mode1 -> 0xd0 | Mode2 -> 0xd8
let max_per_line = function Mode1 -> 4 | Mode2 -> 8
let size16 e = e.regs.(1) land 0x02 <> 0
let edge e = if size16 e then 16 else 8
let step e = if e.regs.(1) land 0x01 <> 0 then 2 else 1
let height e = edge e * step e

(* 모드 2 속성·컬러 테이블 주소 = (R#11<<15 | R#5<<7 | 0x7F) & (오프셋 | ~0x3FF).
   R#5 의 bit2..0 이 1 이어야 속성(+0x200)·컬러 테이블이 서로 다른 자리에
   선다. 0 이면 그 비트가 오프셋을 지워 두 테이블이 같은 128바이트로 접힌다
   (openMSX VDP::updateSpriteAttributeBase). *)
let table_addr e offset =
  let mask = (e.regs.(11) lsl 15) lor (e.regs.(5) lsl 7) lor 0x7f in
  mask land (offset lor lnot 0x3ff)

let attr_addr e i k =
  match e.mode with
  | Mode1 -> ((e.regs.(5) land 0x7f) lsl 7) + (i * 4) + k
  | Mode2 -> table_addr e (0x200 + (i * 4) + k)

let colour_addr e i row = table_addr e ((i * 16) + row)

let pattern_base e =
  match e.mode with
  | Mode1 -> (e.regs.(6) land 0x07) lsl 11
  | Mode2 -> (e.regs.(6) land 0x3f) lsl 11

(* 속성 테이블에서 목록에 든 번호들과, 목록을 끝낸 번호(종결자가 없으면 32). *)
let entries e =
  let rec go i acc =
    if i = 32 then (List.rev acc, 32)
    else if e.vr (attr_addr e i 0) = terminator e.mode then (List.rev acc, i)
    else go (i + 1) (i :: acc)
  in
  go 0 []

let top e i = (e.vr (attr_addr e i 0) + 1) land 0xff

let row_of e i line =
  let row = ((line - top e i) land 0xff) / step e in
  let cb =
    match e.mode with
    | Mode1 -> e.vr (attr_addr e i 3)
    | Mode2 -> e.vr (colour_addr e i row)
  in
  let pat = e.vr (attr_addr e i 2) in
  let pbase = pattern_base e + ((if size16 e then pat land 0xfc else pat) * 8) in
  let bits =
    if size16 e then (e.vr (pbase + row) lsl 8) lor e.vr (pbase + 16 + row)
    else e.vr (pbase + row)
  in
  {
    index = i;
    x0 = e.vr (attr_addr e i 1) - (if cb land 0x80 <> 0 then 32 else 0);
    bits;
    colour = cb land 0x0f;
    cc = e.mode = Mode2 && cb land 0x40 <> 0;
    ic = e.mode = Mode2 && cb land 0x20 <> 0;
  }

(* [line] 에 걸리는 스프라이트 중 표시되는 앞쪽 몇 개의 행 정보(번호 순)와,
   제한을 넘은 첫 번째 스프라이트(5번째 스프라이트 플래그의 번호). *)
let displayed e indices line =
  let h = height e in
  let on = List.filter (fun i -> (line - top e i) land 0xff < h) indices in
  let limit = max_per_line e.mode in
  let rec take n = function
    | x :: rest when n > 0 -> x :: take (n - 1) rest
    | _ -> []
  in
  (List.map (fun i -> row_of e i line) (take limit on), List.nth_opt on limit)

(* 켜진 점마다 x 를 [f] 에 준다(MAG 로 늘어난 점 포함, 화면 밖도 포함). *)
let iter_dots e r f =
  let edge = edge e and step = step e in
  for b = 0 to edge - 1 do
    if (r.bits lsr (edge - 1 - b)) land 1 = 1 then
      for mx = 0 to step - 1 do
        f (r.x0 + (b * step) + mx)
      done
  done

(* 컬러 0 은 TP(R#8 bit5)=0 이면 투명이고 1 이면 팔레트 0 으로 그려진다. *)
let drawn ~opaque0 r = r.colour <> 0 || opaque0

(* 한 줄의 스프라이트 픽셀. [px] 는 길이 256, 투명은 -1. 번호가 낮은 쪽이
   위고, CC 스프라이트는 번호가 앞선 스프라이트 픽셀과 컬러를 OR 한다. *)
let paint e ~opaque0 rows px =
  Array.fill px 0 (Array.length px) (-1);
  List.iter
    (fun r ->
      if drawn ~opaque0 r then
        iter_dots e r (fun x ->
            if x >= 0 && x < 256 then
              if px.(x) < 0 then px.(x) <- r.colour
              else if r.cc then px.(x) <- px.(x) lor r.colour))
    rows

type scan = {
  collided : bool;  (* S#0 bit5 *)
  fifth : int option;  (* S#0 bit6 와 하위 5비트: 프레임에서 처음 걸린 번호 *)
  stop : int;  (* 목록을 끝낸 번호, 하위 5비트(5번째가 없을 때)는 min 31 *)
}

(* 한 프레임([lines] 줄)의 충돌과 5번째 스프라이트. 충돌은 화면 안(x 0..255)
   에서 겹친 점만 센다. 컬러 0(TP=0)·CC·IC 스프라이트는 충돌하지 않고,
   표시되지 않는 5번째 이후도 충돌하지 않는다. *)
let scan e ~opaque0 ~lines =
  let indices, stop = entries e in
  let seen = Array.make 256 false in
  let collided = ref false and fifth = ref None in
  if indices <> [] then
    for line = 0 to lines - 1 do
      let rows, over = displayed e indices line in
      (match (!fifth, over) with None, Some i -> fifth := Some i | _ -> ());
      if rows <> [] then begin
        Array.fill seen 0 256 false;
        List.iter
          (fun r ->
            if drawn ~opaque0 r && (not r.cc) && not r.ic then
              iter_dots e r (fun x ->
                  if x >= 0 && x < 256 then
                    if seen.(x) then collided := true else seen.(x) <- true))
          rows
      end
    done;
  { collided = !collided; fifth = !fifth; stop }
