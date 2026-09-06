(* Z80 코어. 해석형: opcode 를 (x,y,z) 필드로 해독해 디스패치하고
   접두어(DD/FD/ED/CB)는 재귀로 넘긴다. 플래그는 zexall 이 검사하는
   undocumented 비트(F3/F5)까지 계산한다. 메모리와 포트는 코어 밖
   콜백 — MSX 슬롯/PPI 배선(P1)도 이 경로로 들어온다.
   T-state 는 Z80 매뉴얼 표 값. 인터럽트 주입은 P1 자리. *)

type t = {
  mutable a : int;
  mutable f : int;
  mutable b : int;
  mutable c : int;
  mutable d : int;
  mutable e : int;
  mutable h : int;
  mutable l : int;
  mutable a2 : int;
  mutable f2 : int;
  mutable b2 : int;
  mutable c2 : int;
  mutable d2 : int;
  mutable e2 : int;
  mutable h2 : int;
  mutable l2 : int;
  mutable ix : int;
  mutable iy : int;
  mutable sp : int;
  mutable pc : int;
  mutable i : int;
  mutable r : int;
  mutable iff1 : bool;
  mutable iff2 : bool;
  mutable im : int;
  mutable halted : bool;
  mutable t : int;
  rb : int -> int;
  wb : int -> int -> unit;
  pin : int -> int;
  pout : int -> int -> unit;
}

let m8 x = x land 0xff
let m16 x = x land 0xffff

let create ~read ~write ~port_in ~port_out =
  {
    a = 0; f = 0; b = 0; c = 0; d = 0; e = 0; h = 0; l = 0;
    a2 = 0; f2 = 0; b2 = 0; c2 = 0; d2 = 0; e2 = 0; h2 = 0; l2 = 0;
    ix = 0; iy = 0; sp = 0xffff; pc = 0;
    i = 0; r = 0;
    iff1 = false; iff2 = false; im = 0;
    halted = false;
    t = 0;
    rb = read;
    wb = write;
    pin = port_in;
    pout = port_out;
  }

let halted z = z.halted
let set_pc z v = z.pc <- m16 v
let t_states z = z.t
let dump_pc z = z.pc
let dump_a z = z.a
let set_af z v = z.a <- (v lsr 8) land 0xff; z.f <- v land 0xff
let set_bc z v = z.b <- (v lsr 8) land 0xff; z.c <- v land 0xff
let set_de z v = z.d <- (v lsr 8) land 0xff; z.e <- v land 0xff
let set_hl z v = z.h <- (v lsr 8) land 0xff; z.l <- v land 0xff
let set_sp z v = z.sp <- v land 0xffff
let dump_f z = z.f
let dump_bc z = (z.b lsl 8) lor z.c
let dump_de z = (z.d lsl 8) lor z.e
let dump_hl z = (z.h lsl 8) lor z.l
let dump_ix z = z.ix
let dump_iy z = z.iy
let dump_sp z = z.sp

(* 한 step 의 T-state 카운터. *)
let dt = ref 0
let add_t n = dt := !dt + n

let fetch z =
  let v = z.rb z.pc in
  z.pc <- m16 (z.pc + 1);
  z.r <- (z.r land 0x80) lor ((z.r + 1) land 0x7f);
  v

let imm8 z = fetch z

let imm16 z =
  let lo = fetch z in
  let hi = fetch z in
  (hi lsl 8) lor lo

let disp8 z =
  let d = imm8 z in
  if d >= 0x80 then d - 256 else d

let rd16 z addr = (z.rb (m16 (addr + 1)) lsl 8) lor z.rb addr

let push z v =
  (* 하위 바이트가 낮은 주소에 — Z80 16비트 메모리 규약 (little-endian). *)
  z.sp <- m16 (z.sp - 2);
  z.wb z.sp (v land 0xff);
  z.wb (m16 (z.sp + 1)) (v lsr 8)

let pop z =
  let lo = z.rb z.sp in
  let hi = z.rb (m16 (z.sp + 1)) in
  z.sp <- m16 (z.sp + 2);
  (hi lsl 8) lor lo

let parity v =
  let n = ref (v land 0xff) in
  let p = ref 0 in
  while !n <> 0 do
    p := !p + (!n land 1);
    n := !n lsr 1
  done;
  if (!p land 1) = 0 then 0x04 else 0

let sz53 r = (r land 0xa8) lor (if r = 0 then 0x40 else 0)

type idx = No | IXp | IYp

let idx_reg z = function IXp -> z.ix | IYp -> z.iy | No -> assert false

(* (HL) 계열 피연산자의 실효 주소. 접두어가 있으면 변위 fetch 가
   일어난다 — 호출 순서가 인코딩 순서와 같아야 한다. *)
let ea z = function
  | No -> (z.h lsl 8) lor z.l
  | p -> m16 (idx_reg z p + disp8 z)

let hl16 z = (z.h lsl 8) lor z.l

let cond z = function
  | 0 -> z.f land 0x40 = 0
  | 1 -> z.f land 0x40 <> 0
  | 2 -> z.f land 0x01 = 0
  | 3 -> z.f land 0x01 <> 0
  | 4 -> z.f land 0x04 = 0
  | 5 -> z.f land 0x04 <> 0
  | 6 -> z.f land 0x80 = 0
  | 7 -> z.f land 0x80 <> 0
  | _ -> assert false

let rget z i p =
  match i with
  | 0 -> z.b
  | 1 -> z.c
  | 2 -> z.d
  | 3 -> z.e
  | 4 -> z.h
  | 5 -> z.l
  | 6 -> z.rb (ea z p)
  | 7 -> z.a
  | _ -> assert false

let rset z i v p =
  match i with
  | 0 -> z.b <- v
  | 1 -> z.c <- v
  | 2 -> z.d <- v
  | 3 -> z.e <- v
  | 4 -> z.h <- v
  | 5 -> z.l <- v
  | 6 -> z.wb (ea z p) v
  | 7 -> z.a <- v
  | _ -> assert false

let pget z i p =
  match i with
  | 0 -> (z.b lsl 8) lor z.c
  | 1 -> (z.d lsl 8) lor z.e
  | 2 -> (match p with No -> hl16 z | q -> idx_reg z q)
  | 3 -> z.sp
  | _ -> assert false

let pset z i v p =
  let hi = v lsr 8 and lo = v land 0xff in
  match i with
  | 0 -> z.b <- hi; z.c <- lo
  | 1 -> z.d <- hi; z.e <- lo
  | 2 ->
    (match p with
     | No -> z.h <- hi; z.l <- lo
     | IXp -> z.ix <- v
     | IYp -> z.iy <- v)
  | 3 -> z.sp <- v
  | _ -> assert false

let alu_add z v carry =
  let c0 = if carry then z.f land 1 else 0 in
  let sum = z.a + v + c0 in
  let r = m8 sum in
  let cc = if sum > 0xff then 1 else 0 in
  let hh = if ((z.a land 15) + (v land 15) + c0) > 15 then 0x10 else 0 in
  let pv = if ((lnot (z.a lxor v)) land (z.a lxor r) land 0x80) <> 0 then 0x04 else 0 in
  z.f <- sz53 r lor hh lor pv lor cc;
  z.a <- r

let alu_sub z v carry =
  let c0 = if carry then z.f land 1 else 0 in
  let diff = z.a - v - c0 in
  let r = m8 diff in
  let cc = if diff < 0 then 1 else 0 in
  let hh = if ((z.a land 15) - ((v land 15) + c0)) < 0 then 0x10 else 0 in
  let pv = if ((z.a lxor v) land (z.a lxor r) land 0x80) <> 0 then 0x04 else 0 in
  z.f <- sz53 r lor 0x02 lor hh lor pv lor cc;
  z.a <- r

let alu_and z v =
  let r = z.a land v in
  z.f <- sz53 r lor parity r lor 0x10;
  z.a <- r

let alu_xor z v =
  let r = z.a lxor v in
  z.f <- sz53 r lor parity r;
  z.a <- r

let alu_or z v =
  let r = z.a lor v in
  z.f <- sz53 r lor parity r;
  z.a <- r

let alu_cp z v =
  let diff = z.a - v in
  let r = m8 diff in
  let cc = if diff < 0 then 1 else 0 in
  let hh = if ((z.a land 15) - (v land 15)) < 0 then 0x10 else 0 in
  let pv = if ((z.a lxor v) land (z.a lxor r) land 0x80) <> 0 then 0x04 else 0 in
  (* CP 특이사항: S/Z 는 결과에서 오지만 F3/F5 는 피연산자에서 온다. *)
  z.f <- (r land 0x80) lor (if r = 0 then 0x40 else 0) lor (v land 0x28)
         lor 0x02 lor hh lor pv lor cc

let inc8 v f =
  let r = m8 (v + 1) in
  let hh = if (v land 15) = 15 then 0x10 else 0 in
  let pv = if v = 0x7f then 0x04 else 0 in
  (r, (f land 1) lor sz53 r lor hh lor pv)

let dec8 v f =
  let r = m8 (v - 1) in
  let hh = if (v land 15) = 0 then 0x10 else 0 in
  let pv = if v = 0x80 then 0x04 else 0 in
  (r, (f land 1) lor sz53 r lor 0x02 lor hh lor pv)

let add16 z dst src adc =
  let c0 = if adc then z.f land 1 else 0 in
  let sum = dst + src + c0 in
  let r = m16 sum in
  let cc = if sum > 0xffff then 1 else 0 in
  let hh = if ((dst land 0xfff) + (src land 0xfff) + c0) > 0xfff then 0x10 else 0 in
  let f35 = (r lsr 8) land 0x28 in
  (* ADC HL: S,Z,PV 는 16비트 결과에서 — PV 는 부호있는 오버플로,
     패리티가 아니다. *)
  let rest =
    if adc
    then
      sz53 (r lsr 8)
      lor
      (if ((lnot (dst lxor src)) land (dst lxor r) land 0x8000) <> 0
       then 0x04 else 0)
    else z.f land (0x80 lor 0x40 lor 0x04)
  in
  (* S,Z,PV 불변이지 C 불변이 아니다 — C 는 bit15 자리올림으로 갱신. *)
  z.f <- f35 lor hh lor rest lor cc;
  r

let sbc16 z dst src sbc =
  let c0 = if sbc then z.f land 1 else 0 in
  let diff = dst - src - c0 in
  let r = m16 diff in
  let cc = if diff < 0 then 1 else 0 in
  let hh = if ((dst land 0xfff) - ((src land 0xfff) + c0)) < 0 then 0x10 else 0 in
  let f35 = (r lsr 8) land 0x28 in
  let pv =
    if ((dst lxor src) land (dst lxor r) land 0x8000) <> 0 then 0x04 else 0
  in
  z.f <- sz53 (r lsr 8) lor pv lor 0x02 lor hh lor f35 lor cc;
  r

let rot op v c0 =
  match op with
  | 0 -> (((v lsl 1) land 0xff) lor (v lsr 7), v lsr 7)
  | 1 -> ((v lsr 1) lor ((v land 1) lsl 7), v land 1)
  | 2 -> (((v lsl 1) land 0xff) lor c0, v lsr 7)
  | 3 -> ((v lsr 1) lor (c0 lsl 7), v land 1)
  | 4 -> ((v lsl 1) land 0xff, v lsr 7)
  | 5 -> ((v lsr 1) lor (v land 0x80), v land 1)
  | 6 -> (((v lsl 1) land 0xff) lor 1, v land 1)
  | 7 -> (v lsr 1, v land 1)
  | _ -> assert false

let rot_flags r c = sz53 r lor parity r lor c

let daa z =
  let a0 = z.a and f0 = z.f in
  let n_sub = f0 land 2 <> 0 and h_in = f0 land 0x10 <> 0 and c_in = f0 land 1 <> 0 in
  let corr = ref 0 in
  let c_out = ref c_in in
  if h_in || ((a0 land 0x0f) > 9) then corr := !corr lor 0x06;
  if c_in || (a0 > 0x99) then begin corr := !corr lor 0x60; c_out := true end;
  let a1 = m8 (if n_sub then a0 - !corr else a0 + !corr) in
  let h_out = ((a0 lxor a1 lxor !corr) land 0x10) <> 0 in
  z.a <- a1;
  z.f <- sz53 a1 lor parity a1 lor (f0 land 0x02)
         lor (if h_out then 0x10 else 0) lor (if !c_out then 1 else 0)

(* INI/OUTI 계열: Z(내려간 B) 와 N 만 바뀐다. S/F5/F3/H/PV/C 불변 —
   superzazu/z80.c 와 같은 선택, 그 코어로 zexall 을 통과했다. *)
let io_block_flags z =
  z.f <- (z.f land 0xfd) lor 0x02 lor (if z.b = 0 then 0x40 else 0)

let rec exec z op p =
  match op with
  | 0xDD -> exec z (fetch z) IXp
  | 0xFD -> exec z (fetch z) IYp
  | 0xCB -> cb_exec z (fetch z) p
  | 0xED -> ed_exec z (fetch z)
  | _ -> main_exec z op p

and cb_exec z op p =
  let x = op lsr 6 and y = (op lsr 3) land 7 and r = op land 7 in
  (* DDCB/FDCB: 변위를 먼저 읹고 (IX+d) 를 대상으로. 결과는 r≠6 이면
     그 레지스터에도 복사된다. *)
  let addr =
    match p with
    | No -> None
    | q -> Some (m16 (idx_reg z q + disp8 z))
  in
  let base = match addr with Some a -> a | None -> ea z No in
  let v = z.rb base in
  if x = 1 then begin
    let t = v land (1 lsl y) in
    let zf = if t = 0 then 0x40 else 0 in
    let sf = if y = 7 && t <> 0 then 0x80 else 0 in
    let f53 =
      match addr with
      | Some a -> (a lsr 8) land 0x28
      | None -> v land 0x28
    in
    z.f <- (z.f land 1) lor f53 lor 0x12 lor zf lor sf lor (zf lsr 4);
    add_t (match addr with Some _ -> 20 | None -> if r = 6 then 12 else 8)
  end
  else begin
    let v' =
      if x = 2 then v land (lnot (1 lsl y))
      else if x = 3 then v lor (1 lsl y)
      else
        let nv, c = rot y v (z.f land 1) in
        z.f <- rot_flags nv c;
        nv
    in
    z.wb base v';
    (match addr with
     | Some _ -> if r <> 6 then rset z r v' No
     | None -> if r <> 6 then rset z r v' No);
    add_t (match addr with Some _ -> 23 | None -> if r = 6 then 15 else 8)
  end

and ed_exec z op =
  let y = (op lsr 3) land 7 in
  match op with
  | 0x40 | 0x48 | 0x50 | 0x58 | 0x60 | 0x68 | 0x70 | 0x78 ->
    (* IN r,(C) — 0x70 은 IN F,(C): 플래그만 갱신. *)
    let v = z.pin ((z.b lsl 8) lor z.c) in
    z.f <- sz53 v lor parity v;
    if y <> 6 then rset z y v No;
    add_t 12
  | 0x41 | 0x49 | 0x51 | 0x59 | 0x61 | 0x69 | 0x71 | 0x79 ->
    z.pout ((z.b lsl 8) lor z.c)
      (if op = 0x71 then 0 else rget z y No);
    add_t 12
  | 0x42 | 0x52 | 0x62 | 0x72 ->
    let src = pget z ((op lsr 4) land 3) No in
    pset z 2 (sbc16 z (pget z 2 No) src true) No;
    add_t 15
  | 0x4A | 0x5A | 0x6A | 0x7A ->
    let src = pget z ((op lsr 4) land 3) No in
    pset z 2 (add16 z (pget z 2 No) src true) No;
    add_t 15
  | 0x43 | 0x53 | 0x63 | 0x73 ->
    let a = imm16 z in
    let v = pget z ((op lsr 4) land 3) No in
    z.wb a (v land 0xff);
    z.wb (m16 (a + 1)) (v lsr 8);
    add_t 20
  | 0x4B | 0x5B | 0x6B | 0x7B ->
    (* LD rr,(nn) — 저장(0x43 계열)의 읽기 방향. *)
    let a = imm16 z in
    pset z ((op lsr 4) land 3) (rd16 z a) No;
    add_t 20
  | 0x44 ->
    (* NEG *)
    let a0 = z.a in
    let r = m8 (-a0) in
    z.f <- sz53 r lor 0x02
           lor (if a0 <> 0 then 1 else 0)
           lor (if (a0 land 15) <> 0 then 0x10 else 0)
           lor (if a0 = 0x80 then 0x04 else 0);
    z.a <- r;
    add_t 8
  | 0x45 | 0x55 | 0x5D | 0x65 | 0x6D | 0x75 | 0x7D ->
    z.pc <- pop z;
    add_t 14
  | 0x46 -> z.im <- 0; add_t 8
  | 0x4E | 0x66 | 0x6E -> z.im <- 2; add_t 8
  | 0x56 | 0x76 | 0x7E -> z.im <- 1; add_t 8
  | 0x47 -> z.i <- z.a; add_t 9
  | 0x4F -> z.r <- z.a; add_t 9
  | 0x57 ->
    z.f <- sz53 z.i lor (if z.iff2 then 0x04 else 0);
    z.a <- z.i;
    add_t 9
  | 0x5F ->
    z.f <- sz53 z.r lor (if z.iff2 then 0x04 else 0);
    z.a <- z.r;
    add_t 9
  | 0x67 | 0x6F ->
    let addr = hl16 z in
    let v = z.rb addr in
    let a' =
      if op = 0x67 then (z.a land 0xf0) lor (v lsr 4)
      else (z.a land 0xf0) lor (v land 0x0f)
    in
    let v' =
      if op = 0x67 then ((v lsl 4) land 0xff) lor (z.a land 0x0f)
      else (v lsr 4) lor ((z.a land 0x0f) lsl 4)
    in
    z.wb addr v';
    z.a <- a';
    z.f <- sz53 a' lor parity a' lor (z.f land 1);
    add_t 18
  | 0xA0 | 0xA8 | 0xB0 | 0xB8 ->
    let src = hl16 z in
    let dst = (z.d lsl 8) lor z.e in
    let value = z.rb src in
    z.wb dst value;
    let bc = ((z.b lsl 8) lor z.c) - 1 in
    z.b <- bc lsr 8;
    z.c <- bc land 0xff;
    let dir = if op land 0x08 = 0 then 1 else -1 in
    let hl' = m16 (src + dir) and de' = m16 (dst + dir) in
    z.h <- hl' lsr 8;
    z.l <- hl' land 0xff;
    z.d <- de' lsr 8;
    z.e <- de' land 0xff;
    (* F3/F5 = bit3/bit1 of (읽은 값 + A) — wikiti Z80_Instruction_Set *)
    let sum = value + z.a in
    z.f <- (z.f land (0x01 lor 0x80 lor 0x40))
           lor ((sum land 2) lsl 4) lor (sum land 0x08)
           lor (if bc <> 0 then 0x04 else 0);
    if op >= 0xB0 && bc <> 0 then begin
      z.pc <- m16 (z.pc - 2);
      add_t 21
    end else add_t 16
  | 0xA1 | 0xA9 | 0xB1 | 0xB9 ->
    let addr = hl16 z in
    let v = z.rb addr in
    let res = m8 (z.a - v) in
    let hh = if ((z.a land 15) - (v land 15)) < 0 then 0x10 else 0 in
    let k = res - (hh lsr 4) in
    let bc = ((z.b lsl 8) lor z.c) - 1 in
    z.b <- bc lsr 8;
    z.c <- bc land 0xff;
    let dir = if op land 0x08 = 0 then 1 else -1 in
    let hl' = m16 (addr + dir) in
    z.h <- hl' lsr 8;
    z.l <- hl' land 0xff;
    z.f <- (z.f land 0x01)
           lor ((if res = 0 then 0x40 else 0) lor (res land 0x80))
           lor 0x02
           lor hh
           lor ((k land 2) lsl 3)
           lor (k land 0x08)
           lor (if bc <> 0 then 0x04 else 0);
    if op >= 0xB1 && bc <> 0 && res <> 0 then begin
      z.pc <- m16 (z.pc - 2);
      add_t 21
    end else add_t 16
  | 0xA2 | 0xAA | 0xB2 | 0xBA ->
    let addr = hl16 z in
    let port = (z.b lsl 8) lor z.c in
    let v = z.pin port in
    z.wb addr v;
    z.b <- m8 (z.b - 1);
    io_block_flags z;
    let dir = if op land 0x08 = 0 then 1 else -1 in
    let hl' = m16 (addr + dir) in
    z.h <- hl' lsr 8;
    z.l <- hl' land 0xff;
    if op >= 0xB2 && z.b <> 0 then begin
      z.pc <- m16 (z.pc - 2);
      add_t 21
    end else add_t 16
  | 0xA3 | 0xAB | 0xB3 | 0xBB ->
    let addr = hl16 z in
    let port = (z.b lsl 8) lor z.c in
    let v = z.rb addr in
    z.b <- m8 (z.b - 1);
    z.pout port v;
    io_block_flags z;
    let dir = if op land 0x08 = 0 then 1 else -1 in
    let hl' = m16 (addr + dir) in
    z.h <- hl' lsr 8;
    z.l <- hl' land 0xff;
    if op >= 0xB3 && z.b <> 0 then begin
      z.pc <- m16 (z.pc - 2);
      add_t 21
    end else add_t 16
  | _ ->
    add_t 8

and main_exec z op p =
  let y = (op lsr 3) land 7 and w = op land 7 in
  let x = op lsr 6 in
  if x = 1 then begin
    if y = 6 && w = 6 then begin z.halted <- true; add_t 4 end
    else begin
      let v = rget z w p in
      rset z y v p;
      add_t
        (match p with
         | No -> if y = 6 || w = 6 then 7 else 4
         | _ -> if y = 6 || w = 6 then 19 else 4)
    end
  end
  else if x = 2 then begin
    let v = rget z w p in
    (match y with
     | 0 -> alu_add z v false
     | 1 -> alu_add z v true
     | 2 -> alu_sub z v false
     | 3 -> alu_sub z v true
     | 4 -> alu_and z v
     | 5 -> alu_xor z v
     | 6 -> alu_or z v
     | _ -> alu_cp z v);
    add_t (match p with No -> if w = 6 then 7 else 4 | _ -> if w = 6 then 19 else 4)
  end
  else
    match op with
    | 0x00 -> add_t 4
    | 0x10 ->
      let d = disp8 z in
      z.b <- m8 (z.b - 1);
      if z.b <> 0 then begin
        z.pc <- m16 (z.pc + d);
        add_t 13
      end else add_t 8
    | 0x18 ->
      let d = disp8 z in
      z.pc <- m16 (z.pc + d);
      add_t 12
    | 0x20 | 0x28 | 0x30 | 0x38 ->
      let d = disp8 z in
      if cond z (y - 4) then begin
        z.pc <- m16 (z.pc + d);
        add_t 12
      end else add_t 8
    | 0x01 | 0x11 | 0x21 | 0x31 ->
      pset z (op lsr 4) (imm16 z) p;
      add_t (if op = 0x21 && p <> No then 14 else 10)
    | 0x09 | 0x19 | 0x29 | 0x39 ->
      pset z 2 (add16 z (pget z 2 p) (pget z (op lsr 4) p) false) p;
      add_t (if p <> No then 15 else 11)
    | 0x02 -> z.wb ((z.b lsl 8) lor z.c) z.a; add_t 7
    | 0x12 -> z.wb ((z.d lsl 8) lor z.e) z.a; add_t 7
    | 0x22 ->
      let a = imm16 z in
      let v = pget z 2 p in
      z.wb a (v land 0xff);
      z.wb (m16 (a + 1)) (v lsr 8);
      add_t (if p <> No then 20 else 16)
    | 0x32 ->
      let a = imm16 z in
      z.wb a z.a;
      add_t 13
    | 0x0A -> z.a <- z.rb ((z.b lsl 8) lor z.c); add_t 7
    | 0x1A -> z.a <- z.rb ((z.d lsl 8) lor z.e); add_t 7
    | 0x2A ->
      let a = imm16 z in
      pset z 2 (rd16 z a) p;
      add_t (if p <> No then 20 else 16)
    | 0x3A ->
      let a = imm16 z in
      z.a <- z.rb a;
      add_t 13
    | 0x03 | 0x13 | 0x23 | 0x33 ->
      let i = op lsr 4 in
      pset z i (m16 (pget z i p + 1)) p;
      add_t (if i = 2 && p <> No then 10 else 6)
    | 0x0B | 0x1B | 0x2B | 0x3B ->
      let i = op lsr 4 in
      pset z i (m16 (pget z i p - 1)) p;
      add_t (if i = 2 && p <> No then 10 else 6)
    | 0x04 | 0x0C | 0x14 | 0x1C | 0x24 | 0x2C | 0x34 | 0x3C ->
      let v = rget z y p in
      let nv, nf = inc8 v z.f in
      rset z y nv p;
      z.f <- nf;
      add_t
        (if y = 6 then (if p <> No then 23 else 11) else 4)
    | 0x05 | 0x0D | 0x15 | 0x1D | 0x25 | 0x2D | 0x35 | 0x3D ->
      let v = rget z y p in
      let nv, nf = dec8 v z.f in
      rset z y nv p;
      z.f <- nf;
      add_t
        (if y = 6 then (if p <> No then 23 else 11) else 4)
    | 0x06 | 0x0E | 0x16 | 0x1E | 0x26 | 0x2E | 0x3E ->
      rset z y (imm8 z) p;
      add_t (if p <> No then 10 else 7)
    | 0x36 ->
      (* LD (HL),n / LD (IX+d),n: 변위를 먼저 읹고 그 다음 데이터. *)
      (match p with
       | No -> z.wb (hl16 z) (imm8 z)
       | q ->
         let a = ea z q in
         z.wb a (imm8 z));
      add_t (if p <> No then 19 else 10)
    | 0x07 | 0x0F | 0x17 | 0x1F ->
      (* S,Z,PV 불변. F3/F5 는 결과의 bit3/5 로, H=N=0, C=회전비트. *)
      let v, c = rot ((op lsr 3) land 3) z.a (z.f land 1) in
      z.f <- (z.f land 0xc4) lor (v land 0x28) lor c;
      z.a <- v;
      add_t 4
    | 0x27 -> daa z; add_t 4
    | 0x2F ->
      z.a <- m8 (lnot z.a);
      z.f <- (z.f land 0xc5) lor 0x12 lor (z.a land 0x28);
      add_t 4
    | 0x37 -> z.f <- (z.f land 0xc5) lor (z.a land 0x28) lor 1; add_t 4
    | 0x3F ->
      let c = z.f land 1 in
      z.f <- (z.f land 0xc5) lor (z.a land 0x28) lor c lor (if c = 1 then 0x10 else 0);
      add_t 4
    | 0xC0 | 0xC8 | 0xD0 | 0xD8 | 0xE0 | 0xE8 | 0xF0 | 0xF8 ->
      if cond z y then begin
        z.pc <- pop z;
        add_t 11
      end else add_t 5
    | 0xC1 | 0xD1 | 0xE1 ->
      pset z ((op lsr 4) land 3) (pop z) p;
      add_t (if op = 0xE1 && p <> No then 14 else 10)
    | 0xC5 | 0xD5 | 0xE5 ->
      push z (pget z ((op lsr 4) land 3) p);
      add_t (if op = 0xE5 && p <> No then 15 else 11)
    (* AF 는 pget/pset 의 짝이 아니다 — 전용. *)
    | 0xF1 ->
      let v = pop z in
      z.a <- v lsr 8;
      z.f <- v land 0xff;
      add_t 10
    | 0xF5 ->
      push z ((z.a lsl 8) lor z.f);
      add_t 11
    | 0xC2 | 0xCA | 0xD2 | 0xDA | 0xE2 | 0xEA | 0xF2 | 0xFA ->
      let a = imm16 z in
      if cond z y then z.pc <- a;
      add_t 10
    | 0xC3 ->
      z.pc <- imm16 z;
      add_t 10
    | 0xC6 -> alu_add z (imm8 z) false; add_t 7
    | 0xCE -> alu_add z (imm8 z) true; add_t 7
    | 0xD6 -> alu_sub z (imm8 z) false; add_t 7
    | 0xDE -> alu_sub z (imm8 z) true; add_t 7
    | 0xE6 -> alu_and z (imm8 z); add_t 7
    | 0xEE -> alu_xor z (imm8 z); add_t 7
    | 0xF6 -> alu_or z (imm8 z); add_t 7
    | 0xFE -> alu_cp z (imm8 z); add_t 7
    | 0xC9 -> z.pc <- pop z; add_t 10
    | 0xCD ->
      let a = imm16 z in
      push z z.pc;
      z.pc <- a;
      add_t 17
    | 0xC4 | 0xCC | 0xD4 | 0xDC | 0xE4 | 0xEC | 0xF4 | 0xFC ->
      let a = imm16 z in
      if cond z y then begin
        push z z.pc;
        z.pc <- a;
        add_t 17
      end else add_t 10
    | 0xD3 ->
      let port = imm8 z in
      z.pout port z.a;
      add_t 11
    | 0xDB ->
      let port = imm8 z in
      z.a <- z.pin port;
      add_t 11
    | 0xD9 ->
      let t = z.b in z.b <- z.b2; z.b2 <- t;
      let t = z.c in z.c <- z.c2; z.c2 <- t;
      let t = z.d in z.d <- z.d2; z.d2 <- t;
      let t = z.e in z.e <- z.e2; z.e2 <- t;
      let t = z.h in z.h <- z.h2; z.h2 <- t;
      let t = z.l in z.l <- z.l2; z.l2 <- t;
      add_t 4
    | 0xE3 ->
      let spv = rd16 z z.sp in
      let v = pget z 2 p in
      z.wb z.sp (v land 0xff);
      z.wb (m16 (z.sp + 1)) (v lsr 8);
      pset z 2 spv p;
      add_t (if p <> No then 23 else 19)
    | 0xE9 -> z.pc <- pget z 2 p; add_t (if p <> No then 14 else 4)
    | 0xEB ->
      let de = (z.d lsl 8) lor z.e and hl = hl16 z in
      z.d <- hl lsr 8;
      z.e <- hl land 0xff;
      z.h <- de lsr 8;
      z.l <- de land 0xff;
      add_t 4
    | 0xF3 -> z.iff1 <- false; z.iff2 <- false; add_t 4
    | 0xFB -> z.iff1 <- true; z.iff2 <- true; add_t 4
    | 0xF9 -> z.sp <- pget z 2 p; add_t (if p <> No then 10 else 6)
    | 0xC7 | 0xCF | 0xD7 | 0xDF | 0xE7 | 0xEF | 0xF7 | 0xFF ->
      push z z.pc;
      z.pc <- op land 0x38;
      add_t 11
    | _ -> add_t 4

let step z =
  dt := 0;
  if z.halted then begin z.t <- z.t + 4; 4 end
  else begin
    let op = fetch z in
    exec z op No;
    z.t <- z.t + !dt;
    !dt
  end
