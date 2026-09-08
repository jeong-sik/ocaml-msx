(* V9938 palette: first byte 0RRR0BBB, second byte 00000GGG. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let machine : Msx.machine = { ram_kb = 64; vram_kb = 128; roms = [ ""; ""; "" ] }

(* R#N 에 값 쓰기: 0x99 에 값, 그다음 0x80|N. *)
let set_reg m n v =
  Msx.port_out m 0x99 v;
  Msx.port_out m 0x99 (0x80 lor n)

let () =
  let m = Msx.create ~machine in
  set_reg m 16 5;
  Msx.port_out m 0x9A 0x72; (* R=7, B=2 *)
  Msx.port_out m 0x9A 0x03; (* G=3 *)
  let r, g, b = (Msx.palette_entries m).(5) in
  check "byte1 high nibble is R" (r = 255);
  check "byte2 low nibble is G" (g = (3 * 255) / 7);
  check "byte1 low nibble is B" (b = (2 * 255) / 7);

  (* 두 번 쓰면 R#16 이 증가한다 — 다음 색은 인덱스 지정 없이 간다. *)
  Msx.port_out m 0x9A 0x54; (* R=5, B=4 *)
  Msx.port_out m 0x9A 0x05; (* G=5 *)
  let r6, g6, b6 = (Msx.palette_entries m).(6) in
  check "R#16 auto-increments after two writes" (r6 = (5 * 255) / 7);
  check "auto color G" (g6 = (5 * 255) / 7);
  check "auto color B" (b6 = (4 * 255) / 7);

  (* R#16 을 다시 고르면 진행 중이던 첫 바이트는 버린다 — 흘러들면 다음
     색의 채널이 한 칸 어긋난다. *)
  Msx.port_out m 0x9A 0x11; (* 고아가 될 첫 바이트 *)
  set_reg m 16 0;
  Msx.port_out m 0x9A 0x10; (* R=1, B=0 *)
  Msx.port_out m 0x9A 0x02; (* G=2 *)
  let r0, g0, b0 = (Msx.palette_entries m).(0) in
  check "R#16 rewrite discards a half-written color" (r0 = (1 * 255) / 7);
  check "discarded byte does not leak into G" (g0 = (2 * 255) / 7);
  check "discarded byte does not leak into B" (b0 = 0);

  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end;
  print_endline "palette: all checks passed"
