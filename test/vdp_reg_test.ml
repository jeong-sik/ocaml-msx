(* Register writes the V9938 does not have. openMSX VDP::changeRegister
   drops a write to R#47-R#63, which the indirect port 0x9B reaches when
   R#17 points past R#46 (Return of Jelda and Blade Lords do; we raised
   Invalid_argument on the 47-entry register array). VDP::writeIO treats a
   second 0x99 byte with bits 7 and 6 set (0xC0-0xFF) as neither a
   register write nor an address. Everything through the I/O ports. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let set_reg t r v =
  Vdp.io_write t ~port:0x99 v;
  Vdp.io_write t ~port:0x99 (0x80 lor r)

let () =
  let t = Vdp.create () in
  (* R#17 = 46 with auto-increment: the indirect writes land on R#46 (a
     command register; 0 is STOP), then R#47-R#63 (dropped), then wrap to
     R#0. *)
  set_reg t 17 46;
  let before = Array.copy (Vdp.regs t) in
  let wrote =
    match List.iter (fun v -> Vdp.io_write t ~port:0x9B v) (List.init 19 (fun i -> i)) with
    | () -> true
    | exception Invalid_argument _ -> false
  in
  check "indirect writes past R#46 do not raise" wrote;
  let after = Vdp.regs t in
  check "R#46 took the first indirect byte" (after.(46) = 0);
  check "R#0 took the byte after the wrap" (after.(0) = 18);
  check "R#17 wrapped to 1" (after.(17) = 1);
  Array.iteri
    (fun r v ->
      if r <> 0 && r <> 17 && r <> 46 then
        check (Printf.sprintf "R#%d untouched by the dropped writes" r) (after.(r) = v))
    before;
  check "the register file stays 47 entries" (Array.length after = 47);

  (* 0x99: value, then 0xC0 | 5. Not a register write, not an address. *)
  let t = Vdp.create () in
  set_reg t 5 0x36;
  Vdp.io_write t ~port:0x99 0x12;
  Vdp.io_write t ~port:0x99 0xc5;
  check "0xC5 does not write R#5" ((Vdp.regs t).(5) = 0x36);
  (* the latch is spent: the next pair is a fresh register write *)
  set_reg t 7 0xf1;
  check "the next pair writes R#7" ((Vdp.regs t).(7) = 0xf1);
  if !failures > 0 then exit 1;
  print_endline "vdp_reg_test: all pass"
