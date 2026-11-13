(* Port 0x99 takes a register or address write as two bytes. openMSX
   VDP::writeIO and VDP::readIO drop a first byte left waiting when the CPU
   writes port 0x98 or reads port 0x98 or 0x99; the next 0x99 byte starts a
   new pair. Penguin-Kun Wars 2 writes one stray 0x99 byte, then VRAM data,
   then its register pairs: kept, the stray byte shifts every later pair. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let pair t lo hi =
  Vdp.io_write t ~port:0x99 lo;
  Vdp.io_write t ~port:0x99 hi

(* A stray first byte, then [between], then R#7 = 0x35 as a pair. *)
let r7_after between =
  let t = Vdp.create () in
  Vdp.io_write t ~port:0x99 0x55;
  between t;
  pair t 0x35 0x87;
  (Vdp.regs t).(7)

let () =
  check "a 0x98 write drops the waiting byte"
    (r7_after (fun t -> Vdp.io_write t ~port:0x98 0x11) = 0x35);
  check "a 0x98 read drops the waiting byte"
    (r7_after (fun t -> ignore (Vdp.io_read t ~port:0x98)) = 0x35);
  check "a 0x99 read drops the waiting byte"
    (r7_after (fun t -> ignore (Vdp.io_read t ~port:0x99)) = 0x35);
  (* With nothing between, the stray byte pairs with 0x35 (an address
     write, bit 7 clear) and 0x87 waits as a first byte: R#7 stays 0. *)
  check "without a drop the stray byte takes the next one"
    (r7_after (fun _ -> ()) = 0);

  (* An address write after a drop lands where its own two bytes say:
     0x1234, not 0x12 followed by the stray byte. *)
  let t = Vdp.create () in
  Vdp.io_write t ~port:0x99 0x55;
  Vdp.io_write t ~port:0x98 0x00;
  pair t 0x34 0x52;
  Vdp.io_write t ~port:0x98 0xa7;
  check "the address pair after a drop" (Vdp.vram_read t 0x1234 = 0xa7);

  (* A complete pair is not touched by the data port: the write after it
     goes to the address it set, and the next 0x99 byte is a first byte. *)
  let t = Vdp.create () in
  pair t 0x00 0x60;
  Vdp.io_write t ~port:0x98 0x5a;
  pair t 0x0c 0x87;
  check "data after a complete pair" (Vdp.vram_read t 0x2000 = 0x5a);
  check "a pair after data" ((Vdp.regs t).(7) = 0x0c);

  if !failures > 0 then begin
    Printf.eprintf "vdp_port_latch_test: %d failure(s)\n" !failures;
    exit 1
  end;
  print_endline "vdp_port_latch_test: all passed"
