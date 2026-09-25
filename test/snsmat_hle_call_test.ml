(* Regression for task-1762 (Sangokushi II second-unit deployment hang): a
   fifth path to the same disease as task-1564's CHSNS/CHGET fix.

   Live symptom: multi-unit deployment (206年10月, checkpoint
   retro-mania-failure-206-10-second-deploy-v037) accepts the *first*
   unit's "0" (deploy) key but the *second* unit's identical prompt ignores
   every key -- 0/Return/Space/direction, hold 5..300 frames -- forever.
   RAM diff shows the key byte DOES land in the input buffer
   (0xFC14/0xFC2B/0xFC0A), so BIOS CHGET (fixed by task-1564/PR #40) still
   runs; the prompt's own accept check never latches it.

   Static trace (dump of live PC region 0xD900-0xDC00, checkpoint above):
   the placement/battle-direction prompt polls through 0xDB10 -- a routine
   that, right before it reaches the task-1564 CHSNS trampoline at 0xDB1F
   (`CALL 0x009C`), first calls 0xDAE2/0xDAF9. Those read the keyboard
   matrix with a *direct* `LD A,<row>; LD H,0x40; CALL 0x0024` -- BIOS
   SNSMAT (Sense Keyboard Matrix Row) -- never through the IX-patched
   trampoline task-1564 fixed, so it sat outside both the CHSNS-only trap
   and the four-vector (0x009C/0x009F/0x00A2/0x0156) trap alike. With page
   0 mapped to RAM (the DOS-kernel-replay shape), that CALL runs RAM
   power-on garbage instead of the real BIOS row-sense routine: the accept
   check reads a garbage row value, never sees the pressed bit, and the
   prompt sits forever even though CHGET already delivered the byte to the
   buffer upstream.

   Each scenario mirrors chsns_hle_call_test.ml's shape exactly (fresh
   machine per scenario, same BIOS-style page-0 vector table, same
   DOS-kernel-replay wiring) and proves what the fix in [Msx.disk_trap]
   (adding 0x0024 to the served-vector match) is responsible for:
   1. a plain CALL 0x0024 reaches main_rom's SNSMAT stub (the marker lands
      and its A result comes home -- this is the row-sense value the
      deployment prompt's accept check reads);
   2. ppi_a is restored afterward (page 0 goes back to RAM for the rest of
      the kernel);
   3. the CHSNS trampoline (0x009C, task-1564's fix) still works
      side-by-side with the new SNSMAT vector in the same scenario -- the
      two traps do not interfere;
   4. the vector whitelist still excludes non-BIOS addresses (0x0024 is a
      narrow addition, not a blanket page-0 CALL passthrough). *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

(* Same page-0 shape as chsns_hle_call_test.ml, extended with a SNSMAT
   vector/body at 0x0024. *)
let build_main_rom () =
  let main = Bytes.make 0x4000 '\000' in
  (* vector table: 3-byte JP into stub bodies in low main_rom. The SNSMAT
     entry sits at its real BIOS address 0x0024 -- 3 bytes, so it occupies
     0x24-0x26 -- which pushed the CHSNS stub body off the 0x20-0x25 range
     chsns_hle_call_test.ml uses for it, into 0x40, to avoid an address
     collision between the two vectors' own tables. *)
  Bytes.set main 0x24 '\xc3'; Bytes.set main 0x25 '\x30'; Bytes.set main 0x26 '\x00';
  Bytes.set main 0x9c '\xc3'; Bytes.set main 0x9d '\x40'; Bytes.set main 0x9e '\x00';
  (* 0x0030 SNSMAT body: LD A,(0xFBF1); LD (0xC102),A; RET -- returns the
     row bits through A (read back via the caller's own store, same as
     chsns_hle_call_test's CHGET case) and leaves a marker so the scenario
     can tell the real stub ran, not RAM garbage. *)
  Bytes.set main 0x30 '\x3a'; Bytes.set main 0x31 '\xf1'; Bytes.set main 0x32 '\xfb';
  Bytes.set main 0x33 '\x32'; Bytes.set main 0x34 '\x02'; Bytes.set main 0x35 '\xc1';
  Bytes.set main 0x36 '\xc9';
  (* 0x0040 CHSNS body: LD A,0x42; LD (0xC100),A; RET -- same shape as
     chsns_hle_call_test.ml's stub (relocated address only), kept here to
     prove the two traps coexist. *)
  Bytes.set main 0x40 '\x3e'; Bytes.set main 0x41 '\x42';
  Bytes.set main 0x42 '\x32'; Bytes.set main 0x43 '\x00'; Bytes.set main 0x44 '\xc1';
  Bytes.set main 0x45 '\xc9';
  main

let run_scenario ?(pre = []) call_bytes =
  let t =
    Msx.create
      ~machine:{ Msx.ram_kb = 64; vram_kb = 128;
                 roms = [ Bytes.to_string (build_main_rom ()) ] }
  in
  Msx.load_disk ~interface_rom:false t "\000";
  Msx.port_out t 0xa8 0xc3;
  Msx.mem_write t 0xffff 0xaa;
  List.iteri (fun i b -> Msx.mem_write t i b)
    ([ 0x31; 0xf0; 0xff ] @ call_bytes @ [ 0x76 ]);
  List.iter (fun (a, b) -> Msx.mem_write t a b) pre;
  Msx.step t ~frames:1;
  t

let () =
  check "power-on RAM at 0x0024 is garbage, not a jump"
    (let t = run_scenario [] in
     Msx.mem_read t 0x24 = 0 && Msx.mem_read t 0x25 = 0xff);

  (* -- 1. SNSMAT: the deployment prompt's direct row-sense CALL must reach
     the stub AND bring its A result home. Game-side: latch a row value
     0x08 (a bit pattern, not 0/0xff, so the control below stays distinct)
     into 0xFBF1, CALL 0x0024, store A to 0xC103, HALT. *)
  let t = run_scenario
      [ 0x3e; 0x08;             (* LD A,0x08 *)
        0x32; 0xf1; 0xfb;       (* LD (0xFBF1),A *)
        0xcd; 0x24; 0x00;       (* CALL 0x0024 -- SNSMAT *)
        0x32; 0x03; 0xc1;       (* LD (0xC103),A *)
      ] in
  check "SNSMAT: reached HALT (control returned)" (Msx.cpu_halted t);
  check "SNSMAT: stub ran (marker byte landed)"
    (Msx.mem_read t 0xc102 = 0x08);
  check "SNSMAT: stub's A result came home through the caller's store"
    (Msx.mem_read t 0xc103 = 0x08);
  check "SNSMAT: ppi_a restored after the CALL returned" (Msx.ppi_a t = 0xc3);

  (* -- 2. Coexistence: SNSMAT (this fix) and CHSNS (task-1564's fix) in the
     same scenario, back to back -- the routine order the live prompt
     actually uses (0xDAE2's SNSMAT poll, then 0xDB1F's CHSNS). *)
  let t = run_scenario
      [ 0xcd; 0x24; 0x00;       (* CALL 0x0024 -- SNSMAT *)
        0xcd; 0x9c; 0x00;       (* CALL 0x009C -- CHSNS *)
      ] in
  check "coexistence: reached HALT (control returned)" (Msx.cpu_halted t);
  check "coexistence: CHSNS stub still ran after SNSMAT"
    (Msx.mem_read t 0xc100 = 0x42);
  check "coexistence: ppi_a restored after both CALLs" (Msx.ppi_a t = 0xc3);

  (* -- 3. Whitelist control: 0x0026 is no BIOS vector (adjacent to the new
     0x0024 entry) -- the trap must not fire for it. Same absence-evidence
     shape as chsns_hle_call_test's control. *)
  let t =
    run_scenario ~pre:[ (0x38, 0x76) ] [ 0xcd; 0x26; 0x00 ]
  in
  check "control: ppi_a untouched (trap did not fire)" (Msx.ppi_a t = 0xc3);
  check "control: main_rom stub not dispatched (marker cell untouched)"
    (Msx.mem_read t 0xc102 = 0xff)

let () =
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "snsmat_hle_call_test: all pass"
