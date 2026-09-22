(* Regression for task-1564 (Sangokushi II keeper handoff): a disk kernel
   that calls BIOS fixed-page-0 vectors -- CHSNS 0x009C, CHGET 0x009F --
   with a plain CALL instruction (not CALSLT/RST 30h) while page 0 is RAM
   must still reach the real BIOS routines, not RAM power-on garbage read
   back as opcodes.

   Root cause traced statically from a live hang (checkpoint
   task1564-rootcause-confirmed, PC=0xDA85): Sangokushi II's DOS routes
   every BIOS vector through one self-patching trampoline -- each BDOS
   handler does `LD IX,<vector>; CALL 0xDB10`, and 0xDB10 does
   `LD (0xDB20),IX` so its `CALL 0x009C` template at 0xDB1F executes with
   the handler's vector (0x009C for CONST, 0x009F for CONIN, 0x00A2 for
   CONOUT, 0x0156 for DIRIN) -- while page 0 is mapped to RAM (the
   DOS-kernel-replay shape, ppi_a land 3 = 3). The CALL then executes RAM
   power-on garbage -- the (00 FF)* initial pattern, not a jump -- and
   every key-wait path stalls: CONST never sees a key, CONIN never reads
   one. A CHSNS-only trap was not enough: the game would hang one step
   later, inside CONIN's CHGET.

   Each scenario builds the same shape on a FRESH machine (a scenario
   ends in HALT, and a halted CPU must not poison the next one), with
   main_rom (a real 16KB image via [Msx.create]) holding a BIOS-style JP
   vector table at page 0 and stub bodies in low main_rom. The scenarios
   prove four things the fix in [Msx.disk_trap] is responsible for:
   1. a plain CALL 0x009C reaches main_rom's CHSNS stub (the marker lands);
   2. a plain CALL 0x009F reaches the CHGET stub and its A result -- read
      back through the caller's own store -- lands (the CHSNS-only fix
      would fail here);
   3. ppi_a is restored to its pre-call value afterward (the RAM mapping
      for page 0 that the kernel depends on stays intact);
   4. the vector whitelist: a plain CALL to a non-BIOS address with page 0
      in RAM stays in RAM -- the trap does not fire for it. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

(* BIOS-shaped page 0: each vector slot is a 3-byte JP into a stub body in
   low main_rom -- exactly how the real BIOS lays out its fixed page. *)
let build_main_rom () =
  let main = Bytes.make 0x4000 '\000' in
  (* vector table *)
  Bytes.set main 0x9c '\xc3'; Bytes.set main 0x9d '\x20'; Bytes.set main 0x9e '\x00';
  Bytes.set main 0x9f '\xc3'; Bytes.set main 0xa0 '\x28'; Bytes.set main 0xa1 '\x00';
  (* 0x0020 CHSNS body: LD A,0x42; LD (0xC100),A; RET *)
  Bytes.set main 0x20 '\x3e'; Bytes.set main 0x21 '\x42';
  Bytes.set main 0x22 '\x32'; Bytes.set main 0x23 '\x00'; Bytes.set main 0x24 '\xc1';
  Bytes.set main 0x25 '\xc9';
  (* 0x0028 CHGET body: LD A,(0xFBF0); RET -- the system-work-area key latch,
     read through the live slot mapping: the trap switches page 0 only, so
     page 3 stays slot 3 (RAM) and the BIOS sees the kernel's latch. *)
  Bytes.set main 0x28 '\x3a'; Bytes.set main 0x29 '\xf0'; Bytes.set main 0x2a '\xfb';
  Bytes.set main 0x2b '\xc9';
  main

(* A fresh machine in the DOS-kernel-replay shape: disk mounted (the trap
   checks [Bytes.length t.disk], no FAT12 structure is touched;
   [~interface_rom:false] keeps ppi_a untouched, matching the warm-up
   replay shape the real hang was traced from), page0 = slot3 (RAM) and
   page3 = slot3 (RAM: SP, the latch and the marker writes land somewhere
   writable; ppi_a bit layout: page0 bits0-1, page3 bits6-7), slot3 sub
   select at 0xFFFF set to 0xaa (page0·3 to sub2, the RAM mapper -- the
   wiring [boot_disk] installs; [load_disk] alone leaves the 0x00 default,
   where page0 resolves to sub0 the sub ROM and a store to page 0 is
   dropped). The caller program [call_bytes] is planted at RAM 0 as
   LD SP,0xFFF0; <call_bytes>; HALT, then one frame runs. *)
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
  check "power-on RAM at the vectors is garbage, not a jump"
    (let t = run_scenario [] in
     t |> ignore;
     Msx.mem_read t 0x9c = 0 && Msx.mem_read t 0x9f = 0xff);

  (* -- 1. CHSNS: CONST's plain CALL must reach the stub. *)
  let t = run_scenario [ 0xcd; 0x9c; 0x00 ] in
  check "CHSNS: reached HALT (control returned)" (Msx.cpu_halted t);
  check "CHSNS: stub actually ran (marker byte landed)"
    (Msx.mem_read t 0xc100 = 0x42);
  check "CHSNS: ppi_a restored after the CALL returned" (Msx.ppi_a t = 0xc3);

  (* -- 2. CHGET: CONIN's plain CALL must reach the stub AND bring its A
     result home -- the case the CHSNS-only trap did not cover. Game-side:
     latch 0x55 into 0xFBF0, CALL 0x009F, store A to 0xC101, HALT. *)
  let t = run_scenario
      [ 0x3e; 0x55;             (* LD A,0x55 *)
        0x32; 0xf0; 0xfb;       (* LD (0xFBF0),A *)
        0xcd; 0x9f; 0x00;       (* CALL 0x009F -- CHGET *)
        0x32; 0x01; 0xc1;       (* LD (0xC101),A *)
      ] in
  check "CHGET: reached HALT (control returned)" (Msx.cpu_halted t);
  check "CHGET: stub ran and returned the latch through A"
    (Msx.mem_read t 0xc101 = 0x55);
  check "CHGET: ppi_a restored after the CALL returned" (Msx.ppi_a t = 0xc3);

  (* -- 3. Whitelist control: 0x009A is no BIOS vector -- the trap must not
     fire for it. The CALL stays in RAM (power-on garbage), so the scenario
     plants HALT at the next instruction boundary the CPU can only reach by
     executing that garbage 0xFF (RST 38h at 0x0009): halt at 0x0038. *)
  let t =
    run_scenario ~pre:[ (0x38, 0x76) ] [ 0xcd; 0x9a; 0x00 ]
  in
  check "control: ppi_a untouched (trap did not fire)" (Msx.ppi_a t = 0xc3);
  (* Absence evidence: the marker cell still holds the (00 FF)* power-on
     byte 0xff -- observed directly before any step -- so the stub never
     wrote it; 0x00 would be the wrong expectation here, this RAM does not
     start zeroed. *)
  check "control: main_rom stub not dispatched (marker cell untouched)"
    (Msx.mem_read t 0xc100 = 0xff)

let () =
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "chsns_hle_call_test: all pass"
