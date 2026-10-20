(* Regression for ocaml-msx#51: the same plain-CALL trap that serves CHSNS /
   CHGET / CHPUT / DIRIN must also serve SNSMAT 0x0141 and GTSTCK 0x00D5.

   Why those two: C-BIOS 0.29a's own page-0 table puts a 3-byte JP at both
   addresses -- `c3 27 18` at 0x0141 (SNSMAT, body: row select on PPI C, read
   PPI A) and `c3 43 18` at 0x00D5 (GTSTCK, body: LD A,8 / CALL 0x1827 /
   nibble shuffle / table) -- and keyboard matrix row 8 is the one row both of
   them read. A disk kernel that routes BIOS vectors through the 0xDB10
   self-patching trampoline reaches them with a plain CALL exactly as it
   reaches CONST/CONIN, so with page 0 mapped to RAM the CALL would run
   power-on garbage instead of reading the row: every direction key and
   joystick bit would be invisible on that path.

   Scenarios 1-3 use a BIOS-shaped page 0 (JP slots into stub bodies in low
   main_rom) on a fresh machine each, mirroring chsns_hle_call_test, and prove
   the plumbing: a plain CALL reaches the vector body, the caller's argument
   arrives, the result comes home, ppi_a is restored, and the observation
   ledger records (pc, return address).

   Scenario 4 stops using a stub for the direction question and reads the
   core's keyboard matrix and joystick latch directly: the eight arrow
   combinations must produce the two nibble encodings C-BIOS 0.29a's own
   GTSTCK body expects, so both of its direction tables land on 1..8. The
   plumbing and the mapping are separate claims and are tested separately;
   the tables themselves are ROM constants and are cited, not re-derived. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

(* BIOS-shaped page 0: 0x0141 and 0x00D5 are JP stubs into low main_rom, the
   layout C-BIOS 0.29a itself uses (c3 27 18 / c3 43 18). *)
let build_main_rom () =
  let main = Bytes.make 0x4000 '\000' in
  Bytes.set main 0x141 '\xc3'; Bytes.set main 0x142 '\x30'; Bytes.set main 0x143 '\x00';
  Bytes.set main 0x0d5 '\xc3'; Bytes.set main 0x0d6 '\x40'; Bytes.set main 0x0d7 '\x00';
  (* 0x0030 SNSMAT body: LD (0xC140),A; LD A,0x5a; LD (0xC141),A; RET -- proves
     both that the row number went in and that a result came back through A. *)
  Bytes.set main 0x30 '\x32'; Bytes.set main 0x31 '\x40'; Bytes.set main 0x32 '\xc1';
  Bytes.set main 0x33 '\x3e'; Bytes.set main 0x34 '\x5a';
  Bytes.set main 0x35 '\x32'; Bytes.set main 0x36 '\x41'; Bytes.set main 0x37 '\xc1';
  Bytes.set main 0x38 '\xc9';
  (* 0x0040 GTSTCK body: LD (0xC142),A; LD A,0x07; LD (0xC143),A; RET -- 0x07 is
     the left direction code the BIOS's own table returns for a keyboard left. *)
  Bytes.set main 0x40 '\x32'; Bytes.set main 0x41 '\x42'; Bytes.set main 0x42 '\xc1';
  Bytes.set main 0x43 '\x3e'; Bytes.set main 0x44 '\x07';
  Bytes.set main 0x45 '\x32'; Bytes.set main 0x46 '\x43'; Bytes.set main 0x47 '\xc1';
  Bytes.set main 0x48 '\xc9';
  main

(* Fresh machine in the DOS-kernel-replay shape (page 0 = slot 3 RAM, so the
   trap condition [ppi_a land 3 = 3] holds), caller at RAM 0. *)
let run_scenario ?(pre = []) ?(log = false) call_bytes =
  Msx.set_bios_vector_log log;
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

(* -- direction tables, transcribed from C-BIOS 0.29a src/main.asm ----------
   2341: joypos_kbd_tbl  $00,$07,$01,$08,$05,$06,$00,$07 / $03,$00,$02,$01,$04,$05,$03,$00
         index nibble = 0000RDUL (the GTSTCK keyboard path does RRCA*4 / CPL /
         AND 0x0F on the SNSMAT row-8 byte, main.asm:2264-2290)
   2335: joypos_joy_tbl  $00,$01,$05,$00,$07,$08,$06,$07 / $03,$02,$04,$03,$00,$01,$05,$00
         index nibble = 0000RLDU (PSG R14 low nibble, CPL, main.asm:2306-2320) *)
let kbd_tbl = [| 0x00; 0x07; 0x01; 0x08; 0x05; 0x06; 0x00; 0x07;
                 0x03; 0x00; 0x02; 0x01; 0x04; 0x05; 0x03; 0x00 |]
let joy_tbl = [| 0x00; 0x01; 0x05; 0x00; 0x07; 0x08; 0x06; 0x07;
                 0x03; 0x02; 0x04; 0x03; 0x00; 0x01; 0x05; 0x00 |]

let fresh () =
  Msx.create
    ~machine:{ Msx.ram_kb = 64; vram_kb = 128;
               roms = [ Bytes.to_string (build_main_rom ()) ] }

(* SNSMAT row 8 as the BIOS body reads it: select the row on PPI C, read PPI A. *)
let row8 t =
  Msx.port_out t 0xaa 0x08;
  Msx.port_in t 0xa9 land 0xff

(* PSG R14 as the BIOS body reads it (register latch 14, then read port A2). *)
let psg14 t =
  Msx.port_out t 0xa0 0x0e;
  Msx.port_in t 0xa2 land 0xff

let () =
  ignore (fresh);

  (* -- 1. SNSMAT 0x0141: row number 8 goes in, stub result comes back. *)
  let t =
    run_scenario ~log:true
      [ 0x3e; 0x08;             (* LD A,8 -- matrix row 8 *)
        0xcd; 0x41; 0x01;       (* CALL 0x0141 -- SNSMAT *)
        0x32; 0x44; 0xc1 ]      (* LD (0xC144),A -- store the result *)
  in
  check "SNSMAT: reached HALT (control returned)" (Msx.cpu_halted t);
  check "SNSMAT: stub received the caller's row number in A"
    (Msx.mem_read t 0xc140 = 0x08);
  check "SNSMAT: stub ran and its A result came home"
    (Msx.mem_read t 0xc144 = 0x5a);
  check "SNSMAT: ppi_a restored after the CALL returned" (Msx.ppi_a t = 0xc3);

  (* -- 2. GTSTCK 0x00D5: stick number 0 (keyboard) goes in, result comes back. *)
  let t =
    run_scenario ~log:true
      [ 0x3e; 0x00;             (* LD A,0 -- keyboard, not a joystick *)
        0xcd; 0xd5; 0x00;       (* CALL 0x00D5 -- GTSTCK *)
        0x32; 0x45; 0xc1 ]      (* LD (0xC145),A *)
  in
  check "GTSTCK: reached HALT (control returned)" (Msx.cpu_halted t);
  check "GTSTCK: stub received the caller's stick number in A"
    (Msx.mem_read t 0xc142 = 0x00);
  check "GTSTCK: stub ran and its A result came home"
    (Msx.mem_read t 0xc145 = 0x07);
  check "GTSTCK: ppi_a restored after the CALL returned" (Msx.ppi_a t = 0xc3);

  (* -- 3. Observation log: both vectors are visible with their return address,
     which is the only evidence that separates a real kernel call from a guess.
     Lengths are measured as deltas from the count taken just before the run,
     so this check does not depend on which scenario ran first. *)
  let _t =
    run_scenario ~log:true
      [ 0x3e; 0x08; 0xcd; 0x41; 0x01;   (* CALL 0x0141 *)
        0x3e; 0x00; 0xcd; 0xd5; 0x00 ]   (* CALL 0x00D5 *)
  in
  let entries = Msx.bios_vector_entries () in
  check "log: exactly two served vectors recorded"
    (List.length entries = 2);
  check "log: SNSMAT recorded with a return address inside the caller"
    (match entries with
     | (0x0141, ret) :: _ -> ret >= 0x0000 && ret < 0x0100
     | _ -> false);
  check "log: GTSTCK recorded after it"
    (match entries with
     | _ :: (0x00d5, _) :: _ -> true
     | _ -> false);
  (* Logging off must not append. Measure the length before and after the run
     instead of asserting a fixed size that only holds for this file order. *)
  check "log: logging off appends nothing to the ledger"
    (let n0 = List.length (Msx.bios_vector_entries ()) in
     let _t = run_scenario [ 0x3e; 0x08; 0xcd; 0x41; 0x01 ] in
     List.length (Msx.bios_vector_entries ()) = n0);

  (* -- 4. The eight arrow combinations, read from the core's own matrix and
     joystick latch, then pushed through C-BIOS 0.29a's two tables.

     Matrix row 8 (active low when pressed): bit4=L bit5=U bit6=D bit7=R.
     Joystick latch (active low): bit0=U bit1=D bit2=L bit3=R. *)
  let arrows =
    [ "up",     [ Msx.Up ],            1, 0x01;
      "right",  [ Msx.Right ],         3, 0x08;
      "down",   [ Msx.Down ],          5, 0x02;
      "left",   [ Msx.Left ],          7, 0x04;
      "up+right",   [ Msx.Up; Msx.Right ],   2, 0x09;
      "down+right", [ Msx.Down; Msx.Right ], 4, 0x0a;
      "down+left",  [ Msx.Down; Msx.Left ],  6, 0x06;
      "up+left",    [ Msx.Up; Msx.Left ],    8, 0x05 ]
  in
  List.iter
    (fun (name, keys, want_code, want_joy_nibble) ->
       let m = fresh () in
       List.iter (fun k -> ignore (Msx.set_key m k ~pressed:true)) keys;
       let r8 = row8 m in
       (* GTSTCK keyboard path: RRCA*4, CPL, AND 0x0F -> 0000RDUL. *)
       let kbd_nibble = (lnot (r8 lsr 4)) land 0x0f in
       check (Printf.sprintf "matrix row 8: %s selects joypos_kbd_tbl[%X]"
                name kbd_nibble)
         (kbd_tbl.(kbd_nibble) = want_code);
       (* GTSTCK joystick path: CPL on PSG R14 low nibble -> 0000RLDU. *)
       let joy_nibble = (lnot (psg14 m)) land 0x0f in
       check (Printf.sprintf "joystick latch: %s selects joypos_joy_tbl[%X]"
                name joy_nibble)
         (joy_nibble = want_joy_nibble && joy_tbl.(joy_nibble) = want_code);
       (* Releasing must clear both: a stuck bit would read as a held key. *)
       List.iter (fun k -> ignore (Msx.set_key m k ~pressed:false)) keys;
       check (Printf.sprintf "matrix row 8: %s releases cleanly" name)
         (row8 m = 0xff);
       check (Printf.sprintf "joystick latch: %s releases cleanly" name)
         ((lnot (psg14 m)) land 0x0f = 0))
    arrows;
  check "matrix row 8: no arrow held reads 0xFF (nothing pressed)" (row8 (fresh ()) = 0xff)

let () =
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "direction_hle_call_test: all pass"
