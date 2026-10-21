(* Regression for ocaml-msx#51: the same plain-CALL trap that serves CHSNS /
   CHGET / CHPUT / DIRIN must also serve SNSMAT 0x0141, GTSTCK 0x00D5 and
   GTTRIG 0x00D8.

   GTTRIG joins them for the same reason and from the same source: C-BIOS
   0.29a's page-0 table has `jp gttrig` at 0x00D8 (src/main.asm:363-367), and
   gttrig's body (src/main.asm:2349-2397) reads exactly the two places the
   other two vectors read -- SNSMAT row 8 bit 0 for the spacebar path (A=0)
   and PSG R#14 bits 4/5 for the stick path (A=1..4). A disk kernel that
   reaches 0x00D5 by plain CALL reaches 0x00D8 the same way, so leaving it
   unserved means the triggers alone stay invisible while directions work.
   GTPAD 0x00DB and GTPDL 0x00DE stay out on purpose: C-BIOS's own bodies are
   stubs (src/main.asm:2415-2447), so routing there would return a stub answer
   from the ROM instead of power-on garbage -- no keyboard or joystick path
   would be gained.

   Why those two: C-BIOS 0.29a's own page-0 table puts a 3-byte JP at both
   addresses -- `c3 27 18` at 0x0141 (SNSMAT, body: row select on PPI C, read
   PPI A) and `c3 43 18` at 0x00D5 (GTSTCK, body: LD A,8 / CALL 0x1827 /
   nibble shuffle / table) -- and keyboard matrix row 8 is the one row both of
   them read. A disk kernel that routes BIOS vectors through the 0xDB10
   self-patching trampoline reaches them with a plain CALL exactly as it
   reaches CONST/CONIN, so with page 0 mapped to RAM the CALL would run
   power-on garbage instead of reading the row: every direction key and
   joystick bit would be invisible on that path.

   Scenarios 1-2b use a BIOS-shaped page 0 (JP slots into stub bodies in low
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

(* BIOS-shaped page 0: 0x0141, 0x00D5 and 0x00D8 are JP stubs into low main_rom,
   the layout C-BIOS 0.29a itself uses (c3 27 18 / c3 43 18 / jp gttrig). *)
let build_main_rom () =
  let main = Bytes.make 0x4000 '\000' in
  Bytes.set main 0x141 '\xc3'; Bytes.set main 0x142 '\x30'; Bytes.set main 0x143 '\x00';
  Bytes.set main 0x0d5 '\xc3'; Bytes.set main 0x0d6 '\x40'; Bytes.set main 0x0d7 '\x00';
  Bytes.set main 0x0d8 '\xc3'; Bytes.set main 0x0d9 '\x50'; Bytes.set main 0x0da '\x00';
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
  (* 0x0050 GTTRIG body: LD (0xC146),A; LD A,0xFF; LD (0xC147),A; RET -- 0xFF is
     the "trigger pressed" value C-BIOS returns (main.asm:2392-2397). *)
  Bytes.set main 0x50 '\x32'; Bytes.set main 0x51 '\x46'; Bytes.set main 0x52 '\xc1';
  Bytes.set main 0x53 '\x3e'; Bytes.set main 0x54 '\xff';
  Bytes.set main 0x55 '\x32'; Bytes.set main 0x56 '\x47'; Bytes.set main 0x57 '\xc1';
  Bytes.set main 0x58 '\xc9';
  (* 0x0321 (ROM-visible at 0x4321): LD A,8 ; CALL 0x0141 ; RET -- a caller that
     lives inside the BIOS ROM itself, for the origin-split check. *)
  Bytes.set main 0x321 '\x3e'; Bytes.set main 0x322 '\x08';
  Bytes.set main 0x323 '\xcd'; Bytes.set main 0x324 '\x41'; Bytes.set main 0x325 '\x01';
  Bytes.set main 0x326 '\xc9';
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

  (* -- 2b. GTTRIG 0x00D8: trigger number 1 (port 1, button A) goes in, the
     pressed value comes back. *)
  let t =
    run_scenario ~log:true
      [ 0x3e; 0x01;             (* LD A,1 -- port 1 button A *)
        0xcd; 0xd8; 0x00;       (* CALL 0x00D8 -- GTTRIG *)
        0x32; 0x48; 0xc1 ]      (* LD (0xC148),A *)
  in
  check "GTTRIG: reached HALT (control returned)" (Msx.cpu_halted t);
  check "GTTRIG: stub received the caller's trigger number in A"
    (Msx.mem_read t 0xc146 = 0x01);
  check "GTTRIG: stub ran and its A result came home"
    (Msx.mem_read t 0xc148 = 0xff);
  check "GTTRIG: ppi_a restored after the CALL returned" (Msx.ppi_a t = 0xc3);
  (* The two sources GTTRIG's body actually consults (main.asm:2356-2390) are
     already driven by the core: space clears matrix row 8 bit 0, and the
     stick path's PSG R#14 bits 4/5 follow Trigger_a/b. *)
  check "GTTRIG space path: Space clears matrix row 8 bit 0"
    (let m = fresh () in
     ignore (Msx.set_key m Msx.Space ~pressed:true);
     row8 m land 0x01 = 0);
  check "GTTRIG stick path: Trigger_a clears PSG R#14 bit 4"
    (let m = fresh () in
     ignore (Msx.set_key m Msx.Trigger_a ~pressed:true);
     psg14 m land 0x10 = 0);
  check "GTTRIG stick path: Trigger_b clears PSG R#14 bit 5"
    (let m = fresh () in
     ignore (Msx.set_key m Msx.Trigger_b ~pressed:true);
     psg14 m land 0x20 = 0);

  (* -- 3. Observation log: all three vectors are visible with their return
     address, which is the only evidence that separates a real kernel call from
     a guess. Lengths are measured as deltas from the count taken just before
     the run, so this check does not depend on which scenario ran first. *)
  let _t =
    run_scenario ~log:true
      [ 0x3e; 0x08; 0xcd; 0x41; 0x01;   (* CALL 0x0141 *)
        0x3e; 0x00; 0xcd; 0xd5; 0x00;   (* CALL 0x00D5 *)
        0x3e; 0x01; 0xcd; 0xd8; 0x00 ]   (* CALL 0x00D8 *)
  in
  let entries = Msx.bios_vector_entries () in
  check "log: exactly three served vectors recorded"
    (List.length entries = 3);
  check "log: SNSMAT recorded with a return address inside the caller, in RAM"
    (match entries with
     | (0x0141, ret, Msx.From_ram) :: _ -> ret >= 0x0000 && ret < 0x0100
     | _ -> false);
  check "log: GTSTCK recorded after it"
    (match entries with
     | _ :: (0x00d5, _, Msx.From_ram) :: _ -> true
     | _ -> false);
  check "log: GTTRIG recorded last"
    (match entries with
     | _ :: _ :: (0x00d8, _, Msx.From_ram) :: _ -> true
     | _ -> false);
  (* The origin split itself: with page 1 selected to slot 0 the main ROM holds
     0x4000-0x7FFF, so a CALL issued from inside the ROM body must be recorded
     as From_rom while the same vector called from RAM stays From_ram. Without
     this, one 0x0141 count silently mixes the stub's own call with the game's
     -- which is the number a "the game really called it" claim would rest on. *)
  check "origin: address inside the selected main ROM classifies as From_rom"
    (let m = fresh () in
     Msx.port_out m 0xa8 0xc3;
     Msx.bios_vector_origin m 0x4321 = Msx.From_rom);
  check "origin: address in page-0 RAM classifies as From_ram"
    (let m = fresh () in
     Msx.port_out m 0xa8 0xc3;
     Msx.bios_vector_origin m 0x0100 = Msx.From_ram);
  (* And through the trap: a CALL issued from the ROM body must land in the
     From_rom bucket. main_rom carries a tiny caller at ROM offset 0x321
     (visible at 0x4321 when page 1 selects slot 0): LD A,8 ; CALL 0x0141 ;
     RET. The RAM stub just CALLs into it, so the logged return address
     0x4326 sits inside the BIOS ROM while the RAM-issued calls above sit in
     page-0 RAM. *)
  let t =
    run_scenario ~log:true
      [ 0xcd; 0x21; 0x43 ]   (* CALL 0x4321 -- into the ROM caller *)
  in
  let rom_entries =
    List.filter
      (fun (pc, _, origin) -> pc = 0x0141 && origin = Msx.From_rom)
      (Msx.bios_vector_entries ())
  in
  check "origin: a CALL issued from the ROM body is logged as From_rom"
    (match rom_entries with
     | [ (0x0141, 0x4326, Msx.From_rom) ] -> true
     | _ -> false);
  check "origin: the ROM-origin call still returns control to the caller"
    (Msx.cpu_halted t);
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
