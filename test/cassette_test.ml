(* Cassette playback. The reference wave below spells out openMSX 21.0
   CasImage's converter one sample at a time, imperatively; Cassette keeps
   segments and computes samples, so the two are written differently and
   compared on every sample. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let header = "\x1F\xA6\xDE\xBA\xCC\x13\x7D\x74"
let sample_rate = 14976

(* '0' silence, '+' and '-' the halves of a pulse. *)
let reference cas =
  let n = String.length cas in
  let w = Buffer.create 4096 in
  let silence k = Buffer.add_string w (String.make k '0') in
  let bit b = Buffer.add_string w (if b then "+-+-" else "++--") in
  let pilot k = for _ = 1 to k do bit true done in
  let byte c =
    let c = Char.code c in
    bit false;
    for i = 0 to 7 do bit ((c lsr i) land 1 = 1) done;
    bit true;
    bit true
  in
  let is_header p = p + 8 <= n && String.sub cas p 8 = header in
  let pos = ref 0 in
  let write_data () =
    let eof = ref false and stop = ref false and result = ref false in
    while not !stop && !pos + 8 <= n do
      if is_header !pos then (stop := true; result := !eof)
      else begin
        byte cas.[!pos];
        if cas.[!pos] = '\x1a' then eof := true;
        incr pos
      end
    done;
    if not !stop then
      while !pos < n do byte cas.[!pos]; incr pos done;
    !result
  in
  let id p c = p + 10 <= n && String.sub cas p 10 = String.make 10 c in
  while !pos + 8 <= n do
    if is_header !pos then begin
      pos := !pos + 8;
      silence (2 * sample_rate);
      pilot 8000;
      if id !pos '\xEA' then begin
        ignore (write_data ());
        let continue = ref true in
        while !continue do
          pos := !pos + 8;
          silence sample_rate;
          pilot 2000;
          if write_data () then continue := false
          else continue := !pos + 8 <= n
        done
      end
      else if id !pos '\xD0' || id !pos '\xD3' then begin
        ignore (write_data ());
        silence sample_rate;
        pilot 2000;
        pos := !pos + 8;
        ignore (write_data ())
      end
      else ignore (write_data ())
    end
    else incr pos
  done;
  Buffer.contents w

let tape_exn cas = match Cassette.of_cas cas with Ok t -> t | Error e -> failwith e

let same_wave name cas =
  let ref_wave = reference cas in
  let tape = tape_exn cas in
  check (name ^ ": length") (Cassette.samples tape = String.length ref_wave);
  let first_bad = ref (-1) in
  String.iteri
    (fun k c -> if !first_bad < 0 && Cassette.level tape k <> (c <> '-') then first_bad := k)
    ref_wave;
  if !first_bad >= 0 then begin
    incr failures;
    Printf.eprintf "FAIL %s: sample %d differs\n%!" name !first_bad
  end

let file kind name = header ^ String.make 10 kind ^ name
let binary = file '\xD0' "GAME  " ^ header ^ "\x00\x90\x05\x90\x00\x90\x3e\x41\xc9\x00\xff"
let basic = file '\xD3' "PROG  " ^ header ^ "\x01\x02\x03"
let ascii =
  file '\xEA' "TEXT  " ^ header ^ "10 PRINT 1\r\n" ^ header ^ "20 END\r\n\x1a\x1a" ^ binary

let () =
  same_wave "binary" binary;
  same_wave "basic" basic;
  same_wave "ascii then binary" ascii;
  same_wave "ascii without a closing block" (file '\xEA' "TEXT  " ^ header ^ "10 END\r\n");
  (* A file header block with nothing after it still gets the short lead
     openMSX writes before the data block it expects. *)
  same_wave "ascii header block alone" (file '\xEA' "TEXT  ");
  same_wave "binary header block alone" (file '\xD0' "GAME  ");
  same_wave "unknown kind" (header ^ "custom loader block" ^ header ^ "\x01\x02");
  same_wave "junk before the first header" ("xyz" ^ binary);
  same_wave "header with too few bytes after it" (header ^ "\x01");
  check "binary kind" (Cassette.first_file_type (tape_exn binary) = Binary);
  check "basic kind" (Cassette.first_file_type (tape_exn basic) = Basic);
  check "ascii kind" (Cassette.first_file_type (tape_exn ascii) = Ascii);
  check "unknown kind" (Cassette.first_file_type (tape_exn (header ^ "loader")) = Unknown);
  check "no header is an error" (Result.is_error (Cassette.of_cas "not a tape"));
  check "the image is kept" (Cassette.cas (tape_exn basic) = basic);
  (* Hand-counted length of the binary file: 2 s and 8000 bits of lead,
     16 header bytes, 1 s and 2000 bits, 11 data bytes; 44 samples a byte. *)
  check "binary length by hand"
    (Cassette.samples (tape_exn binary)
     = (2 * sample_rate) + (8000 * 4) + (16 * 44) + sample_rate + (2000 * 4) + (11 * 44));
  check "level past the end is high" (Cassette.level (tape_exn basic) max_int);
  check "level before the start is high" (Cassette.level (tape_exn basic) (-1))

(* The deck: openMSX finds sample k at k * 229458 units of 1/(3579545 * 960)
   s; [first_cycle k] is the first CPU cycle that reaches it. *)
let units_per_sample = 229458
let first_cycle k = ((k * units_per_sample) + 959) / 960
let pilot_start = 2 * sample_rate

let () =
  let tape = tape_exn binary in
  let d = Cassette.insert tape ~cycle:0 in
  let on = true and off = false in
  check "silence before the pilot"
    (Cassette.input d ~cycle:(first_cycle pilot_start - 1) ~motor:on);
  check "first pilot half-pulse is high"
    (Cassette.input d ~cycle:(first_cycle pilot_start) ~motor:on);
  check "one cycle before the second half-pulse"
    (Cassette.input d ~cycle:(first_cycle (pilot_start + 1) - 1) ~motor:on);
  check "second pilot half-pulse is low"
    (not (Cassette.input d ~cycle:(first_cycle (pilot_start + 1)) ~motor:on));
  (* With the motor off the tape stays where it is. *)
  let stopped_at = first_cycle (pilot_start + 1) in
  Cassette.advance d ~cycle:(stopped_at + 1_000_000) ~motor:off;
  check "motor off keeps the position"
    (Cassette.position d ~cycle:(stopped_at + 1_000_000) ~motor:off
     = stopped_at * 960);
  check "still on the low half-pulse" (not (Cassette.input d ~cycle:(stopped_at + 1_000_000) ~motor:off));
  check "position counts cycles while the motor runs"
    (Cassette.position d ~cycle:(stopped_at + 1_000_010) ~motor:on = (stopped_at + 10) * 960);
  let resumed = Cassette.resume tape ~position:(stopped_at * 960) ~cycle:5 in
  check "a resumed deck reads where it was"
    (not (Cassette.input resumed ~cycle:5 ~motor:on))

(* Through the machine: PPI port C bit 4 = 0 runs the motor, PSG R#14 bit 7
   shows the input, and a save state carries the tape. *)
let () =
  let machine () = Msx.create ~machine:{ Msx.ram_kb = 64; vram_kb = 128; roms = [ ""; ""; "" ] } in
  let r14 m = Msx.port_out m 0xA0 14; Msx.port_in m 0xA2 land 0x80 <> 0 in
  let motor m on = Msx.port_out m 0xAA (if on then 0x00 else 0x10) in
  let m = machine () in
  check "a bad image is refused" (Result.is_error (Msx.insert_cassette m "nothing"));
  check "no tape reads high" (r14 m);
  (match Msx.insert_cassette m binary with Ok () -> () | Error e -> failwith e);
  motor m false;
  let levels m n = List.init n (fun _ -> Msx.step m ~frames:1; r14 m) in
  check "motor off: silence only" (List.for_all Fun.id (levels m 200));
  motor m true;
  (* The tape starts moving at this write: 2 s (119.8 frames) of silence,
     then the pilot. A frame is about 250 samples, so the pilot shows both
     levels within a few frames. *)
  check "motor on: 2 s of silence first" (List.for_all Fun.id (levels m 119));
  Msx.step m ~frames:2;
  let pilot = levels m 16 in
  check "pilot shows both levels" (List.mem true pilot && List.mem false pilot);
  let state = Msx.serialize m in
  let original = levels m 16 in
  (match Msx.restore ~state with
   | Error e -> failwith ("restore: " ^ e)
   | Ok restored ->
     check "restored tape plays on the same way" (levels restored 16 = original));
  motor m false;
  let held = levels m 16 in
  check "motor off holds one level" (List.for_all (( = ) (List.hd held)) held);
  (* Saved while the motor is off and unread for a while: the stopped time
     does not count. *)
  Msx.step m ~frames:50;
  let state = Msx.serialize m in
  motor m true;
  let original = levels m 16 in
  match Msx.restore ~state with
  | Error e -> failwith ("restore: " ^ e)
  | Ok restored ->
    motor restored true;
    check "saved with the motor off" (levels restored 16 = original)

(* Port C high as an input: the motor pins keep what they last drove and
   latch writes do not reach them (openMSX I8255::outputPortC). *)
let () =
  let m = Msx.create ~machine:{ Msx.ram_kb = 64; vram_kb = 128; roms = [ ""; ""; "" ] } in
  let r14 m = Msx.port_out m 0xA0 14; Msx.port_in m 0xA2 land 0x80 <> 0 in
  let levels m n = List.init n (fun _ -> Msx.step m ~frames:1; r14 m) in
  let both l = List.mem true l && List.mem false l in
  (match Msx.insert_cassette m binary with Ok () -> () | Error e -> failwith e);
  Msx.port_out m 0xAA 0x00;
  Msx.port_out m 0xAB 0x8A (* mode 0, port C high an input *);
  Msx.port_out m 0xAA 0x10 (* the latch says motor off *);
  Msx.step m ~frames:121;
  let state = Msx.serialize m in
  let running = levels m 16 in
  check "C high input: the motor keeps running" (both running);
  (match Msx.restore ~state with
   | Error e -> failwith ("restore: " ^ e)
   | Ok restored -> check "the pins survive a save" (levels restored 16 = running));
  Msx.port_out m 0xAB 0x82 (* an output again: the latch drives the pins *);
  let held = levels m 16 in
  check "C high output: the latch stops the motor" (List.for_all (( = ) (List.hd held)) held);
  Msx.port_out m 0xAB 0x8A;
  Msx.port_out m 0xAB 0x08 (* bit 4 reset: the latch says motor on *);
  let held = levels m 16 in
  check "C high input: a bit reset does not start it" (List.for_all (( = ) (List.hd held)) held)

let () =
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "cassette_test: all pass"
