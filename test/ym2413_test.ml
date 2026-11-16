(* YM2413: register file, pitch, level, envelope, rhythm mode, and the match
   against openMSX's NukeYKT core on a fixed stimulus (test/ym2413/). *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let rate = 49716

let run t n = for _ = 1 to n do Ym2413.step t done

(* RMS and rising zero crossings of output [o] over the next [n] samples. *)
let measure t o n =
  let acc = ref 0. and zc = ref 0 and prev = ref (Ym2413.output t o) in
  for _ = 1 to n do
    Ym2413.step t;
    let v = Ym2413.output t o in
    acc := !acc +. (float v *. float v);
    if !prev <= 0 && v > 0 then incr zc;
    prev := v
  done;
  (sqrt (!acc /. float n), !zc)

let user_patch t bytes = List.iteri (fun r v -> Ym2413.write_register t r v) bytes

(* A sine: the modulator at total level 63 is silent, the carrier sustains
   (EG = 1) at full level, multiple 1. *)
let sine = [ 0x01; 0x21; 0x3f; 0x00; 0xf0; 0xf0; 0x00; 0x0f ]

let key_on t ch ~fnum ~blk ~vol ~inst =
  Ym2413.write_register t (0x10 + ch) (fnum land 0xff);
  Ym2413.write_register t (0x30 + ch) ((inst lsl 4) lor vol);
  Ym2413.write_register t (0x20 + ch) (0x10 lor (blk lsl 1) lor (fnum lsr 8))

let key_off t ch ~fnum ~blk ~sustain =
  Ym2413.write_register t (0x20 + ch)
    ((if sustain then 0x20 else 0) lor (blk lsl 1) lor (fnum lsr 8))

let registers () =
  let t = Ym2413.create () in
  run t 1000;
  check "a new chip is silent" (List.for_all (fun o -> Ym2413.output t o = 0) (List.init Ym2413.outputs Fun.id));
  check "a new chip mixes to 0" (Ym2413.mix t = 0);
  Ym2413.write_register t 0x19 0x55;
  check "0x19 mirrors 0x10" (Ym2413.register t 0x10 = 0x55);
  Ym2413.write_register t 0x29 0x16;
  check "0x29 mirrors 0x20" (Ym2413.register t 0x20 = 0x16);
  Ym2413.write_register t 0x3f 0x73;
  check "0x3F mirrors 0x36" (Ym2413.register t 0x36 = 0x73);
  let before = List.init 0x40 (Ym2413.register t) in
  Ym2413.write_register t 0x40 0xff;
  Ym2413.write_register t 0xff 0xff;
  check "registers 0x40 and above are ignored" (List.init 0x40 (Ym2413.register t) = before);
  Ym2413.write_address t 0x05;
  Ym2413.write_data t 0x9c;
  check "address then data writes the latched register" (Ym2413.register t 0x05 = 0x9c);
  Ym2413.write_data t 0x9d;
  check "the latch stays for the next data write" (Ym2413.register t 0x05 = 0x9d);
  Ym2413.write_register t 0x06 0x11;
  Ym2413.write_data t 0x9e;
  check "write_register leaves the latch alone" (Ym2413.register t 0x05 = 0x9e && Ym2413.register t 0x06 = 0x11);
  Ym2413.reset t;
  check "reset clears every register" (List.for_all (fun r -> Ym2413.register t r = 0) (List.init 0x40 Fun.id))

(* The pitch of an F-number and block at a frequency multiple [m]. *)
let hz ~fnum ~blk m = m *. float rate *. float fnum *. (2. ** float blk) /. (2. ** 19.)

let pitch () =
  List.iter
    (fun (fnum, blk) ->
      let t = Ym2413.create () in
      user_patch t sine;
      key_on t 3 ~fnum ~blk ~vol:0 ~inst:0;
      run t 2000;
      let _, zc = measure t 3 rate in
      let want = hz ~fnum ~blk 1. in
      check
        (Printf.sprintf "fnum %d block %d sounds %.1f Hz (got %d)" fnum blk want zc)
        (Float.abs (float zc -. want) <= 1. +. (want *. 0.002)))
    [ (0x100, 4); (0x1ab, 2); (0x0ad, 6); (0x158, 7) ];
  (* the carrier's frequency multiple, as the data sheet lists it *)
  let multiple = [| 0.5; 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.; 10.; 10.; 12.; 12.; 15.; 15. |] in
  Array.iteri
    (fun ml m ->
      let t = Ym2413.create () in
      user_patch t [ 0x01; 0x20 lor ml; 0x3f; 0x00; 0xf0; 0xf0; 0x00; 0x0f ];
      key_on t 3 ~fnum:0x100 ~blk:2 ~vol:0 ~inst:0;
      run t 2000;
      let _, zc = measure t 3 rate in
      let want = hz ~fnum:0x100 ~blk:2 m in
      check (Printf.sprintf "multiple %d sounds %.1f Hz (got %d)" ml want zc) (Float.abs (float zc -. want) <= 1. +. (want *. 0.002)))
    multiple;
  (* a user instrument change reaches a sounding note: multiple 2 doubles it *)
  let t = Ym2413.create () in
  user_patch t sine;
  key_on t 0 ~fnum:0x100 ~blk:4 ~vol:0 ~inst:0;
  run t 2000;
  let _, zc1 = measure t 0 rate in
  Ym2413.write_register t 1 0x22;
  run t 100;
  let _, zc2 = measure t 0 rate in
  check (Printf.sprintf "multiple 2 doubles the pitch (%d -> %d)" zc1 zc2) (abs (zc2 - (2 * zc1)) <= 2)

(* The channel volume steps by 3 dB. *)
let volume () =
  let level vol =
    let t = Ym2413.create () in
    user_patch t sine;
    key_on t 2 ~fnum:0x100 ~blk:4 ~vol ~inst:0;
    run t 4000;
    fst (measure t 2 8000)
  in
  let l0 = level 0 and l1 = level 1 and l4 = level 4 and l15 = level 15 in
  let db a b = 20. *. log10 (a /. b) in
  check (Printf.sprintf "volume 1 is 3 dB below 0 (%.2f dB)" (db l0 l1)) (Float.abs (db l0 l1 -. 3.) < 0.3);
  check (Printf.sprintf "volume 4 is 12 dB below 0 (%.2f dB)" (db l0 l4)) (Float.abs (db l0 l4 -. 12.) < 0.3);
  check (Printf.sprintf "volume 15 is 45 dB below 0 (%.2f dB)" (db l0 l15)) (Float.abs (db l0 l15 -. 45.) < 1.);
  let t = Ym2413.create () in
  user_patch t sine;
  key_on t 2 ~fnum:0x100 ~blk:5 ~vol:0 ~inst:0;
  let peak = ref 0 in
  for _ = 1 to 20000 do
    Ym2413.step t;
    peak := max !peak (abs (Ym2413.output t 2))
  done;
  check (Printf.sprintf "a full sine reaches 99%% of 2048 (%d)" !peak) (!peak >= 2028 && !peak <= 2048)

let envelope () =
  (* sustained (EG = 1) holds while keyed; percussive (EG = 0) keeps falling *)
  let after_keyon patch =
    let t = Ym2413.create () in
    user_patch t patch;
    key_on t 1 ~fnum:0x100 ~blk:4 ~vol:0 ~inst:0;
    (* past the decay to the sustain level *)
    run t 20000;
    let early = fst (measure t 1 2000) in
    run t 40000;
    let late = fst (measure t 1 2000) in
    (early, late)
  in
  let e, l = after_keyon [ 0x01; 0x21; 0x3f; 0x00; 0xf0; 0xf8; 0x00; 0x4f ] in
  check (Printf.sprintf "a sustained tone holds its level (%.0f -> %.0f)" e l) (Float.abs (e -. l) < 0.02 *. e);
  let e, l = after_keyon [ 0x01; 0x01; 0x3f; 0x00; 0xf0; 0xf8; 0x00; 0x45 ] in
  check (Printf.sprintf "a percussive tone falls (%.0f -> %.0f)" e l) (l < 0.5 *. e);
  (* attack: rate 15 starts at full level, a slow rate climbs *)
  let early_level ar =
    let t = Ym2413.create () in
    user_patch t [ 0x01; 0x21; 0x3f; 0x00; 0xf0; ar lsl 4; 0x00; 0x0f ];
    key_on t 1 ~fnum:0x100 ~blk:4 ~vol:0 ~inst:0;
    (* skip the key-on damp of the silent slot *)
    run t 50;
    fst (measure t 1 50)
  in
  check "attack rate 15 is at full level at once" (early_level 15 > 1000.);
  check "attack rate 4 is still quiet" (early_level 4 < 50.);
  (* Rates: two steps up make the envelope four times faster, and every
     step up is faster, through rate 15. Attack: samples until the sine
     first reaches 1900; decay (percussive, sustain level 15): samples until
     it stays below 40. *)
  let attack ar =
    let t = Ym2413.create () in
    user_patch t [ 0x01; 0x21; 0x3f; 0x00; 0xf0; ar lsl 4; 0x00; 0x0f ];
    key_on t 0 ~fnum:0x100 ~blk:7 ~vol:0 ~inst:0;
    let n = ref 0 in
    while abs (Ym2413.output t 0) < 1900 && !n < 10 * rate do
      Ym2413.step t;
      incr n
    done;
    !n
  in
  let decay dr =
    let t = Ym2413.create () in
    user_patch t [ 0x01; 0x01; 0x3f; 0x00; 0xf0; 0xf0 lor dr; 0x00; 0xf0 ];
    key_on t 0 ~fnum:0x100 ~blk:7 ~vol:0 ~inst:0;
    let n = ref 0 and quiet = ref 0 in
    while !quiet < 64 && !n < 10 * rate do
      Ym2413.step t;
      incr n;
      if abs (Ym2413.output t 0) < 40 then incr quiet else quiet := 0
    done;
    !n - !quiet
  in
  List.iter
    (fun (name, time) ->
      let times = List.init 12 (fun i -> (i + 4, time (i + 4))) in
      List.iter
        (fun (r, n) ->
          match List.assoc_opt (r + 1) times with
          | Some m -> check (Printf.sprintf "%s rate %d (%d samples) is faster than %d (%d)" name (r + 1) m r n) (m < n)
          | None -> ())
        times;
      List.iter
        (fun r ->
          let q = float (List.assoc r times) /. float (List.assoc (r + 2) times) in
          check (Printf.sprintf "%s rate %d is 4 times faster than %d (%.2f)" name (r + 2) r q) (q > 3.6 && q < 4.4))
        [ 4; 5; 6; 7; 8 ])
    [ ("attack", attack); ("decay", decay) ];
  (* key off: the release ends in silence; sustain on (R#0x2x bit 5) is slower *)
  let release_samples sustain =
    let t = Ym2413.create () in
    user_patch t [ 0x01; 0x21; 0x3f; 0x00; 0xf0; 0xf0; 0x00; 0x07 ];
    key_on t 4 ~fnum:0x100 ~blk:4 ~vol:0 ~inst:0;
    run t 4000;
    key_off t 4 ~fnum:0x100 ~blk:4 ~sustain;
    let n = ref 0 and quiet = ref 0 in
    while !quiet < 2000 && !n < 10 * rate do
      Ym2413.step t;
      incr n;
      if Ym2413.output t 4 = 0 then incr quiet else quiet := 0
    done;
    !n - !quiet
  in
  let plain = release_samples false and held = release_samples true in
  check (Printf.sprintf "key off releases to silence (%d samples)" plain) (plain > 0 && plain < rate);
  (* sustain on releases at rate 5, this instrument at its RR 7: two rate
     steps, four times slower *)
  let q = float held /. float plain in
  check (Printf.sprintf "sustain on releases 4 times slower (%.2f)" q) (q > 3.6 && q < 4.4);
  (* a half-sine carrier is silent through the other half of each cycle *)
  let t = Ym2413.create () in
  user_patch t [ 0x01; 0x21; 0x3f; 0x10; 0xf0; 0xf0; 0x00; 0x0f ];
  key_on t 5 ~fnum:0x100 ~blk:4 ~vol:0 ~inst:0;
  run t 2000;
  let zeros = ref 0 and positive = ref 0 and negative = ref 0 and n = 20000 in
  for _ = 1 to n do
    Ym2413.step t;
    let v = Ym2413.output t 5 in
    if v = 0 then incr zeros else if v > 0 then incr positive else incr negative
  done;
  let share = float !zeros /. float n in
  check (Printf.sprintf "a half-sine carrier is silent half the time (%.3f)" share) (share > 0.45 && share < 0.55);
  check
    (Printf.sprintf "a half-sine carrier sounds on one side only (%d above, %d below)" !positive !negative)
    (!positive = 0 || !negative = 0)

let rhythm () =
  let t = Ym2413.create () in
  (* channel 6 sounds as melody, then rhythm mode takes it *)
  user_patch t sine;
  key_on t 6 ~fnum:0x100 ~blk:4 ~vol:0 ~inst:0;
  run t 2000;
  check "channel 6 sounds as melody" (fst (measure t 6 1000) > 1000.);
  Ym2413.write_register t 0x36 0x00;
  Ym2413.write_register t 0x0e 0x30;
  Ym2413.step t;
  let sounding = ref [] in
  for _ = 1 to 4000 do
    Ym2413.step t;
    List.iter
      (fun o -> if Ym2413.output t o <> 0 && not (List.mem o !sounding) then sounding := o :: !sounding)
      (List.init Ym2413.outputs Fun.id)
  done;
  check "rhythm mode with the bass drum key sounds output 9 only" (!sounding = [ 9 ]);
  Ym2413.write_register t 0x0e 0x00;
  Ym2413.step t;
  check "leaving rhythm mode silences the drum outputs"
    (List.for_all (fun o -> Ym2413.output t o = 0) [ 9; 10; 11; 12; 13 ]);
  (* each drum key sounds its own output *)
  List.iter
    (fun (bit, out, name) ->
      let t = Ym2413.create () in
      Ym2413.write_register t 0x16 0x20;
      Ym2413.write_register t 0x17 0x50;
      Ym2413.write_register t 0x18 0xc0;
      Ym2413.write_register t 0x26 0x05;
      Ym2413.write_register t 0x27 0x05;
      Ym2413.write_register t 0x28 0x01;
      Ym2413.write_register t 0x0e (0x20 lor bit);
      let others = List.filter (( <> ) out) (List.init Ym2413.outputs Fun.id) in
      let quiet = ref true and peak = ref 0 in
      for _ = 1 to 4000 do
        Ym2413.step t;
        peak := max !peak (abs (Ym2413.output t out));
        if List.exists (fun o -> Ym2413.output t o <> 0) others then quiet := false
      done;
      check (name ^ " sounds") (!peak > 100);
      check (name ^ " sounds alone") !quiet)
    [ (0x10, 9, "bass drum"); (0x08, 11, "snare drum"); (0x04, 12, "tom-tom"); (0x02, 13, "top cymbal");
      (0x01, 10, "hi-hat") ]

(* A ROM instrument sounds exactly like the user instrument loaded with its
   bytes. *)
let rom_instruments () =
  for inst = 1 to 15 do
    let rom = Ym2413.create () and user = Ym2413.create () in
    user_patch user (Array.to_list Ym2413_tables.default_instruments.(inst));
    key_on rom 2 ~fnum:0x15a ~blk:4 ~vol:1 ~inst;
    key_on user 2 ~fnum:0x15a ~blk:4 ~vol:1 ~inst:0;
    let same = ref true and loud = ref 0 in
    for n = 1 to 30000 do
      if n = 15000 then List.iter (fun t -> key_off t 2 ~fnum:0x15a ~blk:4 ~sustain:false) [ rom; user ];
      Ym2413.step rom;
      Ym2413.step user;
      loud := max !loud (abs (Ym2413.output rom 2));
      if Ym2413.output rom 2 <> Ym2413.output user 2 then same := false
    done;
    check (Printf.sprintf "ROM instrument %d sounds" inst) (!loud > 100);
    check (Printf.sprintf "ROM instrument %d is its bytes as the user instrument" inst) !same
  done

let mix () =
  let t = Ym2413.create () in
  user_patch t sine;
  key_on t 0 ~fnum:0x100 ~blk:4 ~vol:0 ~inst:0;
  key_on t 5 ~fnum:0x0c0 ~blk:4 ~vol:2 ~inst:0;
  let ok = ref true in
  for _ = 1 to 5000 do
    Ym2413.step t;
    let sum = List.fold_left (fun a o -> a + Ym2413.output t o) 0 (List.init Ym2413.outputs Fun.id) in
    if Ym2413.mix t <> sum then ok := false
  done;
  check "mix is the sum of the outputs" !ok

(* The fixture: events.txt is a register-write stimulus at chip-sample times,
   nukeykt.txt what openMSX 21.0's NukeYKT core played for it, per output and
   per window: RMS and how often it rises through its mean. fixture.py made
   both. *)
let read_lines path =
  let ic = open_in path in
  let rec go acc = match input_line ic with l -> go (l :: acc) | exception End_of_file -> close_in ic; List.rev acc in
  go []

let words l = List.filter (( <> ) "") (String.split_on_char ' ' l)

(* RMS of a window, and how often it rises through its mean: fixture.py's
   measure. *)
let window_stats a =
  let n = float (Array.length a) in
  let sum = Array.fold_left (fun s v -> s +. float v) 0. a in
  let sq = Array.fold_left (fun s v -> s +. (float v *. float v)) 0. a in
  let mean = sum /. n in
  let zc = ref 0 in
  for i = 1 to Array.length a - 1 do
    if float a.(i - 1) <= mean && mean < float a.(i) then incr zc
  done;
  (sqrt (sq /. n), !zc)

let correlation a b =
  let n = float (Array.length a) in
  let mean x = Array.fold_left ( +. ) 0. x /. n in
  let ma = mean a and mb = mean b in
  let sab = ref 0. and saa = ref 0. and sbb = ref 0. in
  Array.iteri
    (fun i x ->
      let dx = x -. ma and dy = b.(i) -. mb in
      sab := !sab +. (dx *. dy);
      saa := !saa +. (dx *. dx);
      sbb := !sbb +. (dy *. dy))
    a;
  !sab /. sqrt (!saa *. !sbb)

let openmsx () =
  let writes, stop =
    List.fold_left
      (fun (ws, stop) l ->
        match words l with
        | [ "end"; n ] -> (ws, int_of_string n)
        | [ s; r; v ] -> ((int_of_string s, int_of_string r, int_of_string v) :: ws, stop)
        | _ -> failwith ("events.txt: " ^ l))
      ([], 0) (read_lines "ym2413/events.txt")
  in
  let writes = List.rev writes in
  let first = ref 0 and window = ref 0 and windows = ref 0 in
  let theirs_rms = Array.make Ym2413.outputs [||] and theirs_zc = Array.make Ym2413.outputs [||] in
  List.iter
    (fun l ->
      match words l with
      | "#" :: _ -> ()
      | [ "first"; f; "window"; w; "windows"; n ] ->
        first := int_of_string f;
        window := int_of_string w;
        windows := int_of_string n
      | "rms" :: o :: vs -> theirs_rms.(int_of_string o) <- Array.of_list (List.map float_of_string vs)
      | "zc" :: o :: vs -> theirs_zc.(int_of_string o) <- Array.of_list (List.map int_of_string vs)
      | _ -> failwith ("nukeykt.txt: " ^ l))
    (read_lines "ym2413/nukeykt.txt");
  check "the fixture covers the stimulus" (!first + (!windows * !window) <= stop);
  let t = Ym2413.create () in
  let pending = ref writes in
  let buf = Array.make_matrix Ym2413.outputs !window 0 in
  let ours_rms = Array.make_matrix Ym2413.outputs !windows 0. in
  let ours_zc = Array.make_matrix Ym2413.outputs !windows 0 in
  for n = 0 to !first + (!windows * !window) - 1 do
    let rec apply () =
      match !pending with
      | (s, r, v) :: rest when s <= n ->
        Ym2413.write_register t r v;
        pending := rest;
        apply ()
      | _ -> ()
    in
    apply ();
    Ym2413.step t;
    if n >= !first then begin
      let w = (n - !first) / !window and i = (n - !first) mod !window in
      for o = 0 to Ym2413.outputs - 1 do
        buf.(o).(i) <- Ym2413.output t o
      done;
      if i = !window - 1 then
        for o = 0 to Ym2413.outputs - 1 do
          let r, z = window_stats buf.(o) in
          ours_rms.(o).(w) <- r;
          ours_zc.(o).(w) <- z
        done
    end
  done;
  (* openMSX records about 16 times our scale. emu2413 and NukeYKT differ
     in small ways: a fast decay to sustain level 0 overshoots by 0.75 dB in
     emu2413, release tails end sooner, the LFO starts at another phase. The
     noise of the hi-hat, snare drum and top cymbal is not NukeYKT's, so only
     their loudness is compared. *)
  let noise = [ 10; 11; 13 ] in
  for o = 0 to Ym2413.outputs - 1 do
    let ours = ours_rms.(o) and theirs = theirs_rms.(o) in
    let loud = List.filter (fun i -> ours.(i) > 20.) (List.init !windows Fun.id) in
    let sorted f = List.map f loud |> List.sort compare |> Array.of_list in
    let at q a = a.(min (Array.length a - 1) (Array.length a * q / 100)) in
    let corr = correlation ours theirs in
    let ratio = at 50 (sorted (fun i -> theirs.(i) /. ours.(i))) in
    let alone = List.filter (fun i -> theirs.(i) < 1.) loud in
    check (Printf.sprintf "output %d: loudness follows openMSX (corr %.4f)" o corr) (corr >= 0.995);
    check (Printf.sprintf "output %d: one scale with openMSX (x%.2f)" o ratio) (ratio >= 14.5 && ratio <= 17.5);
    check (Printf.sprintf "output %d: sounds only where openMSX does (%d windows)" o (List.length alone)) (alone = []);
    if not (List.mem o noise) then begin
      let off = sorted (fun i -> abs (ours_zc.(o).(i) - theirs_zc.(o).(i))) in
      check (Printf.sprintf "output %d: pitch follows openMSX (median off by %d)" o (at 50 off)) (at 50 off <= 2);
      check (Printf.sprintf "output %d: pitch follows openMSX (90%% off by %d or less)" o (at 90 off)) (at 90 off <= 25)
    end
  done

let () =
  registers ();
  pitch ();
  volume ();
  envelope ();
  rhythm ();
  rom_instruments ();
  mix ();
  openmsx ();
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end;
  print_endline "ym2413: all checks passed"
