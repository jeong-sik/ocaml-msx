(* MSX-MUSIC in the machine: the 16KB ROM in slot 1, ports 0x7C/0x7D to the
   YM2413, its output in Msx.audio, and the save state.

   Where the expected values come from:
   - openMSX MSXMusic: the ROM sits at 0x4000-0x7FFF (base 0x4000, size
     0x4000); port 0x7C latches the register number and 0x7D writes it; both
     are write only.
   - The chip makes one sample per 72 CPU cycles (3579545 / 72 = 49716 Hz),
     so F-number 0x100 at block 4 sounds 49716 * 256 * 16 / 2^19 = 388.4 Hz.
   - A full sine (2048) weighs 12/7 on the PSG scale and the mixer halves
     every level: the peak is 2048 * 12 / 7 / 2 = 1755. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let cpu_hz = 3_579_545
let frame_cycles = 262 * 228
let chip_hz = float cpu_hz /. 72.
let machine () = Msx.create ~machine:{ Msx.ram_kb = 64; vram_kb = 128; roms = [ ""; ""; "" ] }
let music_rom = String.init 0x4000 (fun i -> Char.chr (((i * 7) + 3) land 0xff))

(* PPI port A: every page from slot 1, or every page from slot 0. *)
let all_slot1 = 0x55
let all_slot0 = 0x00

let insert m =
  match Msx.insert_music_rom m music_rom with
  | Ok () -> ()
  | Error e -> failwith ("a 16KB ROM must go in: " ^ e)

let opll m reg v =
  Msx.port_out m 0x7c reg;
  Msx.port_out m 0x7d v

(* A sine on channel 0: the modulator at total level 63 is silent and the
   carrier sustains at full level; F-number 0x100, block 4, volume 0. *)
let fnum = 0x100
let blk = 4

let tone m =
  List.iteri (opll m) [ 0x01; 0x21; 0x3f; 0x00; 0xf0; 0xf0; 0x00; 0x0f ];
  opll m 0x10 (fnum land 0xff);
  opll m 0x30 0x00;
  opll m 0x20 (0x10 lor (blk lsl 1) lor (fnum lsr 8))

let samples m frames =
  Msx.step m ~frames;
  let s = Msx.audio m in
  Array.init (String.length s / 2) (fun i -> String.get_int16_le s (2 * i))

let all_read m lo hi want =
  let ok = ref true in
  for a = lo to hi do
    if Msx.mem_read m a <> want (a - lo) then ok := false
  done;
  !ok

let absent () =
  let m = machine () and baseline = machine () in
  Msx.port_out m 0xa8 all_slot1;
  check "without MSX-MUSIC slot 1 page 1 is empty bus" (all_read m 0x4000 0x7fff (fun _ -> 0xff));
  Msx.port_out m 0xa8 all_slot0;
  tone m;
  check "without MSX-MUSIC ports 0x7C/0x7D reach nothing" (samples m 30 = samples baseline 30);
  check "not even the chip's registers in the save" (Msx.serialize m = Msx.serialize baseline);
  check "a 32KB ROM is refused" (Result.is_error (Msx.insert_music_rom m (String.make 0x8000 'x')));
  check "an empty ROM is refused" (Result.is_error (Msx.insert_music_rom m ""));
  Msx.port_out m 0xa8 all_slot1;
  check "a refused ROM leaves slot 1 empty" (Msx.mem_read m 0x4000 = 0xff)

let visible () =
  let m = machine () in
  insert m;
  Msx.port_out m 0xa8 all_slot1;
  check "slot 1 page 1 shows the ROM"
    (all_read m 0x4000 0x7fff (fun i -> Char.code music_rom.[i]));
  check "slot 1 page 0 stays empty" (all_read m 0x0000 0x3fff (fun _ -> 0xff));
  check "slot 1 page 2 stays empty" (all_read m 0x8000 0xbfff (fun _ -> 0xff));
  check "ports 0x7C/0x7D are write only" (Msx.port_in m 0x7c = 0xff && Msx.port_in m 0x7d = 0xff)

let mean a = float (Array.fold_left ( + ) 0 a) /. float (Array.length a)

let crossings a =
  let mid = mean a and n = ref 0 in
  for i = 1 to Array.length a - 1 do
    if float a.(i - 1) < mid <> (float a.(i) < mid) then incr n
  done;
  !n

(* Half the peak-to-peak swing: unlike the distance from the mean, it does
   not move when a window holds a part cycle. *)
let peak a =
  float (Array.fold_left max min_int a - Array.fold_left min max_int a) /. 2.

let sounding () =
  let m = machine () in
  insert m;
  tone m;
  ignore (samples m 10);
  let s = samples m 60 in
  let seconds = float (60 * frame_cycles) /. float cpu_hz in
  let hz = chip_hz *. float fnum *. (2. ** float blk) /. (2. ** 19.) in
  let want = 2. *. hz *. seconds and got = crossings s in
  check
    (Printf.sprintf "the tone crosses %d times in 60 frames, want %.1f (%.2f Hz)" got want hz)
    (Float.abs (float got -. want) <= want *. 0.003);
  let p = peak s in
  check (Printf.sprintf "a full sine peaks at %.0f, want 1730..1756" p) (p >= 1730. && p <= 1756.);
  check "the PSG is silent, so the tone is centred on 0" (Float.abs (mean s) < 5.);
  insert m;
  (* The first sample still averages the ticks the mixer summed before. *)
  let s = samples m 5 in
  check "inserting again silences the chip"
    (Array.for_all (fun x -> x = 0) (Array.sub s 1 (Array.length s - 1)))

(* Every one of the fourteen outputs reaches the audio: each melody channel
   alone at full level, then each drum alone in rhythm mode (register 0x0E:
   bit 5 rhythm, bits 4-0 bass drum, snare, tom-tom, top cymbal, hi-hat). *)
let outputs () =
  let full = ref [] in
  for ch = 0 to 8 do
    let m = machine () in
    insert m;
    List.iteri (opll m) [ 0x01; 0x21; 0x3f; 0x00; 0xf0; 0xf0; 0x00; 0x0f ];
    opll m (0x10 + ch) (fnum land 0xff);
    opll m (0x30 + ch) 0x00;
    opll m (0x20 + ch) (0x10 lor (blk lsl 1) lor (fnum lsr 8));
    ignore (samples m 3);
    let p = peak (samples m 10) in
    if p < 1730. || p > 1756. then full := Printf.sprintf "%d=%.0f" ch p :: !full
  done;
  check
    (Printf.sprintf "every melody channel peaks at 1730..1756 (off: %s)"
       (String.concat "," (List.rev !full)))
    (!full = []);
  let quiet = ref [] in
  List.iter
    (fun (name, bit) ->
      let m = machine () in
      insert m;
      for ch = 6 to 8 do
        opll m (0x10 + ch) (fnum land 0xff);
        opll m (0x20 + ch) ((blk lsl 1) lor (fnum lsr 8));
        opll m (0x30 + ch) 0x00
      done;
      opll m 0x0e (0x20 lor bit);
      let p = peak (samples m 10) in
      if p < 100. then quiet := Printf.sprintf "%s %.0f" name p :: !quiet)
    [ ("bass drum", 0x10); ("snare", 0x08); ("tom-tom", 0x04); ("top cymbal", 0x02); ("hi-hat", 0x01) ];
  check
    (Printf.sprintf "every drum reaches the audio (quiet: %s)" (String.concat ", " !quiet))
    (!quiet = [])

let magic = "OCAML-MSX\000\010"

let edit_payload state edit =
  let offset = String.length magic + 16 in
  let payload = String.sub state offset (String.length state - offset) |> edit in
  magic ^ Digest.string payload ^ payload

(* The YM2413 block in a save, as test/state_test.ml counts it. *)
let opll_bytes = ((1 + 64 + 2 + 4 + 9 + (18 * 17) + 14) * 8) + 1 + (18 * 4)

let saved () =
  let m = machine () in
  insert m;
  tone m;
  ignore (samples m 7);
  let state = Msx.serialize m in
  (match Msx.restore ~state with
   | Error e -> failwith ("a state with MSX-MUSIC must restore: " ^ e)
   | Ok r ->
     check "a restored machine sounds the same" (samples r 5 = samples m 5);
     check "and saves the same" (Msx.serialize r = Msx.serialize m);
     Msx.port_out r 0xa8 all_slot1;
     check "and still shows the ROM" (Msx.mem_read r 0x4000 = Char.code music_rom.[0]));
  (* A busier chip, saved while all of it sounds: a user instrument with
     feedback 6 on a full-sine modulator, a half-sine carrier, AM and vibrato
     on channel 0; ROM instrument 3 on channel 1; ROM instrument 1 on channel
     2 released with sustain on; rhythm mode with every drum keyed; and a
     volume change written after the last chip sample. *)
  let busy = machine () in
  insert busy;
  List.iteri (opll busy) [ 0xe1; 0xc1; 0x0c; 0x16; 0xf2; 0xf4; 0x23; 0x15 ];
  opll busy 0x10 0x81;
  opll busy 0x30 0x02;
  opll busy 0x20 0x17;
  opll busy 0x11 0x56;
  opll busy 0x31 0x31;
  opll busy 0x21 0x1b;
  for ch = 6 to 8 do
    opll busy (0x10 + ch) (fnum land 0xff);
    opll busy (0x20 + ch) ((blk lsl 1) lor (fnum lsr 8));
    opll busy (0x30 + ch) 0x22
  done;
  opll busy 0x0e 0x3f;
  opll busy 0x12 (fnum land 0xff);
  opll busy 0x32 0x10;
  opll busy 0x22 (0x10 lor (blk lsl 1) lor (fnum lsr 8));
  ignore (samples busy 5);
  opll busy 0x22 (0x20 lor (blk lsl 1) lor (fnum lsr 8));
  ignore (samples busy 4);
  opll busy 0x31 0x36;
  (match Msx.restore ~state:(Msx.serialize busy) with
   | Error e -> failwith ("a busy chip must restore: " ^ e)
   | Ok r ->
     check "a busy chip sounds the same after a restore" (samples r 30 = samples busy 30);
     check "and saves the same" (Msx.serialize r = Msx.serialize busy));
  let rom_at = String.length state - String.length magic - 16 - (8 + 0x4000 + 8 + opll_bytes) in
  let int64_at p payload = Int64.to_int (String.get_int64_be payload p) in
  check "the ROM sits where the layout says"
    (int64_at rom_at (String.sub state (String.length magic + 16) (String.length state - String.length magic - 16))
     = 0x4000);
  let rejected name want state =
    match Msx.restore ~state with
    | Error e -> check (Printf.sprintf "%s (%s)" name e) (e = want)
    | Ok _ -> check name false
  in
  rejected "a 1-byte MSX-MUSIC ROM is rejected" "invalid MSX-MUSIC ROM size"
    (edit_payload state (fun p ->
         let b = Bytes.create 8 in
         Bytes.set_int64_be b 0 1L;
         String.sub p 0 rom_at ^ Bytes.to_string b ^ "x"
         ^ String.sub p (rom_at + 8 + 0x4000) (String.length p - rom_at - 8 - 0x4000)));
  let set_rest n =
    edit_payload state (fun p ->
        let b = Bytes.of_string p in
        Bytes.set_int64_be b (rom_at + 8 + 0x4000) (Int64.of_int n);
        Bytes.to_string b)
  in
  rejected "a cycle remainder of 72 is rejected" "MSX state integer out of range" (set_rest 72);
  check "a cycle remainder of 71 restores" (Result.is_ok (Msx.restore ~state:(set_rest 71)))

let clipping () =
  let out level =
    let mix = Mixer.create () in
    for _ = 1 to 100 do
      Mixer.push mix level
    done;
    let s = Mixer.output mix in
    List.sort_uniq compare (List.init (String.length s / 2) (fun i -> String.get_int16_le s (2 * i)))
  in
  check "a level of 65534 comes out at 32767" (out 65534 = [ 32767 ]);
  check "a level past 16 bits clips to 32767" (out 100000 = [ 32767 ]);
  check "a level of -65536 comes out at -32768" (out (-65536) = [ -32768 ]);
  check "a level below 16 bits clips to -32768" (out (-100000) = [ -32768 ])

let () =
  absent ();
  visible ();
  sounding ();
  outputs ();
  saved ();
  clipping ();
  if !failures > 0 then exit 1;
  print_endline "msx_music: all checks passed"
