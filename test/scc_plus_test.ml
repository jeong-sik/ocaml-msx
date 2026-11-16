(* Konami SCC+ sound cartridge and the SCC-I chip modes.

   Expected values come from openMSX src/memory/MSXSCCPlusCart.cc and
   src/sound/SCC.cc:
   - Compatible mode moves the deformation register to 0xC0 and shows
     waveform 5 at 0xA0; Plus mode gives channel 5 its own waveform at 0x80
     and moves periods and volumes to 0xA0. Outside Real mode the
     deformation register ignores bit 7.
   - The cart maps 8KB RAM blocks through the low 4 bits of its mapper
     registers (Snatcher 0-7, SD Snatcher 8-15, expanded 0-15), opens the SCC
     at 0x9800 or 0xB800, and its mode register at 0xBFFE picks RAM windows
     and the chip mode. RAM powers up 0xFF (openMSX debugger read). *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

(* --- the chip alone --- *)

let chip mode =
  let t = Scc.create () in
  Scc.set_mode t mode;
  t

let ramp t ch = for i = 0 to 31 do Scc.write t ((ch * 32) + i) (i * 4) done
let levels t n = Array.init n (fun _ -> Scc.tick t)

let () =
  (* Compatible: deformation at 0xC0, nothing at 0xE0. 0x40 makes every
     waveform read-only. *)
  let t = chip Compatible in
  Scc.write t 0xe0 0x40;
  Scc.write t 0x00 0x11;
  check "compatible: 0xE0 is not the deformation register" (Scc.read t 0x00 = 0x11);
  Scc.write t 0xc0 0x40;
  Scc.write t 0x00 0x22;
  check "compatible: 0xC0 is the deformation register" (Scc.read t 0x00 = 0x11);

  (* Compatible: waveform 4 is copied to 5 and 5 reads back at 0xA0. *)
  let t = chip Compatible in
  Scc.write t 0x65 0x5a;
  check "compatible: waveform 5 reads at 0xA0" (Scc.read t 0xa5 = 0x5a);
  Scc.write t 0xa5 0x33;
  check "compatible: 0xA0 is read-only" (Scc.read t 0xa5 = 0x5a);
  check "real: 0xA0 reads 0xFF" (let r = chip Real in Scc.write r 0x65 0x5a; Scc.read r 0xa5 = 0xff);

  (* Reading the deformation register writes 0xFF there: bit 7 dropped, the
     0x40 pattern makes waveforms read-only. *)
  let t = chip Compatible in
  ignore (Scc.read t 0xc0);
  Scc.write t 0x00 0x44;
  check "compatible: a read at 0xC0 locks the waveforms" (Scc.read t 0x00 = 0xff);
  let t = chip Compatible in
  ignore (Scc.read t 0xe0);
  Scc.write t 0x00 0x44;
  check "compatible: a read at 0xE0 does nothing" (Scc.read t 0x00 = 0x44);

  (* Bit 7: in Real mode 0x80 locks waveforms 4-5; the SCC-I ignores it. *)
  let real = chip Real in
  Scc.write real 0xe0 0x80;
  Scc.write real 0x60 0x12;
  check "real: deformation 0x80 locks waveform 4" (Scc.read real 0x60 = 0xff);
  let plus = chip Plus in
  Scc.write plus 0xc0 0x80;
  Scc.write plus 0x60 0x12;
  check "plus: deformation bit 7 is ignored" (Scc.read plus 0x60 = 0x12);

  (* Plus: waveform 5 of its own at 0x80, periods and volumes at 0xA0. *)
  let t = chip Plus in
  Scc.write t 0x80 0x20;
  Scc.write t 0x60 0x10;
  check "plus: waveform 4 stays out of 5" (Scc.read t 0x80 = 0x20 && Scc.read t 0x60 = 0x10);
  for i = 0 to 31 do Scc.write t (0x80 + i) 0x40 done;
  Scc.write t 0xa8 100;
  Scc.write t 0xa9 0;
  Scc.write t 0xae 15;
  Scc.write t 0xaf 0x10;
  check "plus: channel 5 plays its own waveform" ((levels t 100).(99) = (0x40 * 15) asr 4);
  check "plus: 0xA0 reads 0xFF" (Scc.read t 0xa0 = 0xff);

  (* Rotation 0x40: channel 4 turns at channel 5's period outside Plus mode,
     at its own in Plus mode. Periods 99 and 199, 1000 ticks of 16 cycles:
     16000 / 100 = 160 steps (byte 0) and 16000 / 200 = 80 steps (byte 16). *)
  let turned mode =
    let t = chip mode in
    ramp t 3;
    let freq_at = match mode with Scc.Plus -> 0xa0 | Real | Compatible -> 0x80 in
    Scc.write t (freq_at + 6) 99;
    Scc.write t (freq_at + 8) 199;
    Scc.write t (match mode with Scc.Real -> 0xe0 | Compatible | Plus -> 0xc0) 0x40;
    ignore (levels t 1000);
    Scc.read t 0x60
  in
  check "compatible: channel 4 turns at channel 5's period" (turned Compatible = 16 * 4);
  check "plus: channel 4 turns at its own period" (turned Plus = 0);

  (* Power-up puts an SCC-I back in Compatible mode; Real stays Real. *)
  let t = chip Plus in
  Scc.power_up t;
  Scc.write t 0x65 0x5a;
  check "power-up: SCC-I back in Compatible mode" (Scc.read t 0xa5 = 0x5a)

(* --- the cartridge in slot 2 --- *)

let machine () = Msx.create ~machine:{ ram_kb = 64; vram_kb = 128; roms = [ ""; ""; "" ] }

(* pages 1 and 2 on slot 2, page 3 on slot 3 *)
let slot2 t = Msx.port_out t 0xa8 0xe8

let cart ?(preload = "") kind =
  let t = machine () in
  Msx.load_cartridge ~mapper:(Msx.Scc_plus kind) t preload;
  slot2 t;
  t

let mode_reg = 0xbffe
let rd = Msx.mem_read
let wr = Msx.mem_write

let () =
  (* A fresh cart's SCC-I is in Compatible mode; a Konami SCC cart plugged
     in after it gets the Real chip back. *)
  let t = cart Snatcher in
  wr t 0x9000 0x3f;
  wr t 0x9860 0x5a;
  check "fresh cart: Compatible mode shows waveform 5 at 0xA0" (rd t 0x98a0 = 0x5a);
  Msx.load_cartridge ~mapper:Msx.Konami_scc t (String.make 0x20000 '\xff');
  slot2 t;
  wr t 0x9000 0x3f;
  wr t 0x9860 0x5a;
  check "Konami SCC after it: Real mode, 0xA0 reads 0xFF" (rd t 0x98a0 = 0xff);

  let t = cart Snatcher in
  check "cart mapper" (Msx.cart_mapper t = Msx.Scc_plus Snatcher);
  check "RAM powers up 0xFF" (rd t 0x4000 = 0xff && rd t 0xbffd = 0xff);
  wr t 0x4000 0x12;
  check "mode 0: window 0 is not RAM" (rd t 0x4000 = 0xff);
  wr t mode_reg 0x10;
  wr t 0x4000 0x12;
  wr t 0xa000 0x34;
  check "mode bit 4: every window is RAM" (rd t 0x4000 = 0x12 && rd t 0xa000 = 0x34);
  wr t 0x5000 3;
  check "mode bit 4: 0x5000 is RAM, not a register" (rd t 0x5000 = 3 && rd t 0x4000 = 0x12);
  wr t mode_reg 0x00;
  wr t 0x7000 0;
  check "mapper register 1 at 0x7000 picks block 0" (rd t 0x6000 = 0x12);
  wr t 0x7000 0x10;
  check "register bits above 4 are ignored" (rd t 0x6000 = 0x12);
  wr t 0x7000 8;
  check "Snatcher: block 8 is not there" (rd t 0x6000 = 0xff);
  check "outside 0x4000-0xBFFF reads 0xFF" (Msx.port_out t 0xa8 0xea; rd t 0x0000 = 0xff);
  slot2 t;

  (* Mode bits 0, 1, 2+5 make windows 0, 1, 2 RAM; window 3 never alone. *)
  let ram_window bits w =
    let t = cart Expanded in
    wr t mode_reg bits;
    let a = 0x4000 + (w * 0x2000) + 0x100 in
    wr t a 0x5a;
    rd t a = 0x5a
  in
  check "bit 0: window 0" (ram_window 0x01 0 && not (ram_window 0x01 1));
  check "bit 1: window 1" (ram_window 0x02 1 && not (ram_window 0x02 0));
  check "bit 2 alone: window 2 stays ROM-side" (not (ram_window 0x04 2));
  check "bits 2 and 5: window 2" (ram_window 0x24 2);
  check "window 3 needs bit 4" (not (ram_window 0x2f 3) && ram_window 0x10 3);

  (* SD Snatcher's cart answers 8-15: register 8 is block 0. *)
  let t = cart ~preload:"\x77" Sd_snatcher in
  check "SD Snatcher: register 0 is empty" (rd t 0x4000 = 0xff);
  wr t 0x5000 8;
  check "SD Snatcher: register 8 is block 0 (preloaded)" (rd t 0x4000 = 0x77);
  wr t 0x5000 0x1f;
  wr t mode_reg 0x10;
  wr t 0x4000 0x66;
  wr t mode_reg 0x00;
  wr t 0x5000 7;
  check "SD Snatcher: register 7 has no block" (rd t 0x4000 = 0xff);
  wr t 0x5000 15;
  check "SD Snatcher: register 15 is block 7" (rd t 0x4000 = 0x66);

  (* Expanded: 128KB, block 15 at the end. *)
  let t = cart ~preload:(String.make 0x1e000 '\000' ^ "\x99") Expanded in
  wr t 0xb000 15;
  check "expanded: register 15 is the last block" (rd t 0xa000 = 0x99);

  (* The SCC: 0x9800 in Compatible mode when register 2 has 0x3F in its low
     six bits, 0xB800 in Plus mode when register 3 has bit 7. *)
  let t = cart Snatcher in
  wr t 0x9800 0x21;
  check "SCC closed: 0x9800 is RAM-side" (rd t 0x9800 = 0xff);
  wr t 0x9000 0x3f;
  wr t 0x9800 0x21;
  check "register 2 = 0x3F opens the SCC at 0x9800" (rd t 0x9800 = 0x21);
  wr t 0x9000 0x7f;
  check "only the low six bits count" (rd t 0x9800 = 0x21);
  wr t mode_reg 0x20;
  check "Plus mode closes 0x9800" (rd t 0x9800 = 0xff);
  wr t 0xb000 0x80;
  check "register 3 bit 7 opens it at 0xB800" (rd t 0xb800 = 0x21);
  wr t 0xb880 0x42;
  check "Plus mode: waveform 5 at 0x80" (rd t 0xb880 = 0x42 && rd t 0xb860 <> 0x42);

  (* Save keeps RAM, registers, the mode register, and with it the chip mode:
     0xA0 reads 0xFF in Plus mode, waveform 5 in Compatible mode. *)
  let s = Msx.serialize t in
  (match Msx.restore ~state:s with
   | Error e -> failwith ("restore: " ^ e)
   | Ok r ->
     check "round trip is stable" (Msx.serialize r = s);
     slot2 r;
     check "restored: SCC at 0xB800, waveform 5 at 0x80 (Plus mode)" (rd r 0xb800 = 0x21 && rd r 0xb880 = 0x42);
     wr r mode_reg 0x00;
     check "restored: mode 0 is Compatible, waveform 5 at 0xA0" (rd r 0x98a0 = 0x42));

  (* Deformation 0x80 leaves an SCC-I's waveforms writable; restore keeps it
     that way (the rotation flags are rebuilt for the chip mode). *)
  let d = cart Snatcher in
  wr d mode_reg 0x20;
  wr d 0xb000 0x80;
  wr d 0xb8c0 0x80;
  (match Msx.restore ~state:(Msx.serialize d) with
   | Error e -> failwith ("restore: " ^ e)
   | Ok r ->
     slot2 r;
     wr r 0xb860 0x37;
     check "restored deformation 0x80: waveform 4 writable" (rd r 0xb860 = 0x37));

  (* Restore rejects a mode register on another cart and a RAM of the wrong
     size; an older format cannot name an SCC+ cart. *)
  let magic v = Printf.sprintf "OCAML-MSX\000%c" (Char.chr v) in
  let payload s = String.sub s 27 (String.length s - 27) in
  let wrap v p = magic v ^ Digest.string p ^ p in
  let be64 n = let b = Bytes.create 8 in Bytes.set_int64_be b 0 (Int64.of_int n); Bytes.to_string b in
  let rejected name want state =
    match Msx.restore ~state with
    | Error e -> check (name ^ ": " ^ e) (e = want)
    | Ok _ -> check (name ^ " must be rejected") false
  in
  let plain = payload (Msx.serialize (machine ())) in
  let n = String.length plain in
  rejected "mode register without an SCC+ cart" "invalid sound cartridge mode"
    (wrap 11 (String.sub plain 0 (n - 8) ^ be64 0x20));
  let p = payload s in
  let skip at = at + 8 + Int64.to_int (String.get_int64_be p at) in
  let cart_at = skip (skip (skip (skip 0))) in
  let short = String.sub p 0 cart_at ^ be64 1 ^ "\xff" ^ String.sub p (skip cart_at) (String.length p - skip cart_at) in
  rejected "wrong RAM size" "invalid sound cartridge RAM size" (wrap 11 short);
  rejected "SCC+ cart in format 9" "MSX state integer out of range"
    (wrap 9 (String.sub p 0 (String.length p - 8)))

(* --- the mix: an SCC+ cart weighs 40 per SCC unit, an SCC MegaROM 27 --- *)

let dc_sample t =
  (* channel 1 at a constant 0x7F, volume 15: 119 *)
  for i = 0 to 31 do wr t (0x9800 + i) 0x7f done;
  wr t 0x9880 100;
  wr t 0x9881 0;
  wr t 0x988a 15;
  wr t 0x988f 0x01;
  Msx.step t ~frames:1;
  String.get_int16_le (Msx.audio t) (2 * 100)

let expected_sample weight =
  let mixer = Mixer.create () in
  for _ = 1 to 600 do Mixer.push mixer (weight * 119) done;
  String.get_int16_le (Mixer.output mixer) (2 * 100)

let () =
  let k = machine () in
  Msx.load_cartridge ~mapper:Msx.Konami_scc k (String.make 0x20000 '\xff');
  slot2 k;
  wr k 0x9000 0x3f;
  let p = cart Snatcher in
  wr p 0x9000 0x3f;
  let ks = dc_sample k and ps = dc_sample p in
  check (Printf.sprintf "SCC MegaROM: mix weight 27 (got %d)" ks) (ks = expected_sample 27);
  check (Printf.sprintf "SCC+ cart: mix weight 40 (got %d)" ps) (ps = expected_sample 40)

(* --- the empty SCC cart: openMSX extension "scc", the SCC mapper with no
   ROM (RomKonamiSCC, share/extensions/scc.xml, volume 11500) --- *)

let scc_cart () =
  let t = machine () in
  Msx.load_cartridge ~mapper:Msx.Scc_cart t "";
  slot2 t;
  t

let () =
  let t = scc_cart () in
  (* every page on slot 2 *)
  Msx.port_out t 0xa8 0xaa;
  check "SCC cart: no ROM, slot 2 reads 0xFF"
    (List.for_all (fun a -> rd t a = 0xff) [ 0x0000; 0x4000; 0x5000; 0x7fff; 0x9000; 0x9800; 0xbfff; 0xc000; 0xffff ]);
  let t = scc_cart () in
  wr t 0x9800 0x5a;
  check "SCC cart: closed SCC reads 0xFF" (rd t 0x9800 = 0xff);
  wr t 0x9000 0x3f;
  wr t 0x9801 0x11;
  (* waveforms power up 0xFF (openMSX SCC::powerUp) *)
  check "SCC cart: a write while closed missed the SCC" (rd t 0x9801 = 0x11 && rd t 0x9800 = 0xff);
  wr t 0x9800 0x5a;
  check "SCC cart: 0x9000 = 0x3F opens the SCC at 0x9800" (rd t 0x9800 = 0x5a && rd t 0x9000 = 0xff);
  wr t 0x9860 0x33;
  check "SCC cart: a Real SCC, 0xA0 reads 0xFF" (rd t 0x98a0 = 0xff);
  wr t 0x9000 0x00;
  check "SCC cart: 0x9000 = 0x00 closes it" (rd t 0x9800 = 0xff);
  wr t 0x97ff 0x7f;
  check "SCC cart: 0x9000-0x97FF all open it, low 6 bits 0x3F" (rd t 0x9800 = 0x5a);
  let t = scc_cart () in
  List.iter (fun a -> wr t a 0x3f) [ 0x5000; 0x7000; 0x8fff; 0x9800; 0xb000 ];
  wr t 0x9801 0x11;
  check "SCC cart: only 0x9000-0x97FF opens it" (rd t 0x9801 = 0xff);
  check "SCC cart: load_cartridge refuses a ROM"
    (try Msx.load_cartridge ~mapper:Msx.Scc_cart (machine ()) "\xff"; false with Invalid_argument _ -> true);

  (* Save keeps the open SCC and its waveforms; a ROM on it, or an older
     format, is rejected. *)
  let t = scc_cart () in
  wr t 0x9000 0x3f;
  wr t 0x9805 0x66;
  let s = Msx.serialize t in
  (match Msx.restore ~state:s with
   | Error e -> failwith ("restore: " ^ e)
   | Ok r ->
     check "SCC cart: round trip is stable" (Msx.serialize r = s);
     slot2 r;
     check "SCC cart: restored open, waveform kept" (Msx.cart_mapper r = Msx.Scc_cart && rd r 0x9805 = 0x66));
  let magic v = Printf.sprintf "OCAML-MSX\000%c" (Char.chr v) in
  let payload s = String.sub s 27 (String.length s - 27) in
  let wrap v p = magic v ^ Digest.string p ^ p in
  let be64 n = let b = Bytes.create 8 in Bytes.set_int64_be b 0 (Int64.of_int n); Bytes.to_string b in
  let rejected name want state =
    match Msx.restore ~state with
    | Error e -> check (name ^ ": " ^ e) (e = want)
    | Ok _ -> check (name ^ " must be rejected") false
  in
  let p = payload s in
  let skip at = at + 8 + Int64.to_int (String.get_int64_be p at) in
  let cart_at = skip (skip (skip (skip 0))) in
  let with_rom = String.sub p 0 cart_at ^ be64 1 ^ "\xff" ^ String.sub p (skip cart_at) (String.length p - skip cart_at) in
  rejected "SCC cart with a ROM" "invalid SCC cartridge ROM" (wrap 11 with_rom);
  rejected "SCC cart in format 9" "MSX state integer out of range"
    (wrap 9 (String.sub p 0 (String.length p - 8)));

  (* The mix: the empty SCC cart's 11500 weighs 35 per SCC unit. *)
  let t = scc_cart () in
  wr t 0x9000 0x3f;
  let v = dc_sample t in
  check (Printf.sprintf "SCC cart: mix weight 35 (got %d)" v) (v = expected_sample 35)

let () =
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "scc+ cart: ok"
