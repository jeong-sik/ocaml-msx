(* PAC SRAM cart. Expected values come from openMSX src/memory/MSXPac.cc and
   src/memory/SRAM.cc:
   - 0x5FFE = 0x4D and 0x5FFF = 0x69 open the 0x1FFE-byte SRAM at
     0x4000-0x5FFD; the registers read back only while it is open.
   - Closed, the cart reads 0xFF and drops stores into the SRAM.
   - 0x6000-0x7FFF reads 0xFF.
   - The file is "PAC2 BACKUP DATA" followed by the SRAM.
   - A new SRAM is 0xFF: openMSX with a fresh PAC showed no other byte in
     134 games that did not write it. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let opened () =
  let p = Pac.create () in
  Pac.write p 0x5ffe 0x4d;
  Pac.write p 0x5fff 0x69;
  p

let () =
  let p = Pac.create () in
  check "new PAC: closed, reads 0xFF"
    (List.for_all (fun a -> Pac.read p a = 0xff) [ 0x4000; 0x5ffd; 0x5ffe; 0x5fff; 0x6000; 0x7fff ]);
  Pac.write p 0x4000 0x12;
  Pac.write p 0x5ffe 0x4d;
  check "one register is not enough" (Pac.read p 0x4000 = 0xff && Pac.read p 0x5ffe = 0xff);
  Pac.write p 0x5fff 0x69;
  check "0x4D, 0x69 open it; the store made while closed was dropped" (Pac.read p 0x4000 = 0xff);
  check "open: registers read back" (Pac.read p 0x5ffe = 0x4d && Pac.read p 0x5fff = 0x69);
  Pac.write p 0x4000 0x12;
  Pac.write p 0x5ffd 0x34;
  check "open: SRAM 0x4000-0x5FFD stores" (Pac.read p 0x4000 = 0x12 && Pac.read p 0x5ffd = 0x34);
  (* 0x6000 is 0x2000 into the page: no mirror of the SRAM's first bytes. *)
  List.iter (fun a -> Pac.write p a 0x9a) [ 0x4001; 0x4002 ];
  Pac.write p 0x6000 0x56;
  check "open: 0x6000-0x7FFF reads 0xFF"
    (List.for_all (fun a -> Pac.read p a = 0xff) [ 0x6000; 0x6001; 0x6002; 0x7ffe; 0x7fff ]);
  Pac.write p 0x7ffe 0x00;
  check "0x7FFE is not a register" (Pac.read p 0x5ffe = 0x4d);
  Pac.write p 0x5fff 0x68;
  check "another value closes it" (Pac.read p 0x4000 = 0xff && Pac.read p 0x5ffe = 0xff);
  Pac.write p 0x5fff 0x69;
  check "reopened: the SRAM kept its bytes" (Pac.read p 0x4000 = 0x12);
  let q = Pac.create () in
  Pac.write q 0x5fff 0x69;
  Pac.write q 0x5ffe 0x4d;
  check "either order opens it" (Pac.read q 0x5ffe = 0x4d);

  (* The file. *)
  let blank = Pac.to_file (Pac.create ()) in
  check "file: 8206 bytes, header then 0xFF"
    (String.length blank = 8206 && String.sub blank 0 16 = "PAC2 BACKUP DATA"
     && String.for_all (( = ) '\xff') (String.sub blank 16 8190));
  let p = opened () in
  Pac.write p 0x4000 0xab;
  Pac.write p 0x5ffd 0xcd;
  (match Pac.of_file (Pac.to_file p) with
   | Error e -> check ("file round trip: " ^ e) false
   | Ok r ->
     check "file round trip: loads closed" (Pac.read r 0x4000 = 0xff);
     Pac.write r 0x5ffe 0x4d;
     Pac.write r 0x5fff 0x69;
     check "file round trip: SRAM kept" (Pac.read r 0x4000 = 0xab && Pac.read r 0x5ffd = 0xcd));
  check "file: wrong size rejected" (Result.is_error (Pac.of_file (String.sub blank 0 8205)));
  check "file: wrong header rejected"
    (Result.is_error (Pac.of_file ("PAC2 BACKUP DATX" ^ String.sub blank 16 8190)))

(* --- in slot 1 --- *)

let machine () = Msx.create ~machine:{ ram_kb = 64; vram_kb = 128; roms = [ ""; ""; "" ] }

(* port 0xA8: page 1 on slot [s], page 3 on slot 3 *)
let page1 t s = Msx.port_out t 0xa8 (0xc0 lor (s lsl 2))

let () =
  let t = machine () in
  page1 t 1;
  Msx.mem_write t 0x5ffe 0x4d;
  Msx.mem_write t 0x5fff 0x69;
  Msx.mem_write t 0x4000 0x12;
  check "no PAC: slot 1 reads 0xFF" (Msx.mem_read t 0x4000 = 0xff && Msx.mem_read t 0x5ffe = 0xff);
  check "no PAC: Msx.pac is None" (Msx.pac t = None);
  let t = machine () in
  let p = Pac.create () in
  Msx.insert_pac t p;
  page1 t 1;
  Msx.mem_write t 0x5ffe 0x4d;
  Msx.mem_write t 0x5fff 0x69;
  Msx.mem_write t 0x4000 0x12;
  check "PAC in slot 1, page 1" (Msx.mem_read t 0x4000 = 0x12 && Msx.mem_read t 0x5fff = 0x69);
  check "the caller's handle sees the store" (Pac.read p 0x4000 = 0x12);
  Msx.port_out t 0xa8 0xd5;
  check "slot 1 page 0 and 2 are empty" (Msx.mem_read t 0x0000 = 0xff && Msx.mem_read t 0x8000 = 0xff);
  page1 t 2;
  check "slot 2 page 1 is not the PAC" (Msx.mem_read t 0x5fff <> 0x69)

(* --- save state --- *)

let magic v = Printf.sprintf "OCAML-MSX\000%c" (Char.chr v)
let payload s = String.sub s 27 (String.length s - 27)
let wrap v p = magic v ^ Digest.string p ^ p

let () =
  let t = machine () in
  let p = Pac.create () in
  Msx.insert_pac t p;
  page1 t 1;
  Msx.mem_write t 0x5ffe 0x4d;
  Msx.mem_write t 0x5fff 0x69;
  Msx.mem_write t 0x4abc 0x77;
  let s = Msx.serialize t in
  (match Msx.restore ~state:s with
   | Error e -> check ("restore: " ^ e) false
   | Ok r ->
     check "round trip is stable" (Msx.serialize r = s);
     page1 r 1;
     check "restored: PAC open, SRAM kept" (Msx.mem_read r 0x4abc = 0x77 && Msx.mem_read r 0x5ffe = 0x4d);
     match Msx.pac r with
     | None -> check "restored: PAC present" false
     | Some q -> check "restored: a PAC of its own" (q != p && Pac.read q 0x4abc = 0x77));
  (* The same machine and writes, with slot 1 empty. *)
  let bare = machine () in
  page1 bare 1;
  Msx.mem_write bare 0x5ffe 0x4d;
  Msx.mem_write bare 0x5fff 0x69;
  Msx.mem_write bare 0x4abc 0x77;
  (match Msx.restore ~state:(Msx.serialize bare) with
   | Error e -> check ("restore without PAC: " ^ e) false
   | Ok r -> check "restored without PAC" (Msx.pac r = None));

  let with_pac = payload (Msx.serialize t) and without = payload (Msx.serialize bare) in
  let pac_at = String.length without - 1 in
  let v8 = String.sub without 0 pac_at in
  (match Msx.restore ~state:(wrap 8 v8) with
   | Error e -> check ("v8 restore: " ^ e) false
   | Ok r -> check "v8 restores with slot 1 empty"
       (Msx.pac r = None && Msx.serialize r = Msx.serialize bare));

  (* The canonical v5 payload ends before RTC, NEO, the IRQ latch and PAC. *)
  List.iter (fun high ->
    let t = machine () in
    Msx.port_out t 0xaa (high lsl 4);
    let saved = Msx.serialize t in
    let data = payload saved in
    let newer_bytes = (5 * 8) + 2 in
    let v5 = String.sub data 0 (String.length data - newer_bytes) in
    match Msx.restore ~state:(wrap 5 v5) with
    | Error e -> check ("v5 restore: " ^ e) false
    | Ok restored ->
      check "v5 keeps the port C high nibble and the PAC slot empty"
        (Msx.port_in restored 0xaa lsr 4 = high && Msx.pac restored = None))
    [ 0; 15 ];

  (* After the flag: the SRAM as an 8-byte length and its bytes. *)
  let sram_at = pac_at + 1 in
  let rest = sram_at + 8 + 8190 in
  let short =
    String.sub with_pac 0 sram_at
    ^ (let b = Bytes.create 8 in Bytes.set_int64_be b 0 8189L; Bytes.to_string b)
    ^ String.sub with_pac (sram_at + 8) 8189
    ^ String.sub with_pac rest (String.length with_pac - rest)
  in
  (match Msx.restore ~state:(wrap 9 short) with
   | Error e -> check (Printf.sprintf "short SRAM rejected (%s)" e) (e = "invalid PAC SRAM size")
   | Ok _ -> check "short SRAM must be rejected" false)

let () =
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "pac_test: all pass"
