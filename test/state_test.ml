let check name cond = if not cond then failwith name
let restored t = match Msx.restore ~state:(Msx.serialize t) with Ok t -> t | Error e -> failwith e
let reg t r v = Msx.port_out t 0x99 v; Msx.port_out t 0x99 (0x80 lor r)
let fresh () =
  let rom = Bytes.make 32768 '\000' in
  (* RAM counter and sampled keyboard input: real CPU execution survives restore. *)
  List.iteri (fun i b -> Bytes.set rom i (Char.chr b))
    [0x31;0x00;0xf0;0x3e;0xc0;0xd3;0xa8;0x21;0x00;0xc0;
     0x34;0xdb;0xa9;0x32;0x01;0xc0;0xc3;0x0a;0x00];
  Msx.create ~machine:{ram_kb=512;vram_kb=128;roms=[Bytes.to_string rom;"";""]}
let same name a b = check name (Msx.serialize a = Msx.serialize b)

let rejection name expected state =
  match Msx.restore ~state with
  | Error message -> check name (message = expected)
  | Ok _ -> failwith name

(* Recompute the envelope after payload edits: these cases must reach the
   decoder, rather than merely fail the corruption checksum. *)
let edit_payload state edit =
  let magic = "OCAML-MSX\000\001" in
  let offset = String.length magic + 16 in
  let payload = String.sub state offset (String.length state - offset) |> edit in
  magic ^ Digest.string payload ^ payload

let fcb_machine () =
  let disk = Bytes.make (1440 * 512) '\000' in
  let u16 off n = Bytes.set disk off (Char.chr (n land 255));
    Bytes.set disk (off + 1) (Char.chr (n lsr 8)) in
  let code off bytes = List.iteri (fun i n -> Bytes.set disk (off + i) (Char.chr n)) bytes in
  u16 0x0b 512; Bytes.set disk 0x0d '\002'; u16 0x0e 1;
  Bytes.set disk 0x10 '\002'; u16 0x11 112; u16 0x13 1440;
  Bytes.set disk 0x15 '\xf9'; u16 0x16 3; u16 0x18 9; u16 0x1a 2;
  List.iter (fun sector -> code (sector * 512) [0xf9;0xff;0xff;0xff;0x0f]) [1;4];
  Bytes.blit_string "PROGRESSDAT" 0 disk (7 * 512) 11;
  Bytes.set disk (7 * 512 + 11) '\x20';
  u16 (7 * 512 + 26) 2; u16 (7 * 512 + 28) 256;
  Bytes.fill disk (14 * 512) 128 '\x11';
  Bytes.fill disk (14 * 512 + 128) 128 '\x22';
  Bytes.blit_string "\000PROGRESSDAT" 0 disk 0xb0 12;
  (* Open FCB C0B0, set DMA C200, read one record, then park at C080. *)
  code 0x1e [0x31;0x00;0xf0; 0x11;0xb0;0xc0; 0x0e;0x0f; 0xcd;0x7d;0xf3;
    0x32;0x00;0xc3; 0x11;0x00;0xc2; 0x0e;0x1a; 0xcd;0x7d;0xf3;
    0x11;0xb0;0xc0; 0x0e;0x14; 0xcd;0x7d;0xf3; 0x32;0x01;0xc3;
    0xc3;0x80;0xc0];
  code 0x80 [0x18;0xfe];
  (* Continue the same open FCB with the same DMA; no reopen or seek. *)
  code 0x90 [0x11;0xb0;0xc0; 0x0e;0x14; 0xcd;0x7d;0xf3;
    0x32;0x02;0xc3; 0xc3;0xa0;0xc0];
  code 0xa0 [0x18;0xfe];
  let t = Msx.create ~machine:{ram_kb=512;vram_kb=128;roms=[]} in
  Msx.port_out t 0xa8 0xc0;
  Msx.load_disk ~interface_rom:false t (Bytes.to_string disk);
  (match Msx.boot_disk t with Ok () -> () | Error e -> failwith e);
  List.iter (fun a -> Msx.mem_write t a 0xff) [0xc300;0xc301;0xc302];
  t

let () =
  let t = fresh () in
  Msx.port_out t 0xaa 8;
  ignore (Msx.set_key t Msx.Space ~pressed:true);
  Msx.step t ~frames:3;
  let state = Msx.serialize t in
  let copy = restored t in
  same "roundtrip complete state" t copy;
  List.iter (fun n ->
    Msx.step t ~frames:n; Msx.step copy ~frames:n;
    same "restored CPU and callbacks produce identical future" t copy;
    check "frame bytes agree" (Msx.frame_rgb t = Msx.frame_rgb copy)
  ) [1;2;7];
  check "guest sampled held keyboard" (Msx.mem_read copy 0xc001 land 1 = 0);
  Msx.mem_write copy 0xc000 0x5a;
  check "restore does not alias original RAM" (Msx.serialize t <> Msx.serialize copy);
  let t = fresh () in
  reg t 0 0x0a; reg t 1 0x40;
  (* Save between the two address bytes. *)
  Msx.port_out t 0x99 0x34;
  let copy = restored t in
  List.iter (fun m -> Msx.port_out m 0x99 0x40; Msx.port_out m 0x98 0xab) [t;copy];
  same "half-written VRAM address latch" t copy;
  reg t 16 5; Msx.port_out t 0x9a 0x72;
  let copy = restored t in
  List.iter (fun m -> Msx.port_out m 0x9a 3) [t;copy];
  same "half-written palette latch" t copy;
  let buffer = fresh () in
  Msx.port_out buffer 0x99 0; Msx.port_out buffer 0x99 0x40;
  List.iter (Msx.port_out buffer 0x98) [0xab;0xcd;0xef];
  Msx.port_out buffer 0x99 0; Msx.port_out buffer 0x99 0;
  check "first VRAM read primes next byte" (Msx.port_in buffer 0x98 = 0xab);
  let buffer_copy = restored buffer in
  List.iter (fun expected ->
    check "original buffered VRAM read" (Msx.port_in buffer 0x98 = expected);
    check "restored buffered VRAM read" (Msx.port_in buffer_copy 0x98 = expected)
  ) [0xcd;0xef];
  same "VRAM prefetch and address continue together" buffer buffer_copy;
  (* Save an active HMMC after first of two bytes. *)
  List.iter (fun (r,v) -> reg t r v) [36,0;37,0;38,0;39,0;40,4;41,0;42,1;43,0;45,0;44,0x12;46,0xf0];
  let active,_,_,_,_ = Msx.tx_state t in check "transfer active at snapshot" active;
  let copy = restored t in
  List.iter (fun m -> reg m 44 0x34) [t;copy];
  same "pending HMMC resumes at exact pixel" t copy;
  let active,_,_,_,_ = Msx.tx_state copy in check "restored transfer completes" (not active);
  let switched = fresh () in
  reg switched 0 0x0a;
  List.iter (fun (r,v) -> reg switched r v)
    [36,255;37,1;38,0;39,0;40,0;41,0;42,1;43,0;45,4;44,0x12;46,0xf0];
  reg switched 0 0x08;
  for _ = 1 to 130 do reg switched 44 0x34 done;
  let active,_,_,_,_ = Msx.tx_state switched in
  check "mode-switched reverse transfer remains pending" active;
  let switched_copy = restored switched in
  same "negative transfer cursor roundtrip after mode switch" switched switched_copy;
  for _ = 1 to 130 do
    reg switched 44 0x56; reg switched_copy 44 0x56
  done;
  same "mode-switched transfer continues identically" switched switched_copy;
  let reversed = fresh () in
  reg reversed 0 0x0a;
  List.iter (fun (r,v) -> reg reversed r v)
    [36,0;37,0;38,0;39,0;40,2;41,0;42,8;43,0;45,0;44,0x12;46,0xf0];
  reg reversed 45 8;
  for _ = 1 to 3 do reg reversed 44 0x34 done;
  let active,_,_,_,dy = Msx.tx_state reversed in
  check "direction-switched transfer has pending negative row" (active && dy < -1);
  let reversed_copy = restored reversed in
  for _ = 1 to 4 do
    reg reversed 44 0x56; reg reversed_copy 44 0x56
  done;
  same "direction-switched transfer continues identically" reversed reversed_copy;
  let t = fresh () in
  Msx.load_cartridge ~mapper:Msx.Ascii8_sram t (String.make 0x40000 '\x42');
  Msx.port_out t 0xa8 0xe8;
  Msx.mem_write t 0x7000 0x20;
  Msx.mem_write t 0x8000 0x5a;
  let copy = restored t in
  check "Koei SRAM content and selected bank restored" (Msx.mem_read copy 0x8000 = 0x5a);
  Msx.mem_write copy 0x8000 0x33;
  check "SRAM does not alias original" (Msx.mem_read t 0x8000 = 0x5a);
  let t = fresh () in
  Msx.load_disk ~interface_rom:false t (String.init 737280 (fun i -> Char.chr (i land 255)));
  same "mounted disk bytes restored" t (restored t);
  let t = fcb_machine () in
  Msx.step t ~frames:1;
  check "FCB opened and first record read before snapshot"
    (Msx.mem_read t 0xc300 = 0 && Msx.mem_read t 0xc301 = 0
     && Msx.dump_pc t = 0xc080);
  check "first record reached selected DMA"
    (List.init 128 (fun i -> Msx.mem_read t (0xc200 + i)) = List.init 128 (fun _ -> 0x11));
  let copy = restored t in
  List.iter (fun m ->
    List.iteri (fun i byte -> Msx.mem_write m (0xc080 + i) byte) [0xc3;0x90;0xc0];
    Msx.step m ~frames:1;
    check "open FCB resumes at second record using saved DMA"
      (Msx.mem_read m 0xc302 = 0 && Msx.dump_pc m = 0xc0a0
       && List.init 128 (fun i -> Msx.mem_read m (0xc200 + i)) = List.init 128 (fun _ -> 0x22))
  ) [t;copy];
  same "BDOS open file position continues identically" t copy;
  List.iter (fun n ->
    check "truncation rejected" (Result.is_error (Msx.restore ~state:(String.sub state 0 n))))
    [0;1;10;26;String.length state / 2;String.length state - 1];
  let broken = Bytes.of_string state in
  Bytes.set broken (Bytes.length broken - 1) '\xff';
  check "checksum detects corruption" (Result.is_error (Msx.restore ~state:(Bytes.to_string broken)));
  check "trailing bytes rejected" (Result.is_error (Msx.restore ~state:(state ^ "extra")));
  check "unknown version rejected" (Result.is_error (Msx.restore ~state:("UNKNOWN" ^ state)));
  rejection "valid checksum with invalid RAM length" "invalid RAM size"
    (edit_payload state (fun payload ->
      let bytes = Bytes.of_string payload in Bytes.set_int64_be bytes 0 0L;
      Bytes.to_string bytes));
  rejection "valid checksum with negative byte length" "MSX state integer out of range"
    (edit_payload state (fun payload ->
      let bytes = Bytes.of_string payload in Bytes.set_int64_be bytes 0 (-1L);
      Bytes.to_string bytes));
  rejection "valid checksum with incomplete payload" "truncated MSX state"
    (edit_payload state (fun payload -> String.sub payload 0 (String.length payload - 1)));
  rejection "valid checksum with extra payload" "trailing data in MSX state"
    (edit_payload state (fun payload -> payload ^ "extra"));
  print_endline "state: CPU continuation, device latches, media and rejection passed"
