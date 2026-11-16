(* What the CPU reads from the RTC ports. openMSX MSXRTC::readIO returns the
   RP5C01 nibble ORed with 0xF0 on port 0xB5, and the C-BIOS MSX2 machine maps
   only 0xB5 for input, so port 0xB4 reads as an empty port (0xFF). *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let machine () =
  Msx.create ~machine:{ Msx.ram_kb = 64; vram_kb = 128; roms = [ ""; ""; "" ] }

let read_reg t r =
  Msx.port_out t 0xB4 r;
  Msx.port_in t 0xB5

let write_reg t r v =
  Msx.port_out t 0xB4 r;
  Msx.port_out t 0xB5 v

let () =
  let t = machine () in
  (* Block 2 (mode 0x0A, timer enabled): every register keeps four bits. *)
  write_reg t 13 0x0a;
  write_reg t 3 0x08;
  check "data reads the nibble with the high bits set" (read_reg t 3 = 0xf8);
  write_reg t 4 0x00;
  check "a zero nibble reads 0xF0" (read_reg t 4 = 0xf0);
  write_reg t 5 0xff;
  check "only the low nibble is stored" (read_reg t 5 = 0xff);
  check "the mode register reads with the high bits set" (read_reg t 13 = 0xfa);
  check "TEST reads 0xFF" (read_reg t 14 = 0xff);
  check "RESET reads 0xFF" (read_reg t 15 = 0xff);
  (* Block 1 register 0 has no bits: it reads only the high bits. *)
  write_reg t 13 0x09;
  check "a register without bits reads 0xF0" (read_reg t 0 = 0xf0);
  Msx.port_out t 0xB4 3;
  check "port 0xB4 reads as an empty port" (Msx.port_in t 0xB4 = 0xff);
  if !failures > 0 then begin
    Printf.eprintf "rtc_port_test: %d failure(s)\n" !failures;
    exit 1
  end;
  print_endline "rtc_port_test: all passed"
