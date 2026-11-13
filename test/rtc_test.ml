(* RTC (RP5C01) registers at power-on, read through ports 0xB4/0xB5 the
   way C-BIOS games do. openMSX starts the register SRAM at 0xFF and sets
   only the time from the host clock, so the alarm block and the RAM
   blocks 2 and 3 read 0xF. Replicart (C-BIOS) reads block 2 register 3
   and takes a different path on 0. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let m = Msx.create ~machine:{ Msx.ram_kb = 64; vram_kb = 128; roms = [ ""; ""; "" ] }
let mode_reg = 13
let timer_enable = 0x08

let read block r =
  Msx.port_out m 0xb4 mode_reg;
  Msx.port_out m 0xb5 (timer_enable lor block);
  Msx.port_out m 0xb4 r;
  Msx.port_in m 0xb5 land 0x0f

let () =
  for r = 0 to 12 do
    check (Printf.sprintf "block 2 register %d reads 0xF" r) (read 2 r = 0x0f);
    check (Printf.sprintf "block 3 register %d reads 0xF" r) (read 3 r = 0x0f);
    check (Printf.sprintf "time register %d reads 0" r) (read 0 r = 0)
  done;
  (* The alarm block shows its masks; register 10 (24-hour mode) reads 1
     and the leap-year counter (register 11) is part of the time. *)
  let alarm = [ 0x0; 0x0; 0xf; 0x7; 0xf; 0x3; 0x7; 0xf; 0x3; 0x0; 0x1; 0x0; 0x0 ] in
  List.iteri (fun r v -> check (Printf.sprintf "alarm register %d" r) (read 1 r = v)) alarm;
  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "rtc_test: all pass"
