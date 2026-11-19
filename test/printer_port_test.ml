(* The printer port of the NMS 8250 (openMSX MSXPrinterPort, S-3527 setup):
   every port 0x90-0x97 reads the status, and with no printer plugged that
   is BUSY (bit 1) with the unused bits 0. *)

let check name cond =
  if not cond then begin
    Printf.eprintf "printer_port_test: FAIL %s\n%!" name;
    exit 1
  end

let () =
  let t = Msx.create ~machine:{ Msx.ram_kb = 64; vram_kb = 128; roms = [ ""; ""; "" ] } in
  for p = 0x90 to 0x97 do
    check (Printf.sprintf "port %02x reads not ready" p) (Msx.port_in t p = 0x02)
  done;
  (* strobe and data writes go nowhere *)
  Msx.port_out t 0x90 0x00;
  Msx.port_out t 0x91 0x41;
  check "status after writes" (Msx.port_in t 0x90 = 0x02);
  check "port 8f is not the printer" (Msx.port_in t 0x8f = 0xff);
  print_endline "printer_port_test: all pass"
