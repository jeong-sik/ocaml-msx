(* Power-on RAM: every byte of every mapper segment reads 0xFF, as openMSX
   Ram::clear leaves a machine whose config has no initialContent (the C-BIOS
   machines). The RAM mapper sits in slot 3-2; page 0 walks the segments
   through port 0xFC. *)

let ram_kb = 512
let segment_bytes = 0x4000
let ram_sub_slot_everywhere = 0xaa
let slot3_everywhere = 0xff

let () =
  let t =
    Msx.create ~machine:{ Msx.ram_kb; vram_kb = 128; roms = [ ""; ""; "" ] }
  in
  Msx.port_out t 0xa8 slot3_everywhere;
  Msx.mem_write t 0xffff ram_sub_slot_everywhere;
  let segments = ram_kb * 1024 / segment_bytes in
  let first_bad = ref None in
  let bad = ref 0 in
  for seg = 0 to segments - 1 do
    Msx.port_out t 0xfc seg;
    for off = 0 to segment_bytes - 1 do
      let v = Msx.mem_read t off in
      if v <> 0xff then begin
        incr bad;
        if !first_bad = None then first_bad := Some (seg, off, v)
      end
    done
  done;
  match !first_bad with
  | None -> print_endline "ram_power_on_test: all pass"
  | Some (seg, off, v) ->
    Printf.eprintf
      "FAIL %d RAM bytes are not 0xFF at power-on; first: segment %d offset \
       0x%04x reads 0x%02x\n%!"
      !bad seg off v;
    exit 1
