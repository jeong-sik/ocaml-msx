(* Save states must round-trip the WD2793-visible drive state (ocaml-msx #46,
   format v3): [Msx.serialize] used to persist only [disk_dma] and
   [fdc.disk_changed], so a restored machine redrove cold — motor off, track
   register 0, and any in-flight READ SECTOR (busy/drq/intr, the sector buffer
   and its pump position) lost — and two machines differing only in drive
   state serialized to identical bytes. The checks below pin the fix: drive
   state survives save -> restore, and an in-flight read keeps its buffer. *)

let check name cond = if not cond then failwith name
let restored t = match Msx.restore ~state:(Msx.serialize t) with
  | Ok t -> t | Error e -> failwith e

let machine disk =
  let rom = Bytes.make 32768 '\000' in
  (* JP (HL) at 0x0000 so a stray boot keeps halting harmlessly. *)
  Bytes.set rom 0 (Char.chr 0x18);
  let t = Msx.create ~machine:{ram_kb=512; vram_kb=128; roms=[Bytes.to_string rom; ""; ""]} in
  if Bytes.length disk > 0 then
    Msx.load_disk ~interface_rom:false t (Bytes.to_string disk);
  t

let () =
  let disk = Bytes.make (720 * 512) '\000' in
  Bytes.iteri (fun i _ -> Bytes.set disk i (Char.chr (i land 255))) disk;

  (* 1. Motor + track register survive save -> restore. *)
  let a = machine disk in
  Msx.port_out a 0xD4 0x08;             (* system control: motor on, side 0 *)
  Msx.port_out a 0xD1 5;                (* track register = 5 *)
  let b = machine disk in               (* motor off, track 0 *)
  check "motor/track divergence survives serialization"
    (Msx.serialize a <> Msx.serialize b);
  let ra = restored a in
  check "restore keeps the motor bit" (Msx.port_in ra 0xD0 land 0x20 <> 0);
  check "restore keeps the track register" (Msx.port_in ra 0xD1 = 5);
  check "pre-restore machine kept the drive state"
    (Msx.port_in a 0xD0 land 0x20 <> 0 && Msx.port_in a 0xD1 = 5);

  (* 2. An in-flight READ SECTOR keeps its command state (drq, sector
     buffer, pump position) across save -> restore. *)
  let a = machine disk in
  Msx.port_out a 0xD4 0x08;
  Msx.port_out a 0xD1 3;                (* track 3 *)
  Msx.port_out a 0xD2 1;                (* sector 1 *)
  Msx.port_out a 0xD0 0x88;             (* READ SECTOR: immediate-completion pump *)
  check "sector buffer contents read back" (Msx.port_in a 0xD3 = 3 * 1024 * 2 land 255);
  let b = machine disk in
  check "pending read state survives serialization"
    (Msx.serialize a <> Msx.serialize b);
  check "restore keeps DRQ lit" (Msx.port_in a 0xD0 land 0x02 <> 0);
  let ra = restored a in
  check "restore keeps DRQ lit" (Msx.port_in ra 0xD0 land 0x02 <> 0);
  check "restore keeps the buffered byte" (Msx.port_in ra 0xD3 = Msx.port_in a 0xD3)
