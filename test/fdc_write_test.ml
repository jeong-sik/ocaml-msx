(* READ SECTOR and WRITE SECTOR through the NMS8250 register window
   (0x7FF8-0x7FFF in slot 3-3), driven by Z80 code the way the real disk
   ROM drives it: poll 0x7FFF, move one byte through 0x7FFB on each DRQ,
   stop on INTRQ. The controller keeps its own time (lib/wd2793.ml), so
   the program has to keep up with the disk: a byte every 114 cycles.
   Before the WRITE SECTOR fix the model routed 0xA0/0xB0 to READ SECTOR
   and dropped every byte written to the data register, so a game save on
   a real-ROM boot left the image untouched with no error. A save taken in
   the middle of a transfer must resume it. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let sector_bytes = 512
let sectors_per_track = 9
let disk_sectors = 1440

(* Window addresses. *)
let command = 0x7ff8 and track_reg = 0x7ff9 and sector_reg = 0x7ffa and data_reg = 0x7ffb
and side_reg = 0x7ffc and drive_reg = 0x7ffd and lines = 0x7fff

(* RAM in page 3: the transfer buffer, then what the program reports. *)
let buffer = 0xc000
let status_at = 0xd000
let count_at = 0xd002
let done_at = 0xd004
let done_mark = 0xa5
let stack = 0xf000

let read_sector = 0x80
let write_sector = 0xa0
let write_multi = 0xb0
let record_not_found = 0x10
let error_bits = 0x5c (* write protect, record not found, CRC, lost data *)

(* A two-pass assembler for the few instructions the programs need. *)
type item = Op of int list | Label of string | Jr of int * string

let lo n = n land 0xff
let hi n = (n lsr 8) land 0xff
let ld_a n = Op [ 0x3e; n ]
let store_a addr = Op [ 0x32; lo addr; hi addr ]
let load_a addr = Op [ 0x3a; lo addr; hi addr ]
let jr_nz l = Jr (0x20, l)
let jr_nc l = Jr (0x30, l)
let jr_c l = Jr (0x38, l)
let jr l = Jr (0x18, l)

let assemble items =
  let size = function Op b -> List.length b | Label _ -> 0 | Jr _ -> 2 in
  let labels = Hashtbl.create 8 in
  ignore
    (List.fold_left
       (fun pc it ->
         (match it with Label l -> Hashtbl.replace labels l pc | _ -> ());
         pc + size it)
       0 items);
  let _, out =
    List.fold_left
      (fun (pc, acc) it ->
        match it with
        | Op b -> (pc + List.length b, List.rev_append b acc)
        | Label _ -> (pc, acc)
        | Jr (op, l) ->
          let d = Hashtbl.find labels l - (pc + 2) in
          if d < -128 || d > 127 then failwith "jr out of range";
          (pc + 2, lo d :: op :: acc))
      (0, []) items
  in
  List.rev out

(* Program pieces. [setup] stops the power-on RESTORE, turns drive A's
   motor on and tells the controller the head is on track 0. *)
let setup =
  [ Op [ 0xf3 ]; (* DI *)
    Op [ 0x31; lo stack; hi stack ];
    ld_a 0xd0; store_a command;
    ld_a 0x80; store_a drive_reg;
    Op [ 0xaf ]; store_a track_reg ]

(* SEEK to [track] at 6 ms a step and wait for INTRQ. *)
let seek_to ~track =
  [ ld_a track; store_a data_reg;
    ld_a 0x10; store_a command;
    Label "seek"; load_a lines; Op [ 0xe6; 0x40 ]; jr_nz "seek" ]

(* Run [cmd] on [sector], moving bytes between the window and [buf] on
   each DRQ until INTRQ. [side] is written first unless it is None: a
   side write makes the drive drop the track it holds (openMSX
   RealDrive::setSide). Leaves the next buffer address at [count_at] and
   the status at [status_at]. [id] keeps labels apart. *)
let transfer ~id ~cmd ?side ~sector ~buf () =
  let writing = cmd land 0xe0 = 0xa0 in
  let l name = name ^ id in
  (match side with Some sd -> [ ld_a sd; store_a side_reg ] | None -> [])
  @ [ ld_a sector; store_a sector_reg;
    Op [ 0x21; lo buf; hi buf ]; (* LD HL,buf *)
    Op [ 0x22; lo count_at; hi count_at ]; (* LD (count),HL *)
    ld_a cmd; store_a command;
    Label (l "poll"); load_a lines;
    Op [ 0x17 ]; jr_nc (l "byte"); (* bit 7 low: DRQ *)
    Op [ 0x17 ]; jr_c (l "poll"); (* bit 6 high: no INTRQ yet *)
    jr (l "done");
    Label (l "byte") ]
  @ (if writing then [ Op [ 0x7e ]; store_a data_reg ] (* LD A,(HL) *)
     else [ load_a data_reg; Op [ 0x77 ] ]) (* LD (HL),A *)
  @ [ Op [ 0x23 ]; (* INC HL *)
      Op [ 0x22; lo count_at; hi count_at ];
      jr (l "poll");
      Label (l "done"); load_a command; store_a status_at ]

let finish = [ ld_a done_mark; store_a done_at; Op [ 0x76 ] ] (* HALT *)

(* Seek to [track], run [cmd] on [side]/[sector] with [buffer], then
   leave [done_mark] at [done_at]. *)
let program ~cmd ~track ~side ~sector =
  assemble (setup @ seek_to ~track @ transfer ~id:"" ~cmd ~side ~sector ~buf:buffer () @ finish)

(* Every byte of the image is [i land 255], so a write shows against it. *)
let blank () = String.init (disk_sectors * sector_bytes) (fun i -> Char.chr (i land 255))

let machine ~code disk =
  let rom = Bytes.make 32768 '\000' in
  List.iteri (fun i b -> Bytes.set rom i (Char.chr b)) code;
  let t =
    Msx.create
      ~machine:{ ram_kb = 512; vram_kb = 128; roms = [ Bytes.to_string rom; ""; "" ] }
  in
  let disk_rom =
    match Msx.disk_rom_of_string (String.make 0x4000 '\000') with
    | Ok r -> r
    | Error e -> failwith e
  in
  Msx.load_disk ~disk_rom t disk;
  (* Page 0 on slot 0 (the program), page 1 on slot 3-3 (the disk ROM and
     its window), page 3 on slot 3-2 (RAM), as the NMS8250 is wired. *)
  Msx.port_out t 0xA8 0xCC;
  Msx.mem_write t 0xffff 0x8C;
  t

let peek16 t a = Msx.mem_read t a lor (Msx.mem_read t (a + 1) lsl 8)
let finished t = Msx.mem_read t done_at = done_mark
let moved t = peek16 t count_at - buffer
let status t = Msx.mem_read t status_at

let run_to_end t =
  let rec go n = if finished t || n = 0 then () else (Msx.step t ~frames:1; go (n - 1)) in
  go 300;
  if not (finished t) then failwith "the program never saw INTRQ"

let logical ~track ~side ~sector =
  (((track * 2) + side) * sectors_per_track) + (sector - 1)

let image t = match Msx.disk_image t with Some s -> s | None -> failwith "no disk"
let sector_of t n = String.sub (image t) (n * sector_bytes) sector_bytes

let original n =
  String.init sector_bytes (fun i -> Char.chr (((n * sector_bytes) + i) land 255))

let pattern seed = String.init (2 * sector_bytes) (fun i -> Char.chr (((i * 7) + seed) land 255))

let fill t s = String.iteri (fun i c -> Msx.mem_write t (buffer + i) (Char.code c)) s
let buffer_of t n = String.init n (fun i -> Char.chr (Msx.mem_read t (buffer + i)))

(* Step one frame at a time until the transfer is part way, then return. *)
let step_into_transfer t =
  let rec go n =
    if n = 0 then failwith "no frame boundary inside the transfer"
    else begin
      Msx.step t ~frames:1;
      let k = moved t in
      if k > 0 && k < sector_bytes && not (finished t) then () else go (n - 1)
    end
  in
  go 300

let () =
  (* 1. READ SECTOR: track 2, side 1, sector 5. *)
  let n = logical ~track:2 ~side:1 ~sector:5 in
  let t = machine ~code:(program ~cmd:read_sector ~track:2 ~side:1 ~sector:5) (blank ()) in
  run_to_end t;
  check "read: 512 bytes moved" (moved t = sector_bytes);
  check "read: the sector's bytes" (buffer_of t sector_bytes = original n);
  check "read: no error bits" (status t land error_bits = 0);

  (* 2. WRITE SECTOR on the same sector. *)
  let data = pattern 3 in
  let t = machine ~code:(program ~cmd:write_sector ~track:2 ~side:1 ~sector:5) (blank ()) in
  fill t data;
  run_to_end t;
  check "write: 512 bytes taken" (moved t = sector_bytes);
  check "write: the sector is on the disk" (sector_of t n = String.sub data 0 sector_bytes);
  check "write: neighbours untouched"
    (sector_of t (n - 1) = original (n - 1) && sector_of t (n + 1) = original (n + 1));
  check "write: no error bits" (status t land error_bits = 0);

  (* 3. Multi-sector WRITE from sector 8 of track 0 side 0: sectors 8 and
     9, then sector 10 is not on the track and the command ends with record
     not found. Track 0 side 1 sector 1 must not take a third sector. *)
  let s8 = logical ~track:0 ~side:0 ~sector:8 in
  let next_side = logical ~track:0 ~side:1 ~sector:1 in
  let data = pattern 11 in
  let t = machine ~code:(program ~cmd:write_multi ~track:0 ~side:0 ~sector:8) (blank ()) in
  fill t data;
  run_to_end t;
  check "multi: 1024 bytes taken" (moved t = 2 * sector_bytes);
  check "multi: sectors 8 and 9 written"
    (sector_of t s8 ^ sector_of t (s8 + 1) = data);
  check "multi: ends with record not found" (status t land record_not_found <> 0);
  check "multi: the next side is untouched" (sector_of t next_side = original next_side);

  (* 4. Sector 0 is on no track: the write takes no byte and ends with
     record not found after five turns. *)
  let t = machine ~code:(program ~cmd:write_sector ~track:0 ~side:0 ~sector:0) (blank ()) in
  fill t data;
  run_to_end t;
  check "sector 0: no byte taken" (moved t = 0);
  check "sector 0: record not found" (status t land record_not_found <> 0);
  check "sector 0: image unchanged" (image t = blank ());

  (* 5. A save taken mid-read resumes the read: both machines end the same
     and hold the sector. *)
  let n = logical ~track:1 ~side:0 ~sector:7 in
  let t = machine ~code:(program ~cmd:read_sector ~track:1 ~side:0 ~sector:7) (blank ()) in
  step_into_transfer t;
  let r = match Msx.restore ~state:(Msx.serialize t) with Ok r -> r | Error e -> failwith e in
  run_to_end t;
  run_to_end r;
  check "mid-read save: the restored machine reads the sector"
    (buffer_of r sector_bytes = original n && status r land error_bits = 0);
  check "mid-read save: both machines end in the same state" (Msx.serialize r = Msx.serialize t);

  (* 6. A save taken mid-write resumes the write. *)
  let n = logical ~track:4 ~side:0 ~sector:2 in
  let data = pattern 29 in
  let t = machine ~code:(program ~cmd:write_sector ~track:4 ~side:0 ~sector:2) (blank ()) in
  fill t data;
  step_into_transfer t;
  check "mid-write save: the sector is not on the disk yet" (sector_of t n = original n);
  let r = match Msx.restore ~state:(Msx.serialize t) with Ok r -> r | Error e -> failwith e in
  run_to_end r;
  check "mid-write save: the restored machine finishes the sector"
    (sector_of r n = String.sub data 0 sector_bytes && status r land error_bits = 0);
  check "mid-write save: the saved machine is not affected" (sector_of t n = original n);

  (* 7. On the disk HLE path the DSKIO trap writes the image itself. A
     sector read through the window after that must see the new bytes,
     not the track the controller decoded before. *)
  let second = buffer + sector_bytes and source = buffer + (2 * sector_bytes) in
  let dskio_write =
    [ ld_a 0; (* drive A *)
      Op [ 0x01; 0xf9; 0x01 ]; (* LD BC: one sector, media 0xF9 *)
      Op [ 0x11; 0x00; 0x00 ]; (* LD DE: logical sector 0 *)
      Op [ 0x21; lo source; hi source ];
      Op [ 0x37 ]; (* SCF: write *)
      Op [ 0xcd; 0x10; 0x40 ] ] (* CALL DSKIO *)
  in
  let code =
    assemble
      (setup @ seek_to ~track:0
       @ transfer ~id:"1" ~cmd:read_sector ~side:0 ~sector:1 ~buf:buffer ()
       @ dskio_write
       (* No side write: the drive still holds track 0 as decoded before. *)
       @ transfer ~id:"2" ~cmd:read_sector ~sector:1 ~buf:second ()
       @ finish)
  in
  let rom = Bytes.make 32768 '\000' in
  List.iteri (fun i b -> Bytes.set rom i (Char.chr b)) code;
  let t =
    Msx.create
      ~machine:{ ram_kb = 512; vram_kb = 128; roms = [ Bytes.to_string rom; ""; "" ] }
  in
  Msx.load_disk t (blank ());
  (* Page 0 on slot 0, page 1 on slot 2 (the HLE interface ROM and the
     window), page 3 on slot 3-2. *)
  Msx.port_out t 0xA8 0xC8;
  Msx.mem_write t 0xffff 0x80;
  let data = pattern 41 in
  String.iteri (fun i c -> Msx.mem_write t (source + i) (Char.code c)) (String.sub data 0 sector_bytes);
  run_to_end t;
  let read_at a = String.init sector_bytes (fun i -> Char.chr (Msx.mem_read t (a + i))) in
  check "HLE: the first read sees the old sector" (read_at buffer = original 0);
  check "HLE: DSKIO wrote the image" (sector_of t 0 = String.sub data 0 sector_bytes);
  check "HLE: the read after DSKIO sees the new bytes" (read_at second = String.sub data 0 sector_bytes);

  (* 8. Register 5 bit 2 reads 0 once after a disk goes in: at power-on
     with a disk, and after change_disk (openMSX DiskChanger). *)
  let t = machine ~code:(assemble finish) (blank ()) in
  let changed () = Msx.mem_read t drive_reg land 0x04 = 0 in
  let first = changed () in
  let second = changed () in
  check "a disk at power-on is reported once" (first && not second);
  (match Msx.change_disk t (blank ()) with Ok () -> () | Error e -> failwith e);
  let first = changed () in
  let second = changed () in
  check "change_disk is reported once" (first && not second);

  if !failures > 0 then begin
    Printf.eprintf "fdc_write_test: %d failure(s)\n" !failures;
    exit 1
  end;
  print_endline "fdc_write_test: all passed"
