let check name condition = if not condition then failwith name
let byte b i = Char.code (Bytes.get b i)
let word b i = byte b i lor (byte b (i + 1) lsl 8)
let set_word b i n =
  Bytes.set b i (Char.chr (n land 255));
  Bytes.set b (i + 1) (Char.chr ((n lsr 8) land 255))
let root = 3 * 512
let cluster_offset cluster = (4 + cluster - 2) * 512
let fat b cluster =
  let n = word b (512 + cluster * 3 / 2) in
  if cluster land 1 = 0 then n land 0xfff else n lsr 4
let set_fat b cluster value =
  List.iter (fun sector ->
    let off = sector * 512 + cluster * 3 / 2 in
    let old = word b off in
    set_word b off (if cluster land 1 = 0 then old land 0xf000 lor value
                   else old land 15 lor (value lsl 4))) [1;2]
let image () =
  let b = Bytes.make (20 * 512) '\000' in
  set_word b 11 512; Bytes.set b 13 '\001'; set_word b 14 1;
  Bytes.set b 16 '\002'; set_word b 17 16; set_word b 19 20;
  Bytes.set b 21 '\xf9'; set_word b 22 1;
  set_fat b 0 0xff9; set_fat b 1 0xfff;
  b
let put image name data =
  match Disk_fat12.put_file ~image ~name ~data with
  | Ok bytes -> bytes
  | Error _ -> failwith "FAT12 write unexpectedly rejected"
let mirrors b = check "FAT copies agree" (Bytes.sub b 512 512 = Bytes.sub b 1024 512)
let chain b first =
  let rec walk remaining cluster =
    if cluster >= 0xff8 || cluster = 0 then []
    else (check "chain terminates" (remaining > 0); cluster :: walk (remaining - 1) (fat b cluster))
  in walk 16 first
let file_bytes b =
  let size = word b (root + 28) lor (word b (root + 30) lsl 16) in
  let all = chain b (word b (root + 26))
    |> List.map (fun cluster -> Bytes.sub_string b (cluster_offset cluster) 512)
    |> String.concat "" in
  String.sub all 0 size
let rejected label expected original name data =
  let before = Bytes.copy original in
  check label (Disk_fat12.put_file ~image:original ~name ~data = Error expected);
  check (label ^ " keeps input bytes") (original = before)

let test_fat () =
  let original = image () in
  set_fat original 3 0xfff; set_fat original 4 0xfff;
  let data = Bytes.init 1300 (fun i -> Char.chr (i land 255)) in
  let before = Bytes.copy original in
  let written = put original "CAMPAIGNDAT" data in
  check "successful write leaves source image intact" (original = before);
  check "fragmented chain crosses even and odd FAT entries"
    (chain written (word written (root + 26)) = [2;5;6]);
  check "adjacent occupied FAT nibbles are preserved" (fat written 3 = 0xfff && fat written 4 = 0xfff);
  check "fragmented payload roundtrips" (file_bytes written = Bytes.to_string data);
  check "last cluster slack is zeroed"
    (Bytes.sub_string written (cluster_offset 6 + 276) 236 = String.make 236 '\000');
  mirrors written;
  let duplicate = Bytes.copy written in
  Bytes.blit written root duplicate (root + 32) 32;
  check "duplicate target names cannot bypass validated lookup"
    (Disk_fat12.validate_writable_file ~image:duplicate ~name:"CAMPAIGNDAT" = Error Disk_fat12.Invalid_image);
  let shorter = put written "CAMPAIGNDAT" (Bytes.of_string "saved") in
  check "truncation frees old odd and even clusters"
    (chain shorter (word shorter (root + 26)) = [2] && fat shorter 5 = 0 && fat shorter 6 = 0);
  check "truncated payload is exact" (file_bytes shorter = "saved");
  mirrors shorter;
  let empty = put shorter "CAMPAIGNDAT" Bytes.empty in
  check "empty file has no allocation" (word empty (root + 26) = 0 && fat empty 2 = 0);
  let stale_after_end = image () in
  Bytes.blit_string "STALE   DAT" 0 stale_after_end (root + 32) 11;
  let created = put stale_after_end "CAMPAIGNDAT" Bytes.empty in
  check "creating at end marker does not expose stale following entry"
    (Bytes.sub_string created root 11 = "CAMPAIGNDAT" && byte created (root + 32) = 0);
  let deleted_slot = image () in
  Bytes.set deleted_slot root '\xe5';
  Bytes.blit_string "LIVE    DAT" 0 deleted_slot (root + 32) 11;
  Bytes.set deleted_slot (root + 32 + 11) '\x20';
  let live_before = Bytes.sub deleted_slot (root + 32) 32 in
  let reused = put deleted_slot "CAMPAIGNDAT" Bytes.empty in
  check "reusing deleted slot retains later live directory entry"
    (Bytes.sub_string reused root 11 = "CAMPAIGNDAT"
     && Bytes.sub reused (root + 32) 32 = live_before);
  let read_only = Bytes.copy written in
  Bytes.set read_only (root + 11) '\001';
  rejected "read-only file rejected" Disk_fat12.Read_only read_only "CAMPAIGNDAT" Bytes.empty;
  let full = image () in
  for cluster = 2 to 17 do set_fat full cluster 0xfff done;
  rejected "full disk rejected" Disk_fat12.Disk_full full "CAMPAIGNDAT" (Bytes.of_string "x");
  let directory = image () in
  for i = 0 to 15 do
    Bytes.blit_string (Printf.sprintf "FILE%04dDAT" i) 0 directory (root + i * 32) 11
  done;
  rejected "full directory rejected" Disk_fat12.Directory_full directory "CAMPAIGNDAT" Bytes.empty;
  let invalid = image () in set_word invalid 11 0;
  rejected "invalid geometry rejected" Disk_fat12.Invalid_image invalid "CAMPAIGNDAT" Bytes.empty;
  let cyclic = Bytes.copy written in set_fat cyclic 6 2;
  rejected "cyclic existing chain rejected" Disk_fat12.Invalid_image cyclic "CAMPAIGNDAT" Bytes.empty;
  let mismatched = Bytes.copy written in
  Bytes.set mismatched (1024 + 3) (Char.chr (byte mismatched (1024 + 3) lxor 1));
  rejected "inconsistent FAT mirrors rejected" Disk_fat12.Invalid_image mismatched "CAMPAIGNDAT" Bytes.empty;
  let crosslinked = Bytes.copy written in
  Bytes.blit_string "OTHER   DAT" 0 crosslinked (root + 32) 11;
  set_word crosslinked (root + 32 + 26) 2;
  set_word crosslinked (root + 32 + 28) 1;
  rejected "crosslinked root files rejected" Disk_fat12.Invalid_image crosslinked "CAMPAIGNDAT" Bytes.empty;
  rejected "invalid filename rejected" Disk_fat12.Invalid_name (image ()) "BAD/NAMEEXT" Bytes.empty

let write_code bytes offset code =
  List.iteri (fun i n -> Bytes.set bytes (offset + i) (Char.chr n)) code
let machine () =
  let disk = image () in
  write_code disk 0x1e [0xc3;0x80;0xc0];
  write_code disk 0x80 [0x18;0xfe];
  let t = Msx.create ~machine:{ram_kb=512;vram_kb=128;roms=[]} in
  Msx.port_out t 0xa8 0xc0;
  Msx.load_disk ~interface_rom:false t (Bytes.to_string disk);
  (match Msx.boot_disk t with Ok () -> () | Error error -> failwith error);
  Msx.step t ~frames:1;
  String.iteri (fun i c -> Msx.mem_write t (0xc200 + i) (Char.code c)) "\000CAMPAIGNDAT";
  t, disk
let bdos t function_code de hl =
  (* Patch the parked JR into a call stub. The guest parks itself again only
     after storing the returned A/HL, so every assertion observes execution. *)
  let code = [0x11;de land 255;de lsr 8; 0x21;hl land 255;hl lsr 8;
    0x0e;function_code;0xcd;0x7d;0xf3;
    0x32;0x00;0xc3;0x22;0x02;0xc3;
    0x21;0x18;0xfe;0x22;0x80;0xc0;0xc3;0x80;0xc0] in
  List.iteri (fun i n -> Msx.mem_write t (0xc040 + i) n) code;
  List.iteri (fun i n -> Msx.mem_write t (0xc080 + i) n) [0xc3;0x40;0xc0];
  Msx.mem_write t 0xc300 0xff;
  Msx.step t ~frames:1;
  check "BDOS guest parks after return" (Msx.dump_pc t = 0xc080);
  Msx.mem_read t 0xc300, Msx.mem_read t 0xc302 lor (Msx.mem_read t 0xc303 lsl 8)
let success t function_code de hl =
  let a, count = bdos t function_code de hl in
  check (Printf.sprintf "BDOS %02x succeeds" function_code) (a = 0); count
let record ?(fcb=0xc200) t n =
  for i = 0 to 3 do Msx.mem_write t (fcb + 0x21 + i) ((n lsr (8 * i)) land 255) done
let exported t =
  match Msx.disk_image t with Some bytes -> Bytes.of_string bytes | None -> failwith "disk disappeared"
let copy t =
  match Msx.restore ~state:(Msx.serialize t) with Ok t -> t | Error error -> failwith error
let dma t text = String.iteri (fun i c -> Msx.mem_write t (0xc400 + i) (Char.code c)) text

let test_bdos () =
  let t, source = machine () in
  check "CREATE returns success in L as well as A" (success t 0x16 0xc200 0x1234 land 255 = 0);
  check "CREATE installs an empty root file" (file_bytes (exported t) = "");
  Msx.mem_write t 0xc20e 1; Msx.mem_write t 0xc20f 0;
  ignore (success t 0x1a 0xc400 0);
  let first = String.init 700 (fun i -> Char.chr ((i * 7) land 255)) in
  dma t first; ignore (success t 0x26 0xc200 700);
  check "first write publishes exact file bytes" (file_bytes (exported t) = first);
  let restored = copy t in
  let second = String.init 500 (fun i -> Char.chr ((i * 11 + 3) land 255)) in
  List.iter (fun m ->
    dma m second; ignore (success m 0x26 0xc200 500);
    ignore (success m 0x10 0xc200 0);
    ignore (success m 0x0f 0xc200 0);
    record m 0;
    ignore (success m 0x1a 0xc800 0);
    check "reopened block read returns full record count" (success m 0x27 0xc200 1200 = 1200);
    let read = String.init 1200 (fun i -> Char.chr (Msx.mem_read m (0xc800 + i))) in
    check "CREATE/write/close/open/read guest roundtrip" (read = first ^ second);
    let disk = exported m in
    mirrors disk;
    check "exported file spans both odd and even clusters" (chain disk (word disk (root + 26)) = [2;3;4]);
    check "exported FAT file contains both writes" (file_bytes disk = first ^ second)
  ) [t;restored];
  check "checkpoint continues open writer identically" (Msx.serialize t = Msx.serialize restored);
  check "source disk still has no directory entry" (byte source root = 0);
  record t 257;
  ignore (success t 0x26 0xc200 0);
  check "zero-count block write truncates at random record" (file_bytes (exported t) = String.sub first 0 257);
  check "truncate releases trailing allocation" (fat (exported t) 3 = 0 && fat (exported t) 4 = 0);
  (* Explicit random record zero must seek to byte zero even after a read. *)
  ignore (success t 0x1a 0xc800 0);
  record t 0; ignore (success t 0x27 0xc200 10);
  record t 0; ignore (success t 0x27 0xc200 10);
  check "random record zero rereads the beginning"
    (String.init 10 (fun i -> Char.chr (Msx.mem_read t (0xc800 + i))) = String.sub first 0 10);
  (* Records >=64 bytes ignore the fourth random-record byte. The final
     partial record is padded and counts as one returned record. *)
  Msx.mem_write t 0xc20e 128; Msx.mem_write t 0xc20f 0;
  record t 2; Msx.mem_write t 0xc224 0x7f;
  check "partial final record is returned" (success t 0x27 0xc200 1 = 1);
  check "partial data and zero padding reach DMA"
    (Msx.mem_read t 0xc800 = Char.code first.[256]
     && String.init 127 (fun i -> Char.chr (Msx.mem_read t (0xc801 + i))) = String.make 127 '\000');
  record t 2;
  for i = 0 to 255 do Msx.mem_write t (0xc800 + i) 0xa5 done;
  let error, returned = bdos t 0x27 0xc200 2 in
  check "partial short read reports EOF and actual count" (error = 1 && returned = 1);
  check "short read advances by the returned record" (Msx.mem_read t 0xc221 = 3);
  check "short read leaves the next record untouched"
    (String.init 128 (fun i -> Char.chr (Msx.mem_read t (0xc880 + i))) = String.make 128 '\165');
  let error, returned = bdos t 0x27 0xc200 1 in
  check "subsequent EOF returns no records without moving the cursor"
    (error = 1 && returned = 0 && Msx.mem_read t 0xc221 = 3);
  Msx.mem_write t 0xc224 0x7f;
  check "ignored high byte is preserved" (Msx.mem_read t 0xc224 = 0x7f);
  let before_extent_create = Msx.disk_image t in
  Msx.mem_write t 0xc20c 1;
  ignore (success t 0x16 0xc200 0);
  check "nonzero extent CREATE preserves existing file"
    (Msx.disk_image t = before_extent_create);
  (* Two open FCBs must not overwrite each other's later changes using stale
     cached bytes. Both open before either write occurs. *)
  String.iteri (fun i c -> Msx.mem_write t (0xc280 + i) (Char.code c)) "\000CAMPAIGNDAT";
  ignore (success t 0x0f 0xc280 0);
  List.iter (fun fcb -> Msx.mem_write t (fcb + 14) 1; Msx.mem_write t (fcb + 15) 0) [0xc200;0xc280];
  ignore (success t 0x1a 0xc400 0);
  record t 0; dma t "X"; ignore (success t 0x26 0xc200 1);
  record ~fcb:0xc280 t 1; dma t "Y"; ignore (success t 0x26 0xc280 1);
  check "two FCB writers preserve both edits and remaining bytes"
    (file_bytes (exported t) = "XY" ^ String.sub first 2 255);
  (* With one-byte records, the fourth record byte is significant. A record
     at 2^24 cannot fit this floppy and must fail before mutation. *)
  let before = Msx.disk_image t in
  record t (1 lsl 24);
  let result, _ = bdos t 0x26 0xc200 1 in
  check "small-record high byte is not ignored" (result <> 0 && Msx.disk_image t = before);
  let invalid = String.make (Bytes.length source) '\000' in
  (match Msx.change_disk t invalid with Ok () -> () | Error error -> failwith error);
  Msx.mem_write t 0xc20c 1;
  let result, _ = bdos t 0x16 0xc200 0 in
  check "nonzero extent CREATE rejects invalid BPB without mutating media"
    (result = 0xff && Msx.disk_image t = Some invalid)

let test_disk_slots_still_dispatch () =
  List.iter (fun (label, call) ->
    let t, _ = machine () in
    (* boot_disk selected slot2 in page1. RST30 temporarily switches it to
       slot1, then restores the caller's mapping on return. *)
    let code = [0x01;0x34;0x12] @ call
      @ [0xed;0x43;0x20;0xc3;0xc3;0x00;0xc1] in
    List.iteri (fun i byte -> Msx.mem_write t (0xc040 + i) byte) code;
    List.iteri (fun i byte -> Msx.mem_write t (0xc080 + i) byte) [0xc3;0x40;0xc0];
    List.iteri (fun i byte -> Msx.mem_write t (0xc100 + i) byte) [0x18;0xfe];
    Msx.step t ~frames:1;
    check label (Msx.mem_read t 0xc320 = 0 && Msx.mem_read t 0xc321 = 0x12
                 && Msx.dump_pc t = 0xc100 && Msx.port_in t 0xa8 = 0xfb)
  ) ["slot2 direct disk entry", [0xcd;0x16;0x40];
     "slot1 inter-slot disk entry restores caller mapping", [0xf7;0x01;0x16;0x40]]

let test_ram_disk_entry_collisions () =
  let t, _ = machine () in
  (* The same numeric addresses are ordinary instructions when page1 maps
     RAM. Executing them must not call HLE disk BIOS or pop a return address. *)
  Msx.port_out t 0xfd 2; (* page1 must not alias page3 test control code *)
  Msx.port_out t 0xa8 0xff;
  List.iter (fun address ->
    List.iteri (fun i byte -> Msx.mem_write t (address + i) byte)
      [0x3e;0x5a;0x32;0x10;0xc3;0xc3;0x80;0xc1];
    List.iteri (fun i byte -> Msx.mem_write t (0xc180 + i) byte)
      [0x21;0x18;0xfe;0x22;0x00;0xc1;0xc3;0x00;0xc1];
    let pc = Msx.dump_pc t in
    List.iteri (fun i byte -> Msx.mem_write t (pc + i) byte)
      [0xc3;address land 255;address lsr 8];
    Msx.mem_write t 0xc310 0;
    Msx.step t ~frames:1;
    check (Printf.sprintf "RAM %04x is guest code, not disk BIOS" address)
      (Msx.mem_read t 0xc310 = 0x5a && Msx.dump_pc t = 0xc100)
  ) [0x4010;0x4013;0x4016;0x4019;0x401c;0x401f;0x4030;0x4100]

let () =
  test_fat ();
  test_bdos ();
  test_disk_slots_still_dispatch ();
  test_ram_disk_entry_collisions ();
  print_endline "FAT12: fragmented atomic writes and CPU BDOS checkpoint continuation passed"
