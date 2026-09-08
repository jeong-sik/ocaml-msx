(* .dsk 컨테이너 증명. 합성 이미지로 두 포맷과 논리 섹터 주소를 잰다 —
   저작물은 실어 오지 않는다. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

(* 섹터마다 첫 바이트에 논리 번호를 심은 raw 4트랙 이미지. *)
let synthetic_raw () =
  let n = 4 * 18 * 512 in
  let b = Buffer.create n in
  for s = 0 to (4 * 18) - 1 do
    Buffer.add_char b (Char.chr (s land 0xff));
    Buffer.add_string b (String.make 511 '\x00')
  done;
  Buffer.contents b

let () =
  (* raw: 첫 바이트가 곧 논리 섹터 0 의 첫 바이트다. *)
  let raw = Dsk.parse (synthetic_raw ()) in
  check "raw track count" (raw.Dsk.tracks = 4);
  check "raw sector count" (Dsk.total_sectors raw = 72);
  (match Dsk.read_sector raw 0 with
   | Some s -> check "raw sector 0 starts at byte 0" (Char.code s.[0] = 0)
   | None -> check "raw sector 0 readable" false);
  (match Dsk.read_sector raw 19 with
   | Some s -> check "logical order is file order" (Char.code s.[0] = 19)
   | None -> check "sector 19 readable" false);
  check "past the end reads None" (Dsk.read_sector raw 72 = None);

  (* 쓰기: 한 섹터를 바꾸면 그 섹터만 바뀐다. *)
  check "write in range lands" (Dsk.write_sector raw 5 (String.make 512 '\xAB'));
  (match Dsk.read_sector raw 5 with
   | Some s -> check "written byte comes back" (Char.code s.[0] = 0xAB)
   | None -> check "sector 5 readable" false);
  (match Dsk.read_sector raw 6 with
   | Some s -> check "next sector untouched" (Char.code s.[0] = 6)
   | None -> check "sector 6 readable" false);

  (* headered (CPC-family) 는 지금은 명시적 거부 — 규격 정리 후 확장. *)
  (try
     let hdr = "MV - CPC" ^ String.make (0x100 - 8) '\x00' ^ "payload" in
     let d = Dsk.parse hdr in
     check "headered image is refused" (d.Dsk.tracks < 0)
   with Dsk.Bad _ -> check "headered image is refused" true);

  check "empty image is refused"
    (try ignore (Dsk.parse ""); false with Dsk.Bad _ -> true);

  (* ---------- FAT12: BPB 해석, 디렉터리, 클러스터 체인 ----------
     체인 2→3→4→EOF(BIG.BIN 1034바이트) 와 6→EOF(TEST.TXT 5바이트). *)
  let disk = Dsk.parse (String.make (720 * 1024) '\x00') in
  let put s x = check "synthetic write lands" (Dsk.write_sector disk s x) in
  let bpb = Bytes.make 512 '\x00' in
  Bytes.set bpb 11 '\x00';
  Bytes.set bpb 12 '\x02';
  Bytes.set bpb 13 '\x01';
  Bytes.set bpb 14 '\x01';
  Bytes.set bpb 15 '\x00';
  Bytes.set bpb 16 '\x02';
  Bytes.set bpb 17 '\x70';
  Bytes.set bpb 18 '\x00';
  Bytes.set bpb 22 '\x03';
  Bytes.set bpb 23 '\x00';
  put 0 (Bytes.to_string bpb);
  let fat = Bytes.make 512 '\x00' in
  Bytes.set fat 0 '\xf9';
  Bytes.set fat 1 '\xff';
  Bytes.set fat 3 '\x03';
  Bytes.set fat 4 '\x40';
  Bytes.set fat 6 '\xff';
  Bytes.set fat 7 '\xff';
  Bytes.set fat 9 '\xff';
  Bytes.set fat 10 '\xff';
  put 1 (Bytes.to_string fat);
  put 4 (Bytes.to_string fat);
  let dir = Bytes.make 512 '\x00' in
  Bytes.blit_string "TEST    TXT" 0 dir 0 11;
  Bytes.set dir 26 '\x06';
  Bytes.set dir 28 '\x05';
  Bytes.blit_string "BIG     BIN" 0 dir 32 11;
  Bytes.set dir (32 + 26) '\x02';
  Bytes.set dir (32 + 29) '\x04';
  Bytes.set dir (32 + 28) '\x0a';
  put 7 (Bytes.to_string dir);
  put 14 (String.init 512 (fun i -> Char.chr (i land 0xff)));
  put 15 (String.make 512 '\xaa');
  put 16 ("0123456789" ^ String.make 502 '\x00');
  put 18 ("hello" ^ String.make 507 '\x00');

  let fat1 = 1 in
  check "fat12 even entry" (Dsk.fat12_next disk fat1 2 = 3);
  check "fat12 odd entry" (Dsk.fat12_next disk fat1 3 = 4);
  check "fat12 end marker" (Dsk.fat12_next disk fat1 4 = 0xFFF);
  let entries = Dsk.list_dir disk in
  check "dir has two files" (List.length entries = 2);
  (match Dsk.find_file disk "TEST    TXT" with
   | Some e ->
     check "dir entry cluster" (e.Dsk.cluster = 6);
     check "dir entry size" (e.Dsk.size = 5);
     (match Dsk.read_entry disk e with
      | Some c -> check "short file reads whole" (c = "hello")
      | None -> check "short file readable" false)
   | None -> check "TEST.TXT found" false);
  (match Dsk.find_file disk "BIG     BIN" with
   | Some e ->
     check "big size" (e.Dsk.size = 1034);
     (match Dsk.read_entry disk e with
      | Some c ->
        check "big length follows dir size" (String.length c = 1034);
        check "cluster 2 first byte" (Char.code c.[0] = 0);
        check "chain crosses to cluster 3" (Char.code c.[512] = 0xAA);
        check "chain tail truncated to size" (String.sub c 1024 10 = "0123456789")
      | None -> check "BIG.BIN readable" false)
   | None -> check "BIG.BIN found" false);
  check "missing file is None" (Dsk.find_file disk "NOPE    X  " = None);
  (* BPB 가 512바이트 섹터가 아니면 파일 계층은 빈손 — DSKIO 는 그대로. *)
  let nobpb = Dsk.parse (synthetic_raw ()) in
  check "no bpb means no dir" (Dsk.list_dir nobpb = []);

  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end;
  print_endline "dsk: all checks passed"
