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

  if !failures > 0 then begin
    Printf.eprintf "%d failure(s)\n%!" !failures;
    exit 1
  end;
  print_endline "dsk: all checks passed"
