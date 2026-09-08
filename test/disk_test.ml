(* FAT12 + BDOS 디스크 부트 경로 증명. 부트섹터(BPB + 부트코드 + FCB), FAT,
   루트 엔트리, MSXDOS.SYS 페이로드를 전부 이 파일 안에서 직접 만든다 — 외부
   이미지 무의존. C-BIOS ROM(roms/, gitignore)이 없으면 SKIP.

   증명하는 사슬: INIT 트랩 → 부트섹터 0xC000 로드/실행 → BDOS Open(0x0F) 이
   FAT12 루트에서 MSXDOS.SYS 를 찾음 → Set DMA(0x1A) → Random block read(0x27)
   가 0x0100 으로 복사 → JP 0x0100 → 페이로드가 VDP 포트로 VRAM 0x1800 에
   "FAT12 OK" 를 쓴다. 실기 MSX-DOS 부트와 같은 호출 순서다. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let read_file p =
  let ic = open_in_bin p in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let sbytes a = String.init (Array.length a) (fun i -> Char.chr a.(i))

let u16 b off v =
  Bytes.set b off (Char.chr (v land 0xff));
  Bytes.set b (off + 1) (Char.chr ((v lsr 8) land 0xff))

(* 부트코드 (+0x1E). RET NC 로 INIT 의 carry 를 검사한 뒤, 실기 부트 절차와
   같은 BDOS 호출 열을 수행하고 커널(0x0100)로 점프한다. *)
let boot_code =
  sbytes
    [| 0xC0; (* RET NC *)
       0x31; 0x1F; 0xF5; (* LD SP,0xF51F *)
       0x11; 0xB0; 0xC0; (* LD DE,0xC0B0 (FCB) *)
       0x0E; 0x0F; (* LD C,open *)
       0xCD; 0x7D; 0xF3; (* CALL 0xF37D *)
       0x3C; (* INC A *)
       0xCA; 0x00; 0x00; (* JP Z,0x0000 — open 실패면 재부트로 *)
       0x11; 0x00; 0x01; (* LD DE,0x0100 (DMA) *)
       0x0E; 0x1A; (* LD C,set dma *)
       0xCD; 0x7D; 0xF3;
       0x21; 0x01; 0x00; (* LD HL,1 (레코드 수) *)
       0x11; 0xB0; 0xC0; (* LD DE,FCB *)
       0x0E; 0x27; (* LD C,read block *)
       0xCD; 0x7D; 0xF3;
       0xC3; 0x00; 0x01 (* JP 0x0100 *) |]

(* 페이로드 = 가짜 MSXDOS.SYS. page0 이 RAM 이 된 뒤라 BIOS 직접 call 은
   무효 — VDP 포트(0x99 주소, 0x98 데이터)로 name table 0x1800 에 직접 쓴다. *)
let payload =
  sbytes
    [| 0x3E; 0x00; (* LD A,0x00 — 주소 하위 *)
       0xD3; 0x99;
       0x3E; 0x58; (* LD A,0x58 — 0x1800>>8 | write *)
       0xD3; 0x99;
       0x21; 0x15; 0x01; (* LD HL,0x0115 (msg) *)
       0x06; 0x08; (* LD B,8 *)
       0x7E; (* LD A,(HL) *)
       0xD3; 0x98; (* OUT (0x98),A *)
       0x23; (* INC HL *)
       0x10; 0xFA; (* DJNZ *)
       0x18; 0xFE (* JR $ *) |]
  ^ "FAT12 OK"

(* 720KB 2DD 지오메트리 FAT12 디스크 한 장. *)
let dsk () =
  let d = Bytes.make (1440 * 512) '\000' in
  Bytes.set d 0 '\xEB'; (* jmp $ — 부트 가능 마커 *)
  Bytes.set d 1 '\xFE';
  Bytes.set d 2 '\x90';
  Bytes.blit_string "MSXHARNS" 0 d 3 8;
  u16 d 0x0B 512; (* bytes/sector *)
  Bytes.set d 0x0D '\x02'; (* sectors/cluster *)
  u16 d 0x0E 1; (* reserved *)
  Bytes.set d 0x10 '\x02'; (* FAT 개수 *)
  u16 d 0x11 112; (* 루트 엔트리 *)
  u16 d 0x13 1440; (* 총 섹터 *)
  Bytes.set d 0x15 '\xF9'; (* media *)
  u16 d 0x16 3; (* sectors/FAT *)
  u16 d 0x18 9; (* sectors/track *)
  u16 d 0x1A 2; (* heads *)
  Bytes.blit_string boot_code 0 d 0x1E (String.length boot_code);
  (* FCB @ 0xC0B0: 드라이브 0 + "MSXDOS  SYS". 레코드 크기(0x0E)=0 → 128 기본. *)
  Bytes.blit_string "\x00MSXDOS  SYS" 0 d 0xB0 12;
  (* FAT 1/2: media 예약 + 클러스터 2 = EOI. *)
  for f = 0 to 1 do
    let base = (1 + (f * 3)) * 512 in
    Bytes.set d base '\xF9';
    Bytes.set d (base + 1) '\xFF';
    Bytes.set d (base + 2) '\xFF';
    Bytes.set d (base + 3) '\xFF';
    Bytes.set d (base + 4) '\x0F'
  done;
  (* 루트 엔트리 0 @ 섹터 7: MSXDOS.SYS, 클러스터 2, 128 바이트. *)
  let r = 7 * 512 in
  Bytes.blit_string "MSXDOS  SYS" 0 d r 11;
  Bytes.set d (r + 11) '\x20';
  u16 d (r + 26) 2;
  u16 d (r + 28) 128;
  (* 데이터 클러스터 2 @ 섹터 14. *)
  Bytes.blit_string payload 0 d (14 * 512) (String.length payload);
  Bytes.to_string d

let rom_names =
  [ "roms/cbios/cbios_main_msx2.rom"; "roms/cbios/cbios_logo_msx2.rom"; "roms/cbios/cbios_sub.rom" ]

let () =
  if List.exists (fun f -> not (Sys.file_exists f)) rom_names then
    Printf.eprintf "SKIP disk_test: C-BIOS ROM 없음 (roms/ 는 gitignore)\n%!"
  else begin
    let roms = List.map read_file rom_names in
    let t = Msx.create ~machine:{ ram_kb = 512; vram_kb = 128; roms } in
    Msx.load_disk t (dsk ());
    for _ = 1 to 500 do Msx.step t ~frames:1 done;
    let got = String.init 8 (fun i -> Char.chr (Msx.vram_read t (0x1800 + i))) in
    check "disk boot renders payload at 0x1800" (got = "FAT12 OK");
    if !failures > 0 then exit 1
  end
