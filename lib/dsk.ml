(* A .dsk floppy image: the byte-level thing DSKIO reads from and writes to.

   The shape parsed here is the raw one: the sectors alone, 512 bytes each
   in logical order, so the file is exactly tracks*2*9*512 bytes and its
   first byte is the boot sector's first byte. The CPC-family headered
   shapes are refused with [Bad] until their own geometry handling lands.

   Sectors are addressed the way DSKIO does: logical sector numbers from 0,
   nine a head, two heads a track. The image stores them in that order, so
   the file offset is the number times 512 -- the physical interleave the
   drive firmware once walked is not in the file. *)

type t = {
  bytes : Bytes.t;
  mutable tracks : int;  (** cylinders, 2 heads each *)
  sector_bytes : int;
}

exception Bad of string

let bytes_per_sector = 512
let sectors_per_track = 9
let heads = 2

let sectors_per_cylinder = sectors_per_track * heads

(* Sector sizes a track header can name: 128 << the exponent nibble. *)
let named_sector_size code =
  if code > 0 && code <= 6 then Some (128 lsl (code - 1)) else None

let create ~tracks =
  if tracks <= 0 then raise (Bad "a disk needs a track");
  {
    bytes = Bytes.make (tracks * sectors_per_cylinder * bytes_per_sector) '\000';
    tracks;
    sector_bytes = bytes_per_sector;
  }

let total_sectors t = t.tracks * sectors_per_cylinder

(* Raw iff the file is exactly a whole number of cylinders. The extended
   formats open with a track header rather than a jump instruction, so a file
   whose first two bytes are a Z80 jump is not one of them. *)
let looks_raw len = len mod (sectors_per_cylinder * bytes_per_sector) = 0

let parse (data : string) : t =
  let len = String.length data in
  if len = 0 then raise (Bad "empty image");
  if looks_raw len then
    { bytes = Bytes.of_string data
    ; tracks = len / (sectors_per_cylinder * bytes_per_sector)
    ; sector_bytes = bytes_per_sector
    }
  else
    (* A headered image (CPC-family: the file opens with "MV - CPC" or
       "EXTENDED" and a 0x100 file header). Not parsed yet -- the header
       names its own geometry and the extended flavour interleaves per-sector
       ID blocks, so refusing beats guessing. MSX disk images in the wild are
       overwhelmingly raw. *)
    raise
      (Bad
         "headered (CPC-family) image: only raw sector-order .dsk is parsed")

let read_sector t logical =
  if logical < 0 || logical >= total_sectors t then None
  else begin
    let off = logical * t.sector_bytes in
    Some (Bytes.sub_string t.bytes off t.sector_bytes)
  end

let write_sector t logical (data : string) =
  if logical < 0 || logical >= total_sectors t then false
  else begin
    let off = logical * t.sector_bytes in
    let n = min (String.length data) t.sector_bytes in
    Bytes.blit_string data 0 t.bytes off n;
    true
  end

(* ---------- FAT12 — MSX-DOS 디스크의 파일 계층 ----------

   부트 섹터(logical 0)의 BPB 가 기하를 정한다(2D 표준: FAT 2본×3섹터,
   디렉터리 112항목, 1섹터 클러스터). DSKIO 트랩은 섹터 단위로 서비스하고
   이 계층은 BDOS 스텁(0xF37D)이 파일 단위로 서비스할 때 쓴다. 이름은
   8.3 스페이스 패딩 11바이트 — FCB 와 디렉터리가 같은 표현이다. *)

type bpb = {
  sectors_per_cluster : int;
  reserved_sectors : int;
  fat_copies : int;
  sectors_per_fat : int;
  dir_entries : int;
}

let bpb t =
  match read_sector t 0 with
  | None -> None
  | Some s ->
      if String.length s < 26 then None
      else begin
        let u8 o = Char.code s.[o] in
        let u16 o = u8 o lor (u8 (o + 1) lsl 8) in
        if u16 11 <> bytes_per_sector then None
        else
          Some
            { sectors_per_cluster = max 1 (u8 13);
              reserved_sectors = u16 14;
              fat_copies = max 1 (u8 16);
              dir_entries = u16 17;
              sectors_per_fat = u16 22 }
      end

type dir_entry = { name : string; attr : int; cluster : int; size : int }

let dir_start b = b.reserved_sectors + b.fat_copies * b.sectors_per_fat

let dir_sectors b = (b.dir_entries * 32 + bytes_per_sector - 1) / bytes_per_sector

let data_start b = dir_start b + dir_sectors b

let list_dir t =
  match bpb t with
  | None -> []
  | Some b ->
      let out = ref [] in
      for e = 0 to b.dir_entries - 1 do
        let off = e * 32 in
        match read_sector t (dir_start b + (off / bytes_per_sector)) with
        | None -> ()
        | Some s ->
            let base = off mod bytes_per_sector in
            let u8 o = Char.code s.[base + o] in
            (* 0x00 = 디렉터리 끝, 0xE5 = 삭제됨, attr bit3 = 볼륨 라벨. *)
            if u8 0 <> 0x00 && u8 0 <> 0xE5 && u8 11 land 0x08 = 0 then
              out :=
                { name = String.sub s base 11;
                  attr = u8 11;
                  cluster = u8 26 lor (u8 27 lsl 8);
                  size =
                    u8 28 lor (u8 29 lsl 8) lor (u8 30 lsl 16) lor (u8 31 lsl 24) }
                :: !out
      done;
      List.rev !out

(* FAT12 엔트리: 클러스터 번호의 3/2 오프셋에 12비트가 겹쳐 들어 있다. *)
let fat12_next t fat_start c =
  let off = (fat_start * bytes_per_sector) + ((c * 3) / 2) in
  let lo = Char.code (Bytes.get t.bytes off) in
  let hi = Char.code (Bytes.get t.bytes (off + 1)) in
  if c land 1 = 0 then lo lor ((hi land 0x0f) lsl 8)
  else (hi lsl 4) lor (lo lsr 4)

(* 디렉터리 항목의 클러스터 체인을 걸어 파일 내용을 읽는다. 체인이 사이즈보다
   짧으면 있는 만큼 — 손상 디스크를 세워 죽이는 대신 견딘다. *)
let read_entry t entry =
  match bpb t with
  | None -> None
  | Some b ->
      if entry.cluster < 2 then Some ""
      else begin
        let fat_start = b.reserved_sectors in
        let buf = Buffer.create (max bytes_per_sector entry.size) in
        let c = ref entry.cluster in
        let guard = ref (total_sectors t) in
        while
          !c >= 2 && !c < 0xFF8 && Buffer.length buf < entry.size && !guard > 0
        do
          let first = data_start b + ((!c - 2) * b.sectors_per_cluster) in
          for s = 0 to b.sectors_per_cluster - 1 do
            match read_sector t (first + s) with
            | Some sec -> Buffer.add_string buf sec
            | None -> ()
          done;
          c := fat12_next t fat_start !c;
          decr guard
        done;
        let all = Buffer.contents buf in
        if String.length all > entry.size then
          Some (String.sub all 0 entry.size)
        else Some all
      end

let find_file t name =
  List.find_opt (fun e -> e.name = name) (list_dir t)
