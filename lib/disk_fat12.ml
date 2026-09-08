type error = Invalid_image | Invalid_name | Directory_full | Disk_full | Read_only
exception Reject of error
type operation = Validate | Replace of bytes
let operate ~image ~name operation =
  let reject error = raise (Reject error) in
  try
    let length = Bytes.length image in
    if length < 512 then reject Invalid_image;
    let byte off = Char.code (Bytes.get image off) in
    let word off = byte off lor (byte (off + 1) lsl 8) in
    let sector = word 11 and per_cluster = byte 13 and reserved = word 14 in
    let fats = byte 16 and entries = word 17 and sectors = word 19 and per_fat = word 22 in
    if sector <> 512 || per_cluster = 0 || per_cluster land (per_cluster - 1) <> 0
       || reserved = 0 || fats = 0 || entries = 0 || per_fat = 0
       || sectors * sector > length then reject Invalid_image;
    let root = (reserved + fats * per_fat) * sector in
    let data_start = root + ((entries * 32 + sector - 1) / sector) * sector in
    let cluster_size = per_cluster * sector in
    let count = (sectors * sector - data_start) / cluster_size in
    if data_start > sectors * sector || count < 1 || count >= 4085
       || ((count + 1) * 3 / 2) + 1 >= per_fat * sector then reject Invalid_image;
    if String.length name <> 11 || name.[0] = ' '
       || String.exists (fun c -> Char.code c < 32 || String.contains "*?./\\:;,+[]=|<>\"" c) name
    then reject Invalid_name;
    for copy = 1 to fats - 1 do
      if Bytes.sub image (reserved * sector) (per_fat * sector)
         <> Bytes.sub image ((reserved + copy * per_fat) * sector) (per_fat * sector)
      then reject Invalid_image
    done;
    let next cluster =
      let off = reserved * sector + cluster * 3 / 2 in
      let n = word off in
      if cluster land 1 = 0 then n land 0xfff else n lsr 4 in
    let free_entry = ref None and existing = ref None in
    let ended = ref false in
    for i = 0 to entries - 1 do
      let off = root + i * 32 in
      if byte off = 0 then ended := true;
      if !ended || byte off = 0xe5 then (
        if !free_entry = None then free_entry := Some off)
      else if Bytes.sub_string image off 11 = name then (
        if Option.is_some !existing then reject Invalid_image;
        existing := Some off)
    done;
    let entry = match !existing, !free_entry with
      | Some off, _ -> if byte (off + 11) land 0x19 <> 0 then reject Read_only; off
      | None, Some off -> off
      | None, None -> reject Directory_full in
    let owned = Array.make (count + 2) false in
    let allocated = Array.make (count + 2) false in
    let rec chain owner cluster =
      if cluster >= 0xff8 then ()
      else if cluster < 2 || cluster >= count + 2 || allocated.(cluster) then reject Invalid_image
      else (
        allocated.(cluster) <- true;
        if owner = entry then owned.(cluster) <- true;
        chain owner (next cluster)) in
    let rec validate_entries i =
      if i < entries then (
        let off = root + i * 32 in
        if byte off <> 0 then (
          if byte off <> 0xe5 && byte (off + 11) land 0x08 = 0 then (
            let first = word (off + 26) in
            if first <> 0 then chain off first);
          validate_entries (i + 1))) in
    validate_entries 0;
    Option.iter (fun off ->
      let size = word (off + 28) lor (word (off + 30) lsl 16) in
      let capacity = Array.fold_left (fun n yes -> if yes then n + cluster_size else n) 0 owned in
      if size > capacity then reject Invalid_image) !existing;
    match operation with
    | Validate -> Ok image
    | Replace data ->
    let required = (Bytes.length data + cluster_size - 1) / cluster_size in
    let chosen = ref [] and found = ref 0 in
    for cluster = 2 to count + 1 do
      if !found < required && (owned.(cluster) || next cluster = 0) then (
        chosen := cluster :: !chosen; incr found)
    done;
    if !found <> required then reject Disk_full;
    let chosen = List.rev !chosen in
    let result = Bytes.copy image in
    let set_byte off n = Bytes.set result off (Char.chr (n land 255)) in
    let set_word off n = set_byte off n; set_byte (off + 1) (n lsr 8) in
    let set_cluster cluster value =
      for copy = 0 to fats - 1 do
        let off = (reserved + copy * per_fat) * sector + cluster * 3 / 2 in
        let old = Char.code (Bytes.get result off) lor (Char.code (Bytes.get result (off + 1)) lsl 8) in
        set_word off (if cluster land 1 = 0 then (old land 0xf000) lor value
                     else (old land 0x000f) lor (value lsl 4))
      done in
    Array.iteri (fun cluster used -> if used then set_cluster cluster 0) owned;
    let rec write index = function
      | [] -> ()
      | cluster :: rest ->
        set_cluster cluster (match rest with [] -> 0xfff | next :: _ -> next);
        let off = data_start + (cluster - 2) * cluster_size in
        Bytes.fill result off cluster_size '\000';
        let amount = min cluster_size (Bytes.length data - index * cluster_size) in
        Bytes.blit data (index * cluster_size) result off amount;
        write (index + 1) rest in
    write 0 chosen;
    if !existing = None && byte entry = 0 && entry + 32 < root + entries * 32 then
      set_byte (entry + 32) 0;
    Bytes.fill result entry 32 '\000';
    Bytes.blit_string name 0 result entry 11;
    set_byte (entry + 11) 0x20;
    set_word (entry + 26) (match chosen with [] -> 0 | first :: _ -> first);
    for i = 0 to 3 do set_byte (entry + 28 + i) (Bytes.length data lsr (8 * i)) done;
    Ok result
  with Reject error -> Error error

let put_file ~image ~name ~data = operate ~image ~name (Replace data)
let validate_writable_file ~image ~name = Result.map (fun _ -> ()) (operate ~image ~name Validate)
