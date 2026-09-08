(** FAT12 root-file replacement. Validates geometry and allocation before
    producing new image bytes; failures never mutate the input image. *)
type error = Invalid_image | Invalid_name | Directory_full | Disk_full | Read_only
val put_file : image:bytes -> name:string -> data:bytes -> (bytes, error) result

(** Validate a writable root-file lookup, including BPB bounds, FAT agreement,
    all root allocation chains and the target file size, without mutation. *)
val validate_writable_file : image:bytes -> name:string -> (unit, error) result
