(* SHA-1 as FIPS 180-4 section 6.1 states it: pad to 64-byte blocks, then
   80 rounds per block over five 32-bit words. OCaml ints are 63 bits, so
   every sum and shift is masked back to 32. *)

let mask32 = 0xffffffff
let block = 64

(* Initial hash value (section 5.3.1) and the round constant of each group
   of 20 rounds (section 4.2.1). *)
let h_init = [| 0x67452301; 0xefcdab89; 0x98badcfe; 0x10325476; 0xc3d2e1f0 |]
let k = [| 0x5a827999; 0x6ed9eba1; 0x8f1bbcdc; 0xca62c1d6 |]
let rounds_per_group = 20

let rol x n = ((x lsl n) lor (x lsr (32 - n))) land mask32

(* The round function of each group (section 4.1.1): Ch, Parity, Maj,
   Parity. *)
let f group b c d =
  match group with
  | 0 -> (b land c) lor (lnot b land d) land mask32
  | 2 -> (b land c) lor (b land d) lor (c land d)
  | _ -> b lxor c lxor d

(* The message, a 0x80 byte, zeros, and the bit length as 8 big-endian
   bytes, in whole blocks. *)
let pad s =
  let len = String.length s in
  let total = ((len + 1 + 8 + block - 1) / block) * block in
  let m = Bytes.make total '\000' in
  Bytes.blit_string s 0 m 0 len;
  Bytes.set m len '\x80';
  let bits = len * 8 in
  for i = 0 to 7 do
    Bytes.set m (total - 1 - i) (Char.chr ((bits lsr (8 * i)) land 0xff))
  done;
  m

let hex s =
  let m = pad s in
  let h = Array.copy h_init in
  let w = Array.make 80 0 in
  for b = 0 to (Bytes.length m / block) - 1 do
    for t = 0 to 15 do
      w.(t) <- Int32.to_int (Bytes.get_int32_be m ((b * block) + (t * 4))) land mask32
    done;
    for t = 16 to 79 do
      w.(t) <- rol (w.(t - 3) lxor w.(t - 8) lxor w.(t - 14) lxor w.(t - 16)) 1
    done;
    let a = ref h.(0) and bb = ref h.(1) and c = ref h.(2) and d = ref h.(3) and e = ref h.(4) in
    for t = 0 to 79 do
      let g = t / rounds_per_group in
      let tmp = (rol !a 5 + f g !bb !c !d + !e + k.(g) + w.(t)) land mask32 in
      e := !d;
      d := !c;
      c := rol !bb 30;
      bb := !a;
      a := tmp
    done;
    List.iteri (fun i v -> h.(i) <- (h.(i) + v) land mask32) [ !a; !bb; !c; !d; !e ]
  done;
  String.concat "" (Array.to_list (Array.map (Printf.sprintf "%08x") h))
