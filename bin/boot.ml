(* C-BIOS 부트 하네스. ROM 세 개를 싣고 프레임을 돌린 뒤 화면을 PPM 으로
   덤프한다. 판정 재료: 실행 후 PC·VDP 레지스터·VRAM 체크섬·화면이
   검정이 아닌 픽셀 수. *)

let rom_dir = ref "roms/cbios/cbios-0.29a/roms"
let frames = ref 60
let out_prefix = ref "/tmp/msxboot"
let vlog = ref false

let read_file p =
  let ic = open_in_bin p in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let () =
  Arg.parse
    [ ("--vlog", Arg.Set vlog, "  VDP 포트 쓰기 로그");
      ("--frames", Arg.Int (fun n -> frames := n), "N  실행할 프레임");
      ("--roms", Arg.String (fun s -> rom_dir := s), "DIR  C-BIOS roms 디렉터리");
      ("--out", Arg.String (fun s -> out_prefix := s), "PREFIX  덤프 접두어") ]
    (fun _ -> ())
    "boot — C-BIOS 부트";
  let roms =
    List.map
      (fun f -> read_file (Filename.concat !rom_dir f))
      [ "cbios_main_msx2.rom"; "cbios_logo_msx2.rom"; "cbios_sub.rom" ]
  in
  let t =
    Msx.create ~machine:{ ram_kb = 512; vram_kb = 128; roms }
  in
  Msx.set_ldirvm_log true;
  Msx.set_pc_hist true;
  (try Msx.set_watch_enter 0x8000 0xc000 with Not_found -> ());
(try
   let v = Sys.getenv "TRACE_FROM" in
   let n = if String.length v > 4 && v.[2] = ':' then int_of_string (String.sub v 3 (String.length v - 3)) else 200 in
   Msx.set_trace_from (int_of_string ("0x" ^ String.sub v 0 4)) n
 with Not_found -> ());
  let () = ignore (Msx.ldirvm_log_calls ()) in
  (* 마지막 30개 고유 PC 구간을 남긴다 — 루프 구조 확인용. *)
  let ring = Array.make 64 0 in
  let ridx = ref 0 in
  let rec run_frame n =
    if n = 0 then ()
    else begin
      Msx.step t ~frames:1;
      ring.(!ridx land 63) <- Msx.dump_pc t;
      incr ridx;
      run_frame (n - 1)
    end
  in
  run_frame !frames;
  Printf.printf "last pcs:";
  for i = !ridx - 16 to !ridx - 1 do
    Printf.printf " %04x" ring.(i land 63)
  done;
  print_newline ();
  let rgb = Msx.frame_rgb t in
  (* 통계 *)
  let nonblack = ref 0 in
  String.iteri
    (fun i c ->
      let b = Char.code c in
      if i mod 3 = 0 && (b > 8 || Char.code rgb.[i + 1] > 8 || Char.code rgb.[i + 2] > 8) then
        incr nonblack)
    rgb;
  Printf.printf "frames=%d pc=%04x nonblack=%d\n%!" !frames (Msx.dump_pc t) !nonblack;
  Msx.debug_dump t;
  List.iter
    (fun (hl, de, bc) -> Printf.printf "ldirvm hl=%04x de=%04x bc=%04x\n" hl de bc)
    (Msx.ldirvm_log_calls ());
  let hist = Msx.pc_histogram () in
  Printf.printf "pc hist:";
  Array.iteri (fun i n -> if n > 1000 then Printf.printf " %02x:%d" i n) hist;
  print_newline ();
  (try
     let env = Sys.getenv "VRAM_DUMP" in
     let i = String.index env ',' in
     Msx.vram_hex t (int_of_string ("0x" ^ String.sub env 0 i))
       (int_of_string ("0x" ^ String.sub env (i + 1) (String.length env - i - 1)))
   with Not_found -> ());
  print_string (Msx.screen_text t);
  (if !vlog then
     List.iter
       (fun (p, a, v) -> Printf.printf "w %02x a=%04x v=%02x\n" p a v)
       (List.filter (fun (p, _, _) -> p = 0x99 || p = 0x98)
          (Msx.vdp_write_log t)));
  let oc = open_out_bin (!out_prefix ^ ".ppm") in
  Printf.fprintf oc "P6\n256 192\n255\n%s" rgb;
  close_out oc;
  Printf.printf "wrote %s.ppm\n" !out_prefix
