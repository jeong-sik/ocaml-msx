(* C-BIOS 부트 하네스. ROM 세 개를 싣고 프레임을 돌린 뒤 화면을 PPM 으로
   덤프한다. 판정 재료: 실행 후 PC·VDP 레지스터·VRAM 체크섬·화면이
   검정이 아닌 픽셀 수. *)

let rom_dir = ref "roms/cbios/cbios-0.29a/roms"
let frames = ref 60
let out_prefix = ref "/tmp/msxboot"

let read_file p =
  let ic = open_in_bin p in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let () =
  Arg.parse
    [ ("--frames", Arg.Int (fun n -> frames := n), "N  실행할 프레임");
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
  print_string (Msx.screen_text t);
  let oc = open_out_bin (!out_prefix ^ ".ppm") in
  Printf.fprintf oc "P6\n256 192\n255\n%s" rgb;
  close_out oc;
  Printf.printf "wrote %s.ppm\n" !out_prefix
