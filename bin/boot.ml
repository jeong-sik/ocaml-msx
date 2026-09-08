(* C-BIOS 부트 하네스. ROM 세 개를 싣고 프레임을 돌린 뒤 화면을 PPM 으로
   덤프한다. 판정 재료: 실행 후 PC·VDP 레지스터·VRAM 체크섬·화면이
   검정이 아닌 픽셀 수. *)

let rom_dir = ref "roms/cbios/cbios-0.29a/roms"
let frames = ref 60
let out_prefix = ref "/tmp/msxboot"
let vlog = ref false
let watch_mem = ref ""
let cart = ref ""
let disk = ref ""
let disk_warm = ref false
let cart_mapper = ref ""
let tap_space : int list ref = ref []
let assert_boot = ref false

let contains_sub hay needle =
  let n = String.length needle and m = String.length hay in
  n = 0
  ||
  let rec at i j = j = n || (hay.[i + j] = needle.[j] && at i (j + 1)) in
  let rec go i = i + n <= m && (at i 0 || go (i + 1)) in
  go 0

let count_nonblack rgb =
  let n = ref 0 in
  String.iteri
    (fun i c ->
      let b = Char.code c in
      if i mod 3 = 0 && (b > 8 || Char.code rgb.[i + 1] > 8 || Char.code rgb.[i + 2] > 8) then
        incr n)
    rgb;
  !n

let read_file p =
  let ic = open_in_bin p in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let () =
  Arg.parse
    [ ("--vlog", Arg.Set vlog, "  VDP 포트 쓰기 로그");
      ("--assert-boot", Arg.Set assert_boot, "  부트 완주 판정 (로고 렌더 + No cartridge), 어긋나면 exit 1");
      ("--cart", Arg.Set_string cart, "PATH  카트리지 ROM — 슬롯2 페이지1 에.");
      ("--disk", Arg.Set_string disk, "PATH  플로피 이미지(.dsk) — 드라이브 A.");
      ( "--disk-warm",
        Arg.Set disk_warm,
        "  720프레임 워밍업 뒤 Disk ROM 2차 호출 재생 (게임 화면 경로)" );
      ( "--cart-mapper",
        Arg.String (fun s -> cart_mapper := s),
        "NAME  mapper override: plain|ascii8|ascii16|konami|konami-scc" );
      ( "--tap-space",
        Arg.String
          (fun s -> tap_space := List.map int_of_string (String.split_on_char ',' s)),
        "N[,N..]  해당 프레임마다 스페이스 탭 (down 5프레임)" );
      ("--watch-mem", Arg.Set_string watch_mem, "A,B,C  RAM 쓰기 감시 (hex, 콤마 구분)");
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
  (* [begin..end] bounds the then-branch: without it the [let mapper = .. in]
     swallows every following statement (run loop, prints), so a no-cart boot
     (BIOS only or disk) ran nothing and exited silently. *)
  if !cart <> "" then begin
    let mapper = match !cart_mapper with
      | "plain" -> Some Msx.Flat
      | "ascii8" -> Some Msx.Ascii8
      | "ascii16" -> Some Msx.Ascii16
      | "konami" -> Some Msx.Konami
      | "konami-scc" -> Some Msx.Konami_scc
      | "koei" -> Some Msx.Ascii8_sram
      | "" -> None
      | other -> Printf.ksprintf failwith "unknown --cart-mapper %s" other
    in
    Msx.load_cartridge ?mapper t (read_file !cart)
  end;
  if !disk <> "" then begin
    Msx.load_disk ~interface_rom:(not !disk_warm) t (read_file !disk);
    Msx.set_disk_call_log true
  end;
  Msx.set_ldirvm_log true;
  Msx.set_pc_hist true;
  if !watch_mem <> "" then
    Msx.set_watch_mem
      (List.map (fun s -> int_of_string ("0x" ^ s)) (String.split_on_char ',' !watch_mem));
  (try
     (* WATCH_ENTER_AT 이 있으면 프레임 루프 안에서 그 프레임에 건다 —
        0x0000 같은 부트 주소를 처음이 아니라 재진입에서 잡으려면. *)
     ignore (Sys.getenv "WATCH_ENTER_AT");
     ()
   with Not_found ->
   try
     let env = Sys.getenv "WATCH_ENTER" in
     let c = String.index env ',' in
     Msx.set_watch_enter
       (int_of_string ("0x" ^ String.sub env 0 c))
       (int_of_string ("0x" ^ String.sub env (c + 1) (String.length env - c - 1)))
   with Not_found -> Msx.set_watch_enter 0x8000 0xc000);
(try
   let v = Sys.getenv "TRACE_FROM" in
   let c = String.index v (char_of_int 58) in let n = int_of_string (String.sub v (c + 1) (String.length v - c - 1)) in
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
      (try
         let env = Sys.getenv "WATCH_ENTER_AT" in
         if !ridx = int_of_string env then begin
           let e = Sys.getenv "WATCH_ENTER" in
           let c = String.index e ',' in
           Msx.set_watch_enter
             (int_of_string ("0x" ^ String.sub e 0 c))
             (int_of_string ("0x" ^ String.sub e (c + 1) (String.length e - c - 1)))
         end
       with Not_found -> ());
      if List.mem !ridx !tap_space then assert (Msx.set_key t Space ~pressed:true);
      if List.exists (fun f -> f + 5 = !ridx) !tap_space then
        assert (Msx.set_key t Space ~pressed:false);
      ring.(!ridx land 63) <- Msx.dump_pc t;
      incr ridx;
      let dlo, dhi =
        try
          let env = Sys.getenv "DENSE" in
          let c = String.index env ',' in
          (int_of_string (String.sub env 0 c),
           int_of_string (String.sub env (c + 1) (String.length env - c - 1)))
        with Not_found -> (500, 525)
      in
      if !ridx mod 30 = 0 || (!ridx >= dlo && !ridx <= dhi) then begin
        let rgb = Msx.frame_rgb t in
        let nb = count_nonblack rgb in
        Printf.eprintf
          "f=%d pc=%04x s0=%02x irq=%b halt=%b R1=%02x nb=%d ppi=%02x sl3=%02x\n%!"
          !ridx (Msx.dump_pc t) (Msx.vdp_status0 t) (Msx.vdp_irq_active t)
          (Msx.cpu_halted t) (Msx.vdp_regs t).(1) nb (Msx.ppi_a t)
          (Msx.slot3_sel t);
        let ln, cy = Msx.vdp_line t in
        Printf.eprintf "  ln=%d cy=%d R15=%02x\n%!" ln cy (Msx.vdp_regs t).(15)
      end;
      run_frame (n - 1)
    end
  in
  (* 부트 완주 자동 판정: 로고(SCREEN5 비트맵) → 페이드 → "No cartridge".
     로고는 텍스트가 아니라 픽셀 수로, 최종 화면은 screen_text 로 잡는다. *)
  if !assert_boot then begin
    run_frame 30;
    let nb_logo = count_nonblack (Msx.frame_rgb t) in
    if nb_logo < 10_000 then begin
      Printf.eprintf "assert boot FAIL: logo not rendered (f=30 nonblack=%d)\n%!" nb_logo;
      exit 1
    end;
    run_frame 570;
    let txt = Msx.screen_text t in
    if not (contains_sub txt "No cartridge") then begin
      Printf.eprintf "assert boot FAIL: no \"No cartridge\" at f=600\n%s\n%!" txt;
      exit 1
    end;
    let nb_final = count_nonblack (Msx.frame_rgb t) in
    if nb_final < 49_000 then begin
      Printf.eprintf "assert boot FAIL: final screen dark (nonblack=%d)\n%!" nb_final;
      exit 1
    end;
    Printf.printf "assert boot ok: logo=%d final=%d\n" nb_logo nb_final;
    exit 0
  end;
  (* --disk-warm: C-BIOS 를 720 프레임 정주행시켜 F380 인터슬롯 프리미티브를
     RAM 에 심은 뒤 Disk ROM 의 2차 호출(0xC01E CY=1)을 재생한다 — 카트
     INIT 경로가 KOEI.SYS 진입 후 재부트하는 동안 (실측 f≈330), 게임 화면
     까지 가는 쪽이다. *)
  (if !disk_warm && !disk <> "" then begin
     run_frame (min !frames 720);
     match Msx.boot_disk t with
     | Ok () -> ()
     | Error m -> Printf.ksprintf failwith "disk boot: %s" m
   end);
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
  Printf.printf "frames=%d pc=%04x nonblack=%d mode=%s\n%!" !frames (Msx.dump_pc t) !nonblack
    (Msx.display_mode_to_string (Msx.display_mode t));
  if !disk <> "" then begin
    let calls = Msx.disk_call_entries () in
    Printf.printf "disk calls=%d\n" (List.length calls);
    List.iteri
      (fun i (pc, a, bc, de, hl, f) ->
        if i < 20 then
          Printf.printf "  disk @%04x a=%02x bc=%04x de(sec)=%04x hl(addr)=%04x f=%02x\n"
            pc a bc de hl f)
      calls;
    Array.iteri
      (fun i n -> if n > 0 then Printf.printf "bdos %02x: %d\n" i n)
      (Msx.bdos_counts ())
  end;
  Msx.debug_dump t;
  (* SCREEN7 판정 보조: 64K 페이지별로 256바이트 줄(한 표시 줄)의 non-zero
     바이트 수. 한 줄 걸러 비면 인터레이스, 반대 페이지에 있으면 베이스
     오산정, 골고루 차 있으면 렌더 버그. *)
  (try
     ignore (Sys.getenv "ROWSTAT");
     for p = 0 to 1 do
       let base = p * 0x10000 in
       Printf.eprintf "rows page%d:\n%!" p;
       for y = 0 to 211 do
         let c = ref 0 in
         for x = 0 to 255 do
           if Msx.vram_read t (base + (y * 256) + x) <> 0 then incr c
         done;
         Printf.eprintf "%d " !c;
         if y mod 32 = 31 then Printf.eprintf "\n%!"
       done;
       Printf.eprintf "\n%!"
     done
   with Not_found -> ());
  (* SCREEN7 진단: 페이지마다 렌더러와 같은 패킹(2px/바이트, 256B/줄)으로
     전폭 512×212 덤프 — R#2 가 고른 페이지와 그림이 실제로 있는 페이지가
     갈리는지, 다운샘플이 무늬를 부수는지 본다 (삼국지2 moire 실측). *)
  (try
     let n = int_of_string (Sys.getenv "G6_PAGES") in
     let pal = Msx.palette_entries t in
     for p = 0 to n - 1 do
       let base = p * 0x10000 in
       let img = Bytes.make (512 * 212 * 3) '\000' in
       for y = 0 to 211 do
         for x = 0 to 511 do
           let b = Msx.vram_read t (base + (y * 256) + (x lsr 1)) in
           let nib = if x land 1 = 0 then b lsr 4 else b land 15 in
           let r, g, bl = pal.(nib) in
           let i = (y * 512 + x) * 3 in
           Bytes.set img i (Char.chr r);
           Bytes.set img (i + 1) (Char.chr g);
           Bytes.set img (i + 2) (Char.chr bl)
         done
       done;
       let oc = open_out_bin (Printf.sprintf "%s.g6p%d.ppm" !out_prefix p) in
       Printf.fprintf oc "P6\n512 212\n255\n%s" (Bytes.to_string img);
       close_out oc
     done
   with Not_found -> ());
  let (active, n, ny, anx, dy) = Msx.tx_state t in
  Printf.printf "tx active=%b count=%d ny=%d anx=%d dy=%d\n" active n ny anx dy;
  List.iter
    (fun (c, dx, dy, ny) -> Printf.printf "cmd cmr=%02x dx=%d dy=%d ny=%d\n" c dx dy ny)
    (Msx.cmd_history t);
  List.iter
    (fun (hl, de, bc) -> Printf.printf "ldirvm hl=%04x de=%04x bc=%04x\n" hl de bc)
    (Msx.ldirvm_log_calls ());
  List.iter
    (fun (n, a, v, pc) ->
      Printf.printf "watch #%d %04x=%02x @%04x\n" n a v pc)
    (Msx.watch_mem_entries ());
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
  (try
     let env = Sys.getenv "RAM_DUMP" in
     let i = String.index env ',' in
     Msx.ram_hex t (int_of_string ("0x" ^ String.sub env 0 i))
       (int_of_string ("0x" ^ String.sub env (i + 1) (String.length env - i - 1)))
   with Not_found -> ());
  (* MEM_DUMP: RAM_DUMP 가 매퍼 세그먼트 0 기준이라 페이지3(부트섹터·KOEI
     코드)을 못 본다 — Z80 이 보는 주소 그대로 (mem_read) 덤프. *)
  (try
     let env = Sys.getenv "MEM_DUMP" in
     let i = String.index env ',' in
     let a0 = int_of_string ("0x" ^ String.sub env 0 i) in
     let n = int_of_string ("0x" ^ String.sub env (i + 1) (String.length env - i - 1)) in
     for row = 0 to (n - 1) / 16 do
       Printf.eprintf "mem %04x:" (a0 + (row * 16));
       for j = 0 to 15 do
         Printf.eprintf " %02x"
           (Msx.mem_read t ((a0 + (row * 16) + j) land 0xffff))
       done;
       Printf.eprintf "\n%!"
     done
   with Not_found -> ());
  print_string (Msx.screen_text t);
  (if !vlog then
     List.iter
       (fun (p, a, v) -> Printf.printf "w %02x a=%05x v=%02x\n" p a v)
       (List.filter (fun (p, _, _) -> p = 0x99 || p = 0x98 || p = 0x9B)
          (Msx.vdp_write_log t)));
  let oc = open_out_bin (!out_prefix ^ ".ppm") in
  let w, h = Msx.frame_dims t in
  Printf.fprintf oc "P6\n%d %d\n255\n%s" w h rgb;
  close_out oc;
  Printf.printf "wrote %s.ppm\n" !out_prefix
