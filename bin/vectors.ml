(* ADC/SBC HL 단위 벡터 — zexall.src 의 adc16 시드와 손계산 기대값.
   각 케이스: (명령 바이트열, 시드 AF/BC/DE/HL/SP, 기대 HL, 기대 F). *)

let mem = Bytes.make 0x10000 '\000'

let run ~prog ~af ~bc ~de ~hl ~sp =
  Bytes.fill mem 0 0x10000 '\000';
  List.iteri (fun i b -> Bytes.set mem (0x100 + i) (Char.chr b)) prog;
  let z =
    Z80.create
      ~read:(fun a -> Char.code (Bytes.get mem (a land 0xffff)))
      ~write:(fun a v -> Bytes.set mem (a land 0xffff) (Char.chr (v land 0xff)))
      ~port_in:(fun _ -> 0xff)
      ~port_out:(fun _ _ -> ())
  in
  Z80.set_af z af;
  Z80.set_bc z bc;
  Z80.set_de z de;
  Z80.set_hl z hl;
  Z80.set_sp z sp;
  Z80.set_pc z 0x0100;
  ignore (Z80.step z);
  (Z80.dump_hl z, Z80.dump_f z)

let cases =
  [ ( "SBC HL,BC seed1 (src 0x4f88, dst 0xb339, c=0)",
      [ 0xED; 0x42 ], 0x0832, 0x4F88, 0x0F22, 0xB339, 0x7E1F, 0x63B1, 0x36 )
  ; ( "ADC HL,BC seed1 (c=0): 0xb339+0x4f88",
      [ 0xED; 0x4A ], 0x0832, 0x4F88, 0x0F22, 0xB339, 0x7E1F, 0x02C1, 0x11 )
  ; ( "ADC HL,HL zero (c=0)",
      [ 0xED; 0x6A ], 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x40 )
  ; ( "ADC HL,HL 0x8000+0x8000 (c=0)",
      [ 0xED; 0x6A ], 0x0000, 0x0000, 0x0000, 0x8000, 0x0000, 0x0000, 0x45 )
  ]

let () =
  let fail = ref false in
  List.iter
    (fun (name, prog, af, bc, de, hl, sp, want_hl, want_f) ->
      let got_hl, got_f = run ~prog ~af ~bc ~de ~hl ~sp in
      let ok = got_hl = want_hl && got_f = want_f in
      if not ok then fail := true;
      Printf.printf "%-42s hl=%04x(want %04x) f=%02x(want %02x) %s\n%!"
        name got_hl want_hl got_f want_f
        (if ok then "ok" else "FAIL"))
    cases;
  exit (if !fail then 1 else 0)
