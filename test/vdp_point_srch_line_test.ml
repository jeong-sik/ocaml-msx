(* VDP 명령 POINT(4), SRCH(6), LINE(7)와 상태 레지스터 S#2(BD)·S#7·S#8·S#9 를
   openMSX(21.0, my8250cbios)와 맞춘다. 기대값은 같은 명령열을 MSX2 카트리지 ROM 으로
   만들어(레지스터 R#32..R#46 을 직접 쓰고 CE 가 내려갈 때까지 S#2 를 읽는다)
   openMSX 에서 돌린 뒤 물리 VRAM 128KB 와 읽은 상태 바이트를 덤프한 것이다.
   시나리오마다 32줄 띠(논리 순서 바이트)의 MD5 와, SRCH/POINT 뒤 S#2 의 BD·CE
   비트, S#7, S#8, S#9 를 적었다. 같은 ROM 을 이 에뮬레이터로 돌린 128KB VRAM 도
   openMSX 덤프와 모든 모드(SCREEN 5~8)에서 바이트 단위로 같았다. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let set_reg t r v =
  Vdp.io_write t ~port:0x99 v;
  Vdp.io_write t ~port:0x99 (0x80 lor r)

let status t n =
  set_reg t 15 n;
  let v = Vdp.io_read t ~port:0x99 in
  set_reg t 15 0;
  v

(* (sx, sy, dx, dy, nx, ny, colour, arg, command) *)
let issue t (sx, sy, dx, dy, nx, ny, col, arg, cmd) =
  List.iteri (fun i v -> set_reg t (32 + i) v)
    [ sx land 255; (sx lsr 8) land 1; sy land 255; (sy lsr 8) land 3;
      dx land 255; (dx lsr 8) land 1; dy land 255; (dy lsr 8) land 3;
      nx land 255; (nx lsr 8) land 1; ny land 255; (ny lsr 8) land 3;
      col; arg; cmd ]

let band_digest t ~planar ~stride ~y0 =
  let b = Bytes.create (32 * stride) in
  for k = 0 to (32 * stride) - 1 do
    let a = (y0 * stride) + k in
    let phys = if planar then ((a land 1) lsl 16) lor (a lsr 1) else a in
    Bytes.set b k (Char.chr (Vdp.vram_read t phys))
  done;
  Digest.to_hex (Digest.bytes b)

(* name, y0, commands, Some (S#2 & 0x11, S#7, S#8, S#9) after the last command, band MD5.
   띠의 MD5 는 시나리오를 전부 돌린 뒤에 잰다. openMSX 덤프가 그 시점의 VRAM 이고,
   "line y-major runs off the top" 는 위쪽 띠를 가로질러 그린다. *)
let run_mode (label, r0, planar, stride, scenarios, whole) =
  let t = Vdp.create () in
  List.iter (fun (r, v) -> set_reg t r v)
    [ (1, 0x00); (0, r0); (2, 0x1f); (7, 0); (8, 0x0a); (9, 0x80) ];
  issue t (0, 0, 0, 0, 0, 0, 0, 0, 0x80);
  List.iter
    (fun (name, _, cmds, results, _) ->
      List.iter (issue t) cmds;
      match results with
      | None -> ()
      | Some (s2, s7, s8, s9) ->
        let got2 = status t 2 land 0x11 in
        let got7 = status t 7 in
        let got8 = status t 8 in
        let got9 = status t 9 in
        check (Printf.sprintf "%s %s: S#2 BD/CE = 0x%02x, openMSX 0x%02x" label name got2 s2) (got2 = s2);
        check (Printf.sprintf "%s %s: S#7 = 0x%02x, openMSX 0x%02x" label name got7 s7) (got7 = s7);
        check (Printf.sprintf "%s %s: S#8 = 0x%02x, openMSX 0x%02x" label name got8 s8) (got8 = s8);
        check (Printf.sprintf "%s %s: S#9 = 0x%02x, openMSX 0x%02x" label name got9 s9) (got9 = s9))
    scenarios;
  List.iter
    (fun (name, y0, _, _, digest) ->
      let got = band_digest t ~planar ~stride ~y0 in
      check (Printf.sprintf "%s %s: band MD5 %s, openMSX %s" label name got digest) (got = digest))
    scenarios;
  let all = Bytes.create 0x20000 in
  for a = 0 to 0x1ffff do Bytes.set all a (Char.chr (Vdp.vram_read t a)) done;
  let got = Digest.to_hex (Digest.bytes all) in
  check (Printf.sprintf "%s whole VRAM MD5 %s, openMSX %s" label got whole) (got = whole)

let modes =
  [
  ( "G4", 0x06, false, 128,
    [
      ("line x-major down-right", 0, [ (0, 0, 10, 2, 50, 20, 5, 0x00, 0x70) ], None, "442301b3bfca47c3502b4f21d9594935");
      ("line x-major up-left", 32, [ (0, 0, 100, 60, 40, 15, 5, 0x0c, 0x70) ], None, "4e3ee1955bbd3b00646ae10b6273783a");
      ("line x-major up-right", 64, [ (0, 0, 20, 89, 60, 22, 5, 0x08, 0x70) ], None, "f1edc3ffeb0cdddb61a822626c9c16ad");
      ("line x-major down-left", 96, [ (0, 0, 120, 99, 70, 25, 5, 0x04, 0x70) ], None, "7affec63a3d8efc7b48e6a27590fc83b");
      ("line y-major down-right", 128, [ (0, 0, 30, 129, 28, 12, 5, 0x01, 0x70) ], None, "79fc07890d8d6a3ec1df5627d4d83d56");
      ("line y-major up-left", 160, [ (0, 0, 90, 190, 29, 20, 5, 0x0d, 0x70) ], None, "d91415d65323f08dcf398cf4856fd570");
      ("line single pixel", 192, [ (0, 0, 40, 197, 0, 0, 5, 0x00, 0x70) ], None, "88383eaf9ce0b67b70326475b09d70c9");
      ("line nx 0 ny 5", 224, [ (0, 0, 40, 229, 0, 5, 5, 0x00, 0x70) ], None, "88383eaf9ce0b67b70326475b09d70c9");
      ("line ny larger than nx", 256, [ (0, 0, 40, 258, 5, 20, 5, 0x00, 0x70) ], None, "d81a67d81345d101ec5486f4deea8b59");
      ("line runs off the right edge", 288, [ (0, 0, 230, 292, 60, 10, 5, 0x00, 0x70) ], None, "93244bf0382f2e0fa4013e03569aed71");
      ("line runs off the left edge", 320, [ (0, 0, 20, 324, 60, 10, 5, 0x04, 0x70) ], None, "0c2fd8ae361aefbfd78878f8784b2270");
      ("line xor over a filled band", 352, [ (0, 0, 0, 352, 255, 32, 3, 0x00, 0x80); (0, 0, 10, 355, 80, 20, 5, 0x00, 0x73) ], None, "99c6902738d9f1d84de9c01b9590a9df");
      ("line tint skips colour 0", 384, [ (0, 0, 0, 384, 255, 32, 3, 0x00, 0x80); (0, 0, 10, 387, 80, 20, 0, 0x00, 0x78) ], None, "bf038d6f8b1f4ad68c830be942458a55");
      ("line y-major runs off the top", 416, [ (0, 0, 60, 421, 40, 8, 5, 0x09, 0x70) ], None, "5461300af1cb48fcd96a9c3e294cfda9");
      ("srch finds colour", 448, [ (0, 0, 50, 449, 11, 1, 3, 0x00, 0x80); (10, 449, 0, 0, 0, 0, 3, 0x00, 0x60) ], Some (0x10, 0x03, 0x32, 0xfe), "dfd37f7714f221768b5defad46ffb42b");
      ("srch runs off the right edge", 480, [ (0, 0, 50, 481, 11, 1, 3, 0x00, 0x80); (70, 481, 0, 0, 0, 0, 3, 0x00, 0x60) ], Some (0x00, 0x03, 0x00, 0xff), "dfd37f7714f221768b5defad46ffb42b");
      ("srch leftwards finds colour", 512, [ (0, 0, 50, 513, 11, 1, 3, 0x00, 0x80); (100, 513, 0, 0, 0, 0, 3, 0x04, 0x60) ], Some (0x10, 0x03, 0x3c, 0xfe), "dfd37f7714f221768b5defad46ffb42b");
      ("srch not-equal in a block", 544, [ (0, 0, 50, 545, 11, 1, 3, 0x00, 0x80); (52, 545, 0, 0, 0, 0, 3, 0x02, 0x60) ], Some (0x10, 0x03, 0x3d, 0xfe), "dfd37f7714f221768b5defad46ffb42b");
      ("srch leftwards runs off", 576, [ (0, 0, 50, 577, 11, 1, 3, 0x00, 0x80); (40, 577, 0, 0, 0, 0, 3, 0x04, 0x60) ], Some (0x00, 0x03, 0xff, 0xff), "dfd37f7714f221768b5defad46ffb42b");
      ("point inside block", 608, [ (0, 0, 50, 609, 11, 1, 3, 0x00, 0x80); (55, 609, 0, 0, 0, 0, 0, 0x00, 0x40) ], Some (0x00, 0x03, 0xff, 0xff), "dfd37f7714f221768b5defad46ffb42b");
      ("point outside block", 640, [ (0, 0, 50, 641, 11, 1, 3, 0x00, 0x80); (20, 641, 0, 0, 0, 0, 0, 0x00, 0x40) ], Some (0x00, 0x00, 0xff, 0xff), "dfd37f7714f221768b5defad46ffb42b");
      ("line x-major error tie", 672, [ (0, 0, 10, 674, 50, 24, 5, 0x00, 0x70) ], None, "3580e83993edd295872349cd6ca67fc1");
      ("line x-major up error tie", 704, [ (0, 0, 100, 732, 50, 24, 5, 0x08, 0x70) ], None, "4ac98b127d1deb36f78412fb2eb8256a");
      ("line y-major error tie", 736, [ (0, 0, 30, 737, 50, 24, 5, 0x01, 0x70) ], None, "992d5d67763fcc5423413554705f5d99");
      ("line x-major up through the page top", 768, [ (0, 0, 60, 3, 40, 10, 5, 0x08, 0x70) ], None, "4d8d1fd21cac6a4a65c770d1441c989e");
      ("line y-major up through the page top", 800, [ (0, 0, 90, 5, 40, 8, 5, 0x09, 0x70) ], None, "620f0b67a91f7f74151bc5be745b7110");
      ("line x-major down past the page bottom", 832, [ (0, 0, 60, 1021, 40, 10, 5, 0x00, 0x70) ], None, "620f0b67a91f7f74151bc5be745b7110");
      ("line y-major down past the page bottom", 864, [ (0, 0, 90, 1019, 40, 8, 5, 0x01, 0x70) ], None, "620f0b67a91f7f74151bc5be745b7110");
    ],
    "2c4ce94ccff5a8dbe68db27d0b25d71d" );
  ( "G5", 0x08, false, 128,
    [
      ("line x-major down-right", 0, [ (0, 0, 10, 2, 50, 20, 2, 0x00, 0x70) ], None, "5d690eed873aab33b6649994fcc013c1");
      ("line x-major up-left", 32, [ (0, 0, 100, 60, 40, 15, 2, 0x0c, 0x70) ], None, "30eb30e6cf10e1eaa665bdd179b98dd2");
      ("line x-major up-right", 64, [ (0, 0, 20, 89, 60, 22, 2, 0x08, 0x70) ], None, "d56a88536f4c353e37d89024ff1a9078");
      ("line x-major down-left", 96, [ (0, 0, 120, 99, 70, 25, 2, 0x04, 0x70) ], None, "a1562ebacff753f1873fc518559b6b6a");
      ("line y-major down-right", 128, [ (0, 0, 30, 129, 28, 12, 2, 0x01, 0x70) ], None, "30bb0223b6d9c8771e0b9ffe87e5a1bb");
      ("line y-major up-left", 160, [ (0, 0, 90, 190, 29, 20, 2, 0x0d, 0x70) ], None, "df47ce2cf6d415ac3a1cee91cd15cb99");
      ("line single pixel", 192, [ (0, 0, 40, 197, 0, 0, 2, 0x00, 0x70) ], None, "0437dca7189813910f3dde20ae9db817");
      ("line nx 0 ny 5", 224, [ (0, 0, 40, 229, 0, 5, 2, 0x00, 0x70) ], None, "0437dca7189813910f3dde20ae9db817");
      ("line ny larger than nx", 256, [ (0, 0, 40, 258, 5, 20, 2, 0x00, 0x70) ], None, "c2cab9a53d0adc478433e458fa3777a5");
      ("line runs off the right edge", 288, [ (0, 0, 486, 292, 60, 10, 2, 0x00, 0x70) ], None, "c31b26d0000d961c964eec71af5f7490");
      ("line runs off the left edge", 320, [ (0, 0, 20, 324, 60, 10, 2, 0x04, 0x70) ], None, "8f8e34ef2e2c7209af3337e3495c5781");
      ("line xor over a filled band", 352, [ (0, 0, 0, 352, 511, 32, 3, 0x00, 0x80); (0, 0, 10, 355, 80, 20, 2, 0x00, 0x73) ], None, "2145e75f9c3cbf0d95f14f4cecbb1845");
      ("line tint skips colour 0", 384, [ (0, 0, 0, 384, 511, 32, 3, 0x00, 0x80); (0, 0, 10, 387, 80, 20, 0, 0x00, 0x78) ], None, "27cbbc478672fd95cd1389f5be759c95");
      ("line y-major runs off the top", 416, [ (0, 0, 60, 421, 40, 8, 2, 0x09, 0x70) ], None, "1fc10e89e5853e1def22500416ecbf4a");
      ("srch finds colour", 448, [ (0, 0, 50, 449, 11, 1, 3, 0x00, 0x80); (10, 449, 0, 0, 0, 0, 3, 0x00, 0x60) ], Some (0x10, 0x03, 0x32, 0xfe), "52d3610aa8f6d5c689cb824a474cf944");
      ("srch runs off the right edge", 480, [ (0, 0, 50, 481, 11, 1, 3, 0x00, 0x80); (70, 481, 0, 0, 0, 0, 3, 0x00, 0x60) ], Some (0x00, 0x03, 0x00, 0xfe), "52d3610aa8f6d5c689cb824a474cf944");
      ("srch leftwards finds colour", 512, [ (0, 0, 50, 513, 11, 1, 3, 0x00, 0x80); (100, 513, 0, 0, 0, 0, 3, 0x04, 0x60) ], Some (0x10, 0x03, 0x3c, 0xfe), "52d3610aa8f6d5c689cb824a474cf944");
      ("srch not-equal in a block", 544, [ (0, 0, 50, 545, 11, 1, 3, 0x00, 0x80); (52, 545, 0, 0, 0, 0, 3, 0x02, 0x60) ], Some (0x10, 0x03, 0x3d, 0xfe), "52d3610aa8f6d5c689cb824a474cf944");
      ("srch leftwards runs off", 576, [ (0, 0, 50, 577, 11, 1, 3, 0x00, 0x80); (40, 577, 0, 0, 0, 0, 3, 0x04, 0x60) ], Some (0x00, 0x03, 0xff, 0xff), "52d3610aa8f6d5c689cb824a474cf944");
      ("point inside block", 608, [ (0, 0, 50, 609, 11, 1, 3, 0x00, 0x80); (55, 609, 0, 0, 0, 0, 0, 0x00, 0x40) ], Some (0x00, 0x03, 0xff, 0xff), "52d3610aa8f6d5c689cb824a474cf944");
      ("point outside block", 640, [ (0, 0, 50, 641, 11, 1, 3, 0x00, 0x80); (20, 641, 0, 0, 0, 0, 0, 0x00, 0x40) ], Some (0x00, 0x00, 0xff, 0xff), "52d3610aa8f6d5c689cb824a474cf944");
      ("line x-major error tie", 672, [ (0, 0, 10, 674, 50, 24, 2, 0x00, 0x70) ], None, "49bf3c45c70193cec851a50174280db3");
      ("line x-major up error tie", 704, [ (0, 0, 100, 732, 50, 24, 2, 0x08, 0x70) ], None, "ce657188f0ff2b5eaceb9661ab8587cc");
      ("line y-major error tie", 736, [ (0, 0, 30, 737, 50, 24, 2, 0x01, 0x70) ], None, "d4884db02e4cbc1d7f611d299c98eee5");
      ("line x-major up through the page top", 768, [ (0, 0, 60, 3, 40, 10, 2, 0x08, 0x70) ], None, "557e853dff716fb70e7d54cfa61e26b4");
      ("line y-major up through the page top", 800, [ (0, 0, 90, 5, 40, 8, 2, 0x09, 0x70) ], None, "620f0b67a91f7f74151bc5be745b7110");
      ("line x-major down past the page bottom", 832, [ (0, 0, 60, 1021, 40, 10, 2, 0x00, 0x70) ], None, "620f0b67a91f7f74151bc5be745b7110");
      ("line y-major down past the page bottom", 864, [ (0, 0, 90, 1019, 40, 8, 2, 0x01, 0x70) ], None, "620f0b67a91f7f74151bc5be745b7110");
    ],
    "d2c9fa340a163feb4fd83e62ab2cd27c" );
  ( "G6", 0x0a, true, 256,
    [
      ("line x-major down-right", 0, [ (0, 0, 10, 2, 50, 20, 5, 0x00, 0x70) ], None, "0a2c8724b52f6e4c87a369429ad835b0");
      ("line x-major up-left", 32, [ (0, 0, 100, 60, 40, 15, 5, 0x0c, 0x70) ], None, "00b297e8322ea57067b632646fdfd59c");
      ("line y-major down-right", 64, [ (0, 0, 30, 65, 28, 12, 5, 0x01, 0x70) ], None, "10462b57d68a69762ed203a113434ed0");
      ("line runs off the right edge", 96, [ (0, 0, 486, 100, 60, 10, 5, 0x00, 0x70) ], None, "10751fda325d67056ee5bedf0cfb4292");
      ("line xor over a filled band", 128, [ (0, 0, 0, 128, 511, 32, 3, 0x00, 0x80); (0, 0, 10, 131, 80, 20, 5, 0x00, 0x73) ], None, "056b2740839025ea6dbfd79412ce3b78");
      ("srch finds colour", 160, [ (0, 0, 50, 161, 11, 1, 3, 0x00, 0x80); (10, 161, 0, 0, 0, 0, 3, 0x00, 0x60) ], Some (0x10, 0x03, 0x32, 0xfe), "80c7d3462d80b722d27eeaf30f68c2f1");
      ("srch runs off the right edge", 192, [ (0, 0, 50, 193, 11, 1, 3, 0x00, 0x80); (70, 193, 0, 0, 0, 0, 3, 0x00, 0x60) ], Some (0x00, 0x03, 0x00, 0xfe), "80c7d3462d80b722d27eeaf30f68c2f1");
      ("srch leftwards runs off", 224, [ (0, 0, 50, 225, 11, 1, 3, 0x00, 0x80); (40, 225, 0, 0, 0, 0, 3, 0x04, 0x60) ], Some (0x00, 0x03, 0xff, 0xff), "80c7d3462d80b722d27eeaf30f68c2f1");
      ("point inside block", 256, [ (0, 0, 50, 257, 11, 1, 3, 0x00, 0x80); (55, 257, 0, 0, 0, 0, 0, 0x00, 0x40) ], Some (0x00, 0x03, 0xff, 0xff), "80c7d3462d80b722d27eeaf30f68c2f1");
      ("line x-major error tie", 288, [ (0, 0, 10, 290, 50, 24, 5, 0x00, 0x70) ], None, "e48dd863dae873f731443768f7a849dd");
      ("line y-major error tie", 320, [ (0, 0, 30, 321, 50, 24, 5, 0x01, 0x70) ], None, "890135d32c7b87c160bb86ce130fbf99");
      ("line x-major up through the page top", 352, [ (0, 0, 60, 3, 40, 10, 5, 0x08, 0x70) ], None, "bd523f3de6a93b92936c76024b69b8e0");
      ("line y-major up through the page top", 384, [ (0, 0, 90, 5, 40, 8, 5, 0x09, 0x70) ], None, "0829f71740aab1ab98b33eae21dee122");
      ("line x-major down past the page bottom", 416, [ (0, 0, 60, 509, 40, 10, 5, 0x00, 0x70) ], None, "0829f71740aab1ab98b33eae21dee122");
      ("line y-major down past the page bottom", 448, [ (0, 0, 90, 507, 40, 8, 5, 0x01, 0x70) ], None, "0829f71740aab1ab98b33eae21dee122");
    ],
    "4f2257c744cdf4352e4dcc671b54d235" );
  ( "G7", 0x0e, true, 256,
    [
      ("line x-major down-right", 0, [ (0, 0, 10, 2, 50, 20, 165, 0x00, 0x70) ], None, "3531bb313d1a3c91242559a961eb1da3");
      ("line x-major up-left", 32, [ (0, 0, 100, 60, 40, 15, 165, 0x0c, 0x70) ], None, "93d1116d3ad1c5f1e65bef061c9f581f");
      ("line y-major down-right", 64, [ (0, 0, 30, 65, 28, 12, 165, 0x01, 0x70) ], None, "04238ed7b06e3fffb9ce0221e8e92e0a");
      ("line runs off the right edge", 96, [ (0, 0, 230, 100, 60, 10, 165, 0x00, 0x70) ], None, "e631976055ab22ac87c8c211dbc35335");
      ("line xor over a filled band", 128, [ (0, 0, 0, 128, 255, 32, 28, 0x00, 0x80); (0, 0, 10, 131, 80, 20, 165, 0x00, 0x73) ], None, "d1c811d06a55b31a055813ac02ede49d");
      ("srch finds colour", 160, [ (0, 0, 50, 161, 11, 1, 28, 0x00, 0x80); (10, 161, 0, 0, 0, 0, 28, 0x00, 0x60) ], Some (0x10, 0x1c, 0x32, 0xfe), "20654ad8f7ad144f78f253f3b2d31181");
      ("srch runs off the right edge", 192, [ (0, 0, 50, 193, 11, 1, 28, 0x00, 0x80); (70, 193, 0, 0, 0, 0, 28, 0x00, 0x60) ], Some (0x00, 0x1c, 0x00, 0xff), "20654ad8f7ad144f78f253f3b2d31181");
      ("srch leftwards runs off", 224, [ (0, 0, 50, 225, 11, 1, 28, 0x00, 0x80); (40, 225, 0, 0, 0, 0, 28, 0x04, 0x60) ], Some (0x00, 0x1c, 0xff, 0xff), "20654ad8f7ad144f78f253f3b2d31181");
      ("point inside block", 256, [ (0, 0, 50, 257, 11, 1, 28, 0x00, 0x80); (55, 257, 0, 0, 0, 0, 0, 0x00, 0x40) ], Some (0x00, 0x1c, 0xff, 0xff), "20654ad8f7ad144f78f253f3b2d31181");
      ("line x-major error tie", 288, [ (0, 0, 10, 290, 50, 24, 165, 0x00, 0x70) ], None, "76a388c507a4afa870dd53a2b221ba7e");
      ("line y-major error tie", 320, [ (0, 0, 30, 321, 50, 24, 165, 0x01, 0x70) ], None, "aea9b9b81f5ffe86a29e10d403e48bf6");
      ("line x-major up through the page top", 352, [ (0, 0, 60, 3, 40, 10, 165, 0x08, 0x70) ], None, "2bced0e8caf7f420919b756ce15c678b");
      ("line y-major up through the page top", 384, [ (0, 0, 90, 5, 40, 8, 165, 0x09, 0x70) ], None, "0829f71740aab1ab98b33eae21dee122");
      ("line x-major down past the page bottom", 416, [ (0, 0, 60, 509, 40, 10, 165, 0x00, 0x70) ], None, "0829f71740aab1ab98b33eae21dee122");
      ("line y-major down past the page bottom", 448, [ (0, 0, 90, 507, 40, 8, 165, 0x01, 0x70) ], None, "0829f71740aab1ab98b33eae21dee122");
    ],
    "7e5b34f340ecef6b94b3f614822a9968" );
  ]

(* SRCH 가 남긴 BD 와 경계 X 는 상태 저장·복원을 건너 살아남고, 이 값이 없는
   형식 6 상태는 BD 꺼짐·X 0 으로 복원된다. *)
let state_round_trip () =
  let t = Vdp.create () in
  List.iter (fun (r, v) -> set_reg t r v) [ (0, 0x06); (1, 0); (9, 0x80) ];
  issue t (0, 0, 50, 1, 11, 1, 3, 0x00, 0x80);
  issue t (10, 1, 0, 0, 0, 0, 3, 0x00, 0x60);
  let w = State_codec.writer () in
  Vdp.write_state w t;
  Vdp.write_search_state w t;
  let state = State_codec.finish w in
  let t2 = Vdp.create () in
  let r = State_codec.reader state in
  Vdp.read_state r t2;
  Vdp.read_search_state r t2;
  check "restored: S#2 BD stays set" (status t2 2 land 0x10 = 0x10);
  check "restored: S#8 = 0x32" (status t2 8 = 0x32);
  check "restored: S#9 = 0xfe" (status t2 9 = 0xfe);
  check "restored: reading S#9 clears BD" (status t2 2 land 0x10 = 0);
  let head = String.length "OCAML-MSX\000" + 1 in
  let payload = String.sub state (head + 16) (String.length state - head - 16) in
  let v6_payload = String.sub payload 0 (String.length payload - 9) in
  let v6 = "OCAML-MSX\000\006" ^ Digest.string v6_payload ^ v6_payload in
  let t3 = Vdp.create () in
  Vdp.read_state (State_codec.reader v6) t3;
  check "format 6: BD clear" (status t3 2 land 0x10 = 0);
  check "format 6: S#8 = 0" (status t3 8 = 0)

let () =
  state_round_trip ();
  List.iter run_mode modes;
  if !failures > 0 then begin
    Printf.eprintf "vdp_point_srch_line_test: %d failure(s)\n%!" !failures;
    exit 1
  end
  else print_endline "vdp_point_srch_line_test: all pass"
