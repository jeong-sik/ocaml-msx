(* Proof of the software database lookup: SHA-1 against the FIPS 180
   example vectors, the reader against a small database written here in
   the shape openMSX's softwaredb.xml has (RomDatabase.cc DBParser rules),
   and the type names against openMSX RomInfo.cc's names and aliases. *)

let failures = ref 0

let check name cond =
  if not cond then begin
    incr failures;
    Printf.eprintf "FAIL %s\n%!" name
  end

let () =
  (* FIPS 180-2 appendix A examples; the 56-byte one pads into a second
     block. *)
  List.iter
    (fun (name, msg, want) -> check ("sha1 " ^ name) (Sha1.hex msg = want))
    [ ("empty", "", "da39a3ee5e6b4b0d3255bfef95601890afd80709");
      ("abc", "abc", "a9993e364706816aba3e25717850c26c9cd0d89d");
      ( "56 bytes",
        "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
        "84983e441c3bd26ebaae4aa1f95129e5e54670f1" );
      ("a million a", String.make 1_000_000 'a', "34aa973cd4c4daa4f61eeb2bdbad27316534016f") ];

  (* Six "ROMs" and a database that lists them. *)
  let rom i = Printf.sprintf "rom %d" i in
  let h i = Sha1.hex (rom i) in
  let xml =
    String.concat "\n"
      [ {|<?xml version="1.0" encoding="UTF-8"?>|};
        {|<!DOCTYPE softwaredb SYSTEM "softwaredb1.dtd">|};
        {|<softwaredb timestamp="1">|};
        (* a comment and a CDATA section, each holding a '>' and then a
           whole dump, are not read *)
        Printf.sprintf {|<!-- a > b <dump><rom><hash>%s</hash></rom></dump> -->|}
          (Sha1.hex "in comment");
        Printf.sprintf {|<![CDATA[ a > b <dump><rom><hash>%s</hash></rom></dump> ]]>|}
          (Sha1.hex "in cdata");
        {|<software><title>A &amp; B</title>|};
        (* a megarom; an upper-case hash; a remark holding a '>' *)
        Printf.sprintf
          {|<dump><original value="true">GoodMSX</original><megarom><type>KonamiSCC</type><hash>%s</hash><remark><text>a > b</text></remark></megarom></dump>|}
          (String.uppercase_ascii (h 1));
        (* a rom with no type is Mirrored, a plain ROM *)
        Printf.sprintf {|<dump><rom><hash>%s</hash></rom></dump>|} (h 2);
        (* a placed plain rom *)
        Printf.sprintf
          {|<dump><rom><type>Normal</type><start>0x4000</start><hash>%s</hash></rom></dump>|}
          (h 3);
        (* an alias, in lower case *)
        Printf.sprintf {|<dump><megarom><type>konami4</type><hash>%s</hash></megarom></dump>|} (h 4);
        (* a type with no mapper here *)
        Printf.sprintf {|<dump><megarom><type>R-Type</type><hash>%s</hash></megarom></dump>|} (h 5);
        (* dropped: bad hashes, no hash, a repeat of rom 4's hash *)
        {|<dump><megarom><type>ASCII8</type><hash>not a sha1</hash></megarom></dump>|};
        Printf.sprintf {|<dump><megarom><type>ASCII8</type><hash>%s</hash></megarom></dump>|}
          (String.sub (Sha1.hex "39 digits") 0 39);
        {|<dump><megarom><type>ASCII16</type></megarom></dump>|};
        Printf.sprintf {|<dump><megarom><type>ASCII16</type><hash>%s</hash></megarom></dump>|} (h 4);
        (* a bad hash after a good one keeps the good one *)
        Printf.sprintf
          {|<dump><megarom><type>ASCII8</type><hash>%s</hash><hash>xyz</hash></megarom></dump>|}
          (h 6);
        "</software>";
        (* a hash outside any dump is not read *)
        Printf.sprintf {|<hash>%s</hash>|} (Sha1.hex "stray");
        "</softwaredb>" ]
  in
  (match Rom_db.parse xml with
   | Error m -> check ("parse: " ^ m) false
   | Ok db ->
     check "entries" (Rom_db.entries db = 6);
     check "skipped" (Rom_db.skipped db = 4);
     let is i want = check (Printf.sprintf "rom %d" i) (Rom_db.find db (rom i) = want) in
     is 1 (Some (Mapper Konami_scc));
     is 2 (Some (Mapper Flat));
     is 3 (Some (Mapper Flat));
     is 4 (Some (Mapper Konami));
     is 5 (Some (Not_emulated "R-Type"));
     is 6 (Some (Mapper Ascii8));
     List.iter
       (fun s -> check ("not listed: " ^ s) (Rom_db.find db s = None))
       [ "stray"; "in comment"; "in cdata" ]);

  List.iter
    (fun (name, xml) ->
      check ("malformed: " ^ name)
        (match Rom_db.parse xml with Error _ -> true | Ok _ -> false))
    [ ("comment", "<a><!-- no end");
      ("CDATA", "<a><![CDATA[ no end");
      ("tag", "<dump><rom");
      ("declaration", "<?xml no end") ];

  (* openMSX RomInfo.cc names and aliases, any case. *)
  List.iter
    (fun (name, want) ->
      check ("type " ^ name) (Rom_db.kind_of_name name = Rom_db.Mapper want))
    [ ("ASCII8", Msx.Ascii8); ("4", Msx.Ascii8);
      ("ASCII16", Msx.Ascii16); ("5", Msx.Ascii16);
      ("Konami", Msx.Konami); ("KONAMI4", Msx.Konami); ("3", Msx.Konami);
      ("KonamiSCC", Msx.Konami_scc); ("SCC", Msx.Konami_scc);
      ("KONAMI5", Msx.Konami_scc); ("2", Msx.Konami_scc);
      ("KoeiSRAM8", Msx.Ascii8_sram); ("KoeiSRAM32", Msx.Ascii8_sram);
      ("Mirrored", Msx.Flat); ("MirroredC000", Msx.Flat); ("Normal8000", Msx.Flat);
      ("page23", Msx.Flat); ("Page0123", Msx.Flat); ("64kB", Msx.Flat);
      ("Plain", Msx.Flat); ("0xC000", Msx.Flat); ("ROMBAS", Msx.Flat) ];
  List.iter
    (fun name ->
      check ("not emulated " ^ name) (Rom_db.kind_of_name name = Rom_db.Not_emulated name))
    [ "ASCII8SRAM8"; "ASCII16SRAM2"; "GenericKonami"; "16kB"; "R-Type"; "" ];

  List.iter
    (fun m ->
      check ("name " ^ Rom_db.mapper_name m)
        (Rom_db.mapper_of_name (Rom_db.mapper_name m) = Some m))
    [ Msx.Flat; Msx.Ascii8; Msx.Ascii16; Msx.Konami; Msx.Konami_scc; Msx.Ascii8_sram ];

  if !failures > 0 then exit 1;
  print_endline "rom_db_test: all pass"
