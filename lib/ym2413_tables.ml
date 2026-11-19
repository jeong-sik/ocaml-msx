(* Tables of the YM2413 (OPLL) model in {!Ym2413}, taken from emu2413 v1.6.3
   (https://github.com/digital-sound-antiques/emu2413, main @ 83254be).

   The MIT License (MIT)

   Copyright (c) 2001-2019 Mitsutaka Okazaki

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(* exp_table.(x) = round((2^(x / 256) - 1) * 1024): the fraction of the
   exponent the log-sin output is turned back into a level with. *)
let exp_table = [|
    0; 3; 6; 8; 11; 14; 17; 20; 22; 25; 28; 31; 34; 37; 40; 42;
    45; 48; 51; 54; 57; 60; 63; 66; 69; 72; 75; 78; 81; 84; 87; 90;
    93; 96; 99; 102; 105; 108; 111; 114; 117; 120; 123; 126; 130; 133; 136; 139;
    142; 145; 148; 152; 155; 158; 161; 164; 168; 171; 174; 177; 181; 184; 187; 190;
    194; 197; 200; 204; 207; 210; 214; 217; 220; 224; 227; 231; 234; 237; 241; 244;
    248; 251; 255; 258; 262; 265; 268; 272; 276; 279; 283; 286; 290; 293; 297; 300;
    304; 308; 311; 315; 318; 322; 326; 329; 333; 337; 340; 344; 348; 352; 355; 359;
    363; 367; 370; 374; 378; 382; 385; 389; 393; 397; 401; 405; 409; 412; 416; 420;
    424; 428; 432; 436; 440; 444; 448; 452; 456; 460; 464; 468; 472; 476; 480; 484;
    488; 492; 496; 501; 505; 509; 513; 517; 521; 526; 530; 534; 538; 542; 547; 551;
    555; 560; 564; 568; 572; 577; 581; 585; 590; 594; 599; 603; 607; 612; 616; 621;
    625; 630; 634; 639; 643; 648; 652; 657; 661; 666; 670; 675; 680; 684; 689; 693;
    698; 703; 708; 712; 717; 722; 726; 731; 736; 741; 745; 750; 755; 760; 765; 770;
    774; 779; 784; 789; 794; 799; 804; 809; 814; 819; 824; 829; 834; 839; 844; 849;
    854; 859; 864; 869; 874; 880; 885; 890; 895; 900; 906; 911; 916; 921; 927; 932;
    937; 942; 948; 953; 959; 964; 969; 975; 980; 986; 991; 996; 1002; 1007; 1013; 1018;
  |]

(* The first quarter of the log-sin wave:
   round(-log2(sin((x + 0.5) * pi / 512)) * 256). The other three quarters
   are mirrors of it ({!Ym2413}). *)
let quarter_sin = [|
    2137; 1731; 1543; 1419; 1326; 1252; 1190; 1137; 1091; 1050; 1013; 979; 949; 920; 894; 869;
    846; 825; 804; 785; 767; 749; 732; 717; 701; 687; 672; 659; 646; 633; 621; 609;
    598; 587; 576; 566; 556; 546; 536; 527; 518; 509; 501; 492; 484; 476; 468; 461;
    453; 446; 439; 432; 425; 418; 411; 405; 399; 392; 386; 380; 375; 369; 363; 358;
    352; 347; 341; 336; 331; 326; 321; 316; 311; 307; 302; 297; 293; 289; 284; 280;
    276; 271; 267; 263; 259; 255; 251; 248; 244; 240; 236; 233; 229; 226; 222; 219;
    215; 212; 209; 205; 202; 199; 196; 193; 190; 187; 184; 181; 178; 175; 172; 169;
    167; 164; 161; 159; 156; 153; 151; 148; 146; 143; 141; 138; 136; 134; 131; 129;
    127; 125; 122; 120; 118; 116; 114; 112; 110; 108; 106; 104; 102; 100; 98; 96;
    94; 92; 91; 89; 87; 85; 83; 82; 80; 78; 77; 75; 74; 72; 70; 69;
    67; 66; 64; 63; 62; 60; 59; 57; 56; 55; 53; 52; 51; 49; 48; 47;
    46; 45; 43; 42; 41; 40; 39; 38; 37; 36; 35; 34; 33; 32; 31; 30;
    29; 28; 27; 26; 25; 24; 23; 23; 22; 21; 20; 20; 19; 18; 17; 17;
    16; 15; 15; 14; 13; 13; 12; 12; 11; 10; 10; 9; 9; 8; 8; 7;
    7; 7; 6; 6; 5; 5; 5; 4; 4; 4; 3; 3; 3; 2; 2; 2;
    2; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 0; 0;
  |]

(* Vibrato: the F-number offset for the top three F-number bits (row) and
   the LFO step (column), about 14 cents deep. *)
let pm_table = [|
    [| 0; 0; 0; 0; 0; 0; 0; 0 |];
    [| 0; 0; 1; 0; 0; 0; -1; 0 |];
    [| 0; 1; 2; 1; 0; -1; -2; -1 |];
    [| 0; 1; 3; 1; 0; -1; -3; -1 |];
    [| 0; 2; 4; 2; 0; -2; -4; -2 |];
    [| 0; 2; 5; 2; 0; -2; -5; -2 |];
    [| 0; 3; 6; 3; 0; -3; -6; -3 |];
    [| 0; 3; 7; 3; 0; -3; -7; -3 |];
  |]

(* Tremolo: the attenuation added at each LFO step. Each entry lasts 64
   chip samples. *)
let am_table = [|
    0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 1; 1; 1; 1; 1;
    2; 2; 2; 2; 2; 2; 2; 2; 3; 3; 3; 3; 3; 3; 3; 3;
    4; 4; 4; 4; 4; 4; 4; 4; 5; 5; 5; 5; 5; 5; 5; 5;
    6; 6; 6; 6; 6; 6; 6; 6; 7; 7; 7; 7; 7; 7; 7; 7;
    8; 8; 8; 8; 8; 8; 8; 8; 9; 9; 9; 9; 9; 9; 9; 9;
    10; 10; 10; 10; 10; 10; 10; 10; 11; 11; 11; 11; 11; 11; 11; 11;
    12; 12; 12; 12; 12; 12; 12; 12; 13; 13; 13; 12; 12; 12; 12; 12;
    12; 12; 12; 11; 11; 11; 11; 11; 11; 11; 11; 10; 10; 10; 10; 10;
    10; 10; 10; 9; 9; 9; 9; 9; 9; 9; 9; 8; 8; 8; 8; 8;
    8; 8; 8; 7; 7; 7; 7; 7; 7; 7; 7; 6; 6; 6; 6; 6;
    6; 6; 6; 5; 5; 5; 5; 5; 5; 5; 5; 4; 4; 4; 4; 4;
    4; 4; 4; 3; 3; 3; 3; 3; 3; 3; 3; 2; 2; 2; 2; 2;
    2; 2; 2; 1; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0;
    0; 0;
  |]

(* Envelope step patterns for the low two rate bits. *)
let eg_step_tables = [|
    [| 0; 1; 0; 1; 0; 1; 0; 1 |];
    [| 0; 1; 0; 1; 1; 1; 0; 1 |];
    [| 0; 1; 1; 1; 0; 1; 1; 1 |];
    [| 0; 1; 1; 1; 1; 1; 1; 1 |];
  |]

(* The YM2413's instrument ROM: 8 bytes per instrument in register 0-7
   order. 0 is the user instrument; 16-18 are the rhythm instruments.
   1-15 are emu2413's. openMSX's NukeYKT core differs in 10 of them and its
   Okazaki core in all 15; no hardware recording here settles which is right. *)
let default_instruments = [|
    [| 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00 |]; (* 0: user *)
    [| 0x71; 0x61; 0x1e; 0x17; 0xd0; 0x78; 0x00; 0x17 |]; (* 1: violin *)
    [| 0x13; 0x41; 0x1a; 0x0d; 0xd8; 0xf7; 0x23; 0x13 |]; (* 2: guitar *)
    [| 0x13; 0x01; 0x99; 0x00; 0xf2; 0xc4; 0x21; 0x23 |]; (* 3: piano *)
    [| 0x11; 0x61; 0x0e; 0x07; 0x8d; 0x64; 0x70; 0x27 |]; (* 4: flute *)
    [| 0x32; 0x21; 0x1e; 0x06; 0xe1; 0x76; 0x01; 0x28 |]; (* 5: clarinet *)
    [| 0x31; 0x22; 0x16; 0x05; 0xe0; 0x71; 0x00; 0x18 |]; (* 6: oboe *)
    [| 0x21; 0x61; 0x1d; 0x07; 0x82; 0x81; 0x11; 0x07 |]; (* 7: trumpet *)
    [| 0x33; 0x21; 0x2d; 0x13; 0xb0; 0x70; 0x00; 0x07 |]; (* 8: organ *)
    [| 0x61; 0x61; 0x1b; 0x06; 0x64; 0x65; 0x10; 0x17 |]; (* 9: horn *)
    [| 0x41; 0x61; 0x0b; 0x18; 0x85; 0xf0; 0x81; 0x07 |]; (* 10: synthesizer *)
    [| 0x33; 0x01; 0x83; 0x11; 0xea; 0xef; 0x10; 0x04 |]; (* 11: harpsichord *)
    [| 0x17; 0xc1; 0x24; 0x07; 0xf8; 0xf8; 0x22; 0x12 |]; (* 12: vibraphone *)
    [| 0x61; 0x50; 0x0c; 0x05; 0xd2; 0xf5; 0x40; 0x42 |]; (* 13: synth bass *)
    [| 0x01; 0x01; 0x55; 0x03; 0xe9; 0x90; 0x03; 0x02 |]; (* 14: acoustic bass *)
    [| 0x41; 0x41; 0x89; 0x03; 0xf1; 0xe4; 0xc0; 0x13 |]; (* 15: electric guitar *)
    [| 0x01; 0x01; 0x18; 0x0f; 0xdf; 0xf8; 0x6a; 0x6d |]; (* 16: bass drum *)
    [| 0x01; 0x01; 0x00; 0x00; 0xc8; 0xd8; 0xa7; 0x68 |]; (* 17: hi-hat / snare drum *)
    [| 0x05; 0x01; 0x00; 0x00; 0xf8; 0xaa; 0x59; 0x55 |]; (* 18: tom-tom / top cymbal *)
  |]
