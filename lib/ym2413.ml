(* YM2413 (OPLL), the FM chip of MSX-MUSIC and the FM-PAC. A port of emu2413
   v1.6.3 (https://github.com/digital-sound-antiques/emu2413, main @ 83254be)
   to OCaml, without its rate converter, VRC7/YMF281B tone sets, pan and
   channel mask: the chip runs at clock / 72 (49716 Hz on an MSX) and the
   machine resamples. ym2413.mli lists where it departs from emu2413.

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

open Ym2413_tables

(* The phase accumulator has 19 bits; its top 10 index a 1024-entry wave. *)
let pg_bits = 10
let pg_width = 1 lsl pg_bits
let pg_mask = pg_width - 1
let dp_bits = 19
let dp_mask = (1 lsl dp_bits) - 1
let dp_base_bits = dp_bits - pg_bits

(* Envelope attenuation in 0.375 dB steps, 7 bits; 127 is silence. *)
let eg_mute = 127
let eg_max = eg_mute - 4
let eg_step = 0.375

(* The envelope rate of a slot sounding its key-on damp. *)
let damper_rate = 12

(* The rate a released carrier with sustain on (R#0x20 bit 5) falls at, and
   the one without sustain on a percussive (EG = 0) instrument. *)
let sustain_release_rate = 5
let percussive_release_rate = 7

let channels = 9
let slots = 18
let outputs = 14
let instruments = 19
let user_instrument = 0
let registers = 0x40

(* Wave entries flagged negative carry this bit; the half-sine wave's silent
   half holds [half_sine_silence]. *)
let negative = 0x8000
let half_sine_silence = 0xfff

(* Rhythm slots: bass drum uses channel 6's two slots, the others one slot of
   channel 7 or 8 each. *)
let slot_hh = 14
let slot_sd = 15
let slot_tom = 16
let slot_cym = 17

(* Output indices: 0-8 the melody channels, then the rhythm sounds. *)
let out_bd = 9
let out_hh = 10
let out_sd = 11
let out_tom = 12
let out_cym = 13

(* Registers. *)
let r_rhythm = 0x0e
let r_test = 0x0f
let r_fnum_low = 0x10
let r_fnum_high = 0x20
let r_instrument = 0x30

(* Noise LFSR taps, and how many steps it moves around each rhythm output. *)
let noise_taps = 0x800200
let noise_steps_bd = 14
let noise_steps_other = 2

(* LFO: vibrato reads bits 10-12 of its phase; one tremolo entry lasts 64
   samples. Both phases are kept modulo their period. *)
let pm_period = 8 lsl 10
let am_entry = 64
let am_period = Array.length am_table * am_entry
let eg_counter_mask = 0xffff

let wave_full = Array.make pg_width 0
let wave_half = Array.make pg_width 0

let () =
  let quarter = pg_width / 4 in
  Array.blit quarter_sin 0 wave_full 0 quarter;
  for x = 0 to quarter - 1 do
    wave_full.(quarter + x) <- wave_full.(quarter - x - 1)
  done;
  for x = 0 to (pg_width / 2) - 1 do
    wave_full.((pg_width / 2) + x) <- negative lor wave_full.(x)
  done;
  Array.blit wave_full 0 wave_half 0 (pg_width / 2);
  Array.fill wave_half (pg_width / 2) (pg_width / 2) half_sine_silence

(* Frequency multiplier, doubled so 0 means x1/2. *)
let ml_table = [| 1; 2; 4; 6; 8; 10; 12; 14; 16; 18; 20; 20; 24; 24; 30; 30 |]

(* Key scale level in units of 2 x dB for the top four F-number bits. *)
let kl_table =
  Array.map (fun db -> 2. *. db)
    [| 0.000; 9.000; 12.000; 13.875; 15.000; 16.125; 16.875; 17.625; 18.000; 18.750;
       19.125; 19.500; 19.875; 20.250; 20.625; 21.000 |]

(* Key scale attenuation by (block << 4 | top four F-number bits) and KL. *)
let tll_table =
  Array.init (8 * 16) (fun i ->
      let block = i lsr 4 and fnum = i land 15 in
      Array.init 4 (fun kl ->
          if kl = 0 then 0
          else
            let tmp = int_of_float (kl_table.(fnum) -. (6. *. float (7 - block))) in
            if tmp <= 0 then 0 else int_of_float (float (tmp asr (3 - kl)) /. eg_step)))

(* Key scale of rate by (block << 1 | F-number bit 8) and KR. *)
let rks_table = Array.init 16 (fun i -> [| i lsr 2; i |])

type patch = {
  mutable tl : int;
  mutable fb : int;
  mutable eg : bool;
  mutable ml : int;
  mutable ar : int;
  mutable dr : int;
  mutable sl : int;
  mutable rr : int;
  mutable kr : int;
  mutable kl : int;
  mutable am : bool;
  mutable pm : bool;
  mutable ws : int;
}

let blank_patch () =
  { tl = 0; fb = 0; eg = false; ml = 0; ar = 0; dr = 0; sl = 0; rr = 0; kr = 0; kl = 0;
    am = false; pm = false; ws = 0 }

(* Fill the modulator and carrier patches from an 8-byte instrument. *)
let load_instrument (m : patch) (c : patch) (d : int array) =
  m.am <- d.(0) land 0x80 <> 0;
  c.am <- d.(1) land 0x80 <> 0;
  m.pm <- d.(0) land 0x40 <> 0;
  c.pm <- d.(1) land 0x40 <> 0;
  m.eg <- d.(0) land 0x20 <> 0;
  c.eg <- d.(1) land 0x20 <> 0;
  m.kr <- (d.(0) lsr 4) land 1;
  c.kr <- (d.(1) lsr 4) land 1;
  m.ml <- d.(0) land 15;
  c.ml <- d.(1) land 15;
  m.kl <- (d.(2) lsr 6) land 3;
  c.kl <- (d.(3) lsr 6) land 3;
  m.tl <- d.(2) land 63;
  c.tl <- 0;
  m.fb <- d.(3) land 7;
  c.fb <- 0;
  m.ws <- (d.(3) lsr 3) land 1;
  c.ws <- (d.(3) lsr 4) land 1;
  m.ar <- (d.(4) lsr 4) land 15;
  c.ar <- (d.(5) lsr 4) land 15;
  m.dr <- d.(4) land 15;
  c.dr <- d.(5) land 15;
  m.sl <- (d.(6) lsr 4) land 15;
  c.sl <- (d.(7) lsr 4) land 15;
  m.rr <- d.(6) land 15;
  c.rr <- d.(7) land 15

(* A slot is the modulator or carrier of a two-operator channel, or, in
   rhythm mode, a single-slot drum (hi-hat, snare, tom, cymbal). *)
type role = Modulator | Carrier | Single

type eg_state = Attack | Decay | Sustain | Release | Damp

type slot = {
  mutable role : role;
  mutable patch : patch;
  mutable out0 : int;  (** latest output *)
  mutable out1 : int;  (** the one before, for feedback *)
  mutable wave : int array;
  mutable pg_phase : int;
  mutable pg_out : int;
  mutable pg_keep : bool;  (** the phase survives key-on *)
  mutable blk_fnum : int;
  mutable fnum : int;
  mutable blk : int;
  mutable eg_state : eg_state;
  mutable volume : int;
  mutable key_on : bool;
  mutable sustain : bool;
  mutable tll : int;
  mutable rks : int;
  mutable eg_rate_h : int;
  mutable eg_rate_l : int;
  mutable eg_shift : int;
  mutable eg_out : int;
  mutable updates : int;
}

(* What a register write leaves for the next sample to recompute. *)
let update_ws = 1
let update_tll = 2
let update_rks = 4
let update_eg = 8
let update_all = 255

type t = {
  mutable address : int;
  reg : int array;
  mutable test : int;
  mutable key_status : int;
  mutable rhythm : bool;
  mutable eg_counter : int;
  mutable pm_phase : int;
  mutable am_phase : int;
  mutable lfo_am : int;
  mutable noise : int;
  mutable short_noise : bool;
  instrument : int array;  (** per channel *)
  slot : slot array;
  patches : patch array;  (** modulator and carrier of each instrument *)
  out : int array;
}

let bit v n = (v lsr n) land 1

(* C's int16_t: what the chip's 16-bit outputs wrap to. *)
let int16 v = ((v + 0x8000) land 0xffff) - 0x8000

let modulator t ch = t.slot.(ch * 2)
let carrier t ch = t.slot.((ch * 2) + 1)

let parameter_rate s =
  if s.role = Modulator && not s.key_on then 0
  else
    match s.eg_state with
    | Attack -> s.patch.ar
    | Decay -> s.patch.dr
    | Sustain -> if s.patch.eg then 0 else s.patch.rr
    | Release ->
      if s.sustain then sustain_release_rate
      else if s.patch.eg then s.patch.rr
      else percussive_release_rate
    | Damp -> damper_rate

let request s flag = s.updates <- s.updates lor flag

let commit s =
  let p = s.patch in
  if s.updates land update_ws <> 0 then s.wave <- (if p.ws = 0 then wave_full else wave_half);
  if s.updates land update_tll <> 0 then begin
    let tl = if s.role = Modulator then p.tl else s.volume in
    s.tll <- tll_table.(s.blk_fnum lsr 5).(p.kl) + (tl lsl 1)
  end;
  if s.updates land update_rks <> 0 then s.rks <- rks_table.(s.blk_fnum lsr 8).(p.kr);
  if s.updates land (update_rks lor update_eg) <> 0 then begin
    match parameter_rate s with
    | 0 ->
      s.eg_shift <- 0;
      s.eg_rate_h <- 0;
      s.eg_rate_l <- 0
    | rate ->
      s.eg_rate_h <- min 15 (rate + (s.rks lsr 2));
      s.eg_rate_l <- s.rks land 3;
      s.eg_shift <-
        (match s.eg_state with
         | Attack -> if 0 < s.eg_rate_h && s.eg_rate_h < 12 then 13 - s.eg_rate_h else 0
         | Decay | Sustain | Release | Damp -> if s.eg_rate_h < 13 then 13 - s.eg_rate_h else 0)
  end;
  s.updates <- 0

let fresh_slot number patch =
  { role = (if number land 1 = 0 then Modulator else Carrier);
    patch; out0 = 0; out1 = 0; wave = wave_full; pg_phase = 0; pg_out = 0; pg_keep = false;
    blk_fnum = 0; fnum = 0; blk = 0; eg_state = Release; volume = 0; key_on = false;
    sustain = false; tll = 0; rks = 0; eg_rate_h = 0; eg_rate_l = 0; eg_shift = 0;
    eg_out = eg_mute; updates = 0 }

let slot_on s =
  s.key_on <- true;
  s.eg_state <- Damp;
  request s update_eg

let slot_off s =
  s.key_on <- false;
  if s.role <> Modulator then begin
    s.eg_state <- Release;
    request s update_eg
  end

let update_key_status t =
  let r14 = t.reg.(r_rhythm) in
  let status = ref 0 in
  for ch = 0 to channels - 1 do
    if t.reg.(r_fnum_high + ch) land 0x10 <> 0 then status := !status lor (3 lsl (ch * 2))
  done;
  if bit r14 5 = 1 then begin
    if r14 land 0x10 <> 0 then status := !status lor (3 lsl (6 * 2));
    if r14 land 0x01 <> 0 then status := !status lor (1 lsl slot_hh);
    if r14 land 0x08 <> 0 then status := !status lor (1 lsl slot_sd);
    if r14 land 0x04 <> 0 then status := !status lor (1 lsl slot_tom);
    if r14 land 0x02 <> 0 then status := !status lor (1 lsl slot_cym)
  end;
  let changed = t.key_status lxor !status in
  for i = 0 to slots - 1 do
    if bit changed i = 1 then
      if bit !status i = 1 then slot_on t.slot.(i) else slot_off t.slot.(i)
  done;
  t.key_status <- !status

let set_instrument t ch num =
  t.instrument.(ch) <- num;
  let m = modulator t ch and c = carrier t ch in
  m.patch <- t.patches.(num * 2);
  c.patch <- t.patches.((num * 2) + 1);
  request m update_all;
  request c update_all

let set_sustain t ch on =
  let c = carrier t ch and m = modulator t ch in
  c.sustain <- on;
  request c update_eg;
  if m.role <> Modulator then begin
    m.sustain <- on;
    request m update_eg
  end

let set_slot_volume s volume =
  s.volume <- volume;
  request s update_tll

let set_fnumber t ch fnum =
  List.iter
    (fun s ->
      s.fnum <- fnum;
      s.blk_fnum <- (s.blk_fnum land 0xe00) lor (fnum land 0x1ff);
      request s (update_eg lor update_rks lor update_tll))
    [ carrier t ch; modulator t ch ]

let set_block t ch blk =
  List.iter
    (fun s ->
      s.blk <- blk;
      s.blk_fnum <- ((blk land 7) lsl 9) lor (s.blk_fnum land 0x1ff);
      request s (update_eg lor update_rks lor update_tll))
    [ carrier t ch; modulator t ch ]

(* Rhythm instruments 16-18 sit on channels 6-8 while R#0x0E bit 5 is set. *)
let update_rhythm_mode t =
  let on = bit t.reg.(r_rhythm) 5 = 1 in
  if t.rhythm <> on then begin
    let hh = t.slot.(slot_hh) and sd = t.slot.(slot_sd) in
    let tom = t.slot.(slot_tom) and cym = t.slot.(slot_cym) in
    if on then begin
      hh.role <- Single;
      hh.pg_keep <- true;
      sd.role <- Single;
      tom.role <- Single;
      cym.role <- Single;
      cym.pg_keep <- true;
      set_instrument t 6 16;
      set_instrument t 7 17;
      set_instrument t 8 18;
      set_slot_volume hh (((t.reg.(r_instrument + 7) lsr 4) land 15) lsl 2);
      set_slot_volume tom (((t.reg.(r_instrument + 8) lsr 4) land 15) lsl 2)
    end
    else begin
      hh.role <- Modulator;
      hh.pg_keep <- false;
      sd.role <- Carrier;
      tom.role <- Modulator;
      cym.role <- Carrier;
      cym.pg_keep <- false;
      for ch = 6 to 8 do
        set_instrument t ch (t.reg.(r_instrument + ch) lsr 4)
      done
    end
  end;
  t.rhythm <- on

let update_ampm t =
  if t.test land 2 <> 0 then begin
    t.pm_phase <- 0;
    t.am_phase <- 0
  end
  else begin
    let fast = t.test land 8 <> 0 in
    t.pm_phase <- (t.pm_phase + if fast then 1024 else 1) mod pm_period;
    t.am_phase <- (t.am_phase + if fast then am_entry else 1) mod am_period
  end;
  t.lfo_am <- am_table.(t.am_phase / am_entry)

let update_noise t steps =
  for _ = 1 to steps do
    if t.noise land 1 <> 0 then t.noise <- t.noise lxor noise_taps;
    t.noise <- t.noise lsr 1
  done

let update_short_noise t =
  let hh = t.slot.(slot_hh).pg_out and cym = t.slot.(slot_cym).pg_out in
  t.short_noise <-
    bit hh 2 lxor bit hh 7 lor (bit hh 3 lxor bit cym 5) lor (bit cym 3 lxor bit cym 5) = 1

let calc_phase s pm_phase reset =
  let pm = if s.patch.pm then pm_table.((s.fnum lsr 6) land 7).((pm_phase lsr 10) land 7) else 0 in
  if reset then s.pg_phase <- 0;
  (* lsl and lsr associate to the right: the parentheses keep C's order. *)
  let step = (((((s.fnum land 0x1ff) * 2) + pm) * ml_table.(s.patch.ml)) lsl s.blk) lsr 2 in
  s.pg_phase <- (s.pg_phase + step) land dp_mask;
  s.pg_out <- s.pg_phase lsr dp_base_bits

let attack_step s counter =
  let table = eg_step_tables.(s.eg_rate_l) in
  match s.eg_rate_h with
  | 12 -> 4 - table.((counter land 0xc) lsr 1)
  | 13 -> 3 - table.((counter land 0xc) lsr 1)
  | 14 -> 2 - table.((counter land 0xc) lsr 1)
  | 0 | 15 -> 0
  | _ -> if table.((counter lsr s.eg_shift) land 7) <> 0 then 4 else 0

let decay_step s counter =
  let table = eg_step_tables.(s.eg_rate_l) in
  match s.eg_rate_h with
  | 0 -> 0
  | 13 -> table.(((counter land 0xc) lsr 1) lor (counter land 1))
  | 14 -> table.((counter land 0xc) lsr 1) + 1
  | 15 -> 2
  | _ -> table.((counter lsr s.eg_shift) land 7)

let start_envelope s =
  if min 15 (s.patch.ar + (s.rks lsr 2)) = 15 then begin
    s.eg_state <- Decay;
    s.eg_out <- 0
  end
  else s.eg_state <- Attack;
  request s update_eg

let calc_envelope s buddy counter test =
  let resting =
    (match s.eg_state with Sustain | Release -> true | Attack | Decay | Damp -> false)
    && (s.eg_out >= eg_mute || s.eg_rate_h = 0)
  in
  if test || not resting then begin
    let mask = (1 lsl s.eg_shift) - 1 in
    (match s.eg_state with
     | Attack ->
       if 0 < s.eg_out && 0 < s.eg_rate_h && counter land mask land lnot 3 = 0 then begin
         let step = attack_step s counter in
         if step > 0 then s.eg_out <- max 0 (s.eg_out - (s.eg_out lsr step) - 1)
       end
     | Decay | Sustain | Release | Damp ->
       if s.eg_rate_h > 0 && counter land mask = 0 then
         s.eg_out <- min eg_mute (s.eg_out + decay_step s counter));
    (match s.eg_state with
     | Damp ->
       (* The damp ends where the attenuation reaches EG_MAX, on an envelope
          tick, and the key-on phase reset happens there. *)
       if s.eg_out >= eg_max && counter land mask = 0 then begin
         start_envelope s;
         if s.role <> Modulator then begin
           if not s.pg_keep then s.pg_phase <- 0;
           match buddy with
           | Some b when not b.pg_keep -> b.pg_phase <- 0
           | Some _ | None -> ()
         end
       end
     | Attack ->
       if s.eg_out = 0 then begin
         s.eg_state <- Decay;
         request s update_eg
       end
     | Decay ->
       if s.eg_out lsr 3 = s.patch.sl then begin
         s.eg_state <- Sustain;
         request s update_eg
       end
     | Sustain | Release -> ());
    if test then s.eg_out <- 0
  end

let update_slots t =
  t.eg_counter <- (t.eg_counter + 1) land eg_counter_mask;
  for i = 0 to slots - 1 do
    let s = t.slot.(i) in
    let buddy =
      match s.role with
      | Modulator -> Some t.slot.(i + 1)
      | Carrier -> Some t.slot.(i - 1)
      | Single -> None
    in
    if s.updates <> 0 then commit s;
    calc_envelope s buddy t.eg_counter (t.test land 1 <> 0);
    calc_phase s t.pm_phase (t.test land 4 <> 0)
  done

(* Log-sin plus attenuation back to a level: -4096..4094. *)
let exp_lookup i =
  let level = (exp_table.((i land 0xff) lxor 0xff) + 1024) asr ((i land 0x7f00) lsr 8) in
  int16 ((if i land negative <> 0 then lnot level else level) lsl 1)

let to_linear h s am =
  if s.eg_out > eg_max then 0
  else
    let att = min eg_mute (s.eg_out + s.tll + am) lsl 4 in
    exp_lookup ((h + att) land 0xffff)

let tremolo t s = if s.patch.am then t.lfo_am else 0

let calc_carrier t ch fm =
  let s = carrier t ch in
  s.out1 <- s.out0;
  s.out0 <- to_linear s.wave.((s.pg_out + (2 * (fm asr 1))) land pg_mask) s (tremolo t s);
  s.out0

let calc_modulator t ch =
  let s = modulator t ch in
  let fm = if s.patch.fb > 0 then int16 ((s.out1 + s.out0) asr (9 - s.patch.fb)) else 0 in
  s.out1 <- s.out0;
  s.out0 <- to_linear s.wave.((s.pg_out + fm) land pg_mask) s (tremolo t s);
  s.out0

let calc_tom t =
  let s = modulator t 8 in
  to_linear s.wave.(s.pg_out) s 0

let noise_bit t = t.noise land 1 <> 0

let calc_snare t =
  let s = carrier t 7 in
  let phase =
    match bit s.pg_out (pg_bits - 2) = 1, noise_bit t with
    | true, true -> 0x300
    | true, false -> 0x200
    | false, true -> 0x0
    | false, false -> 0x100
  in
  to_linear s.wave.(phase) s 0

let calc_cymbal t =
  let s = carrier t 8 in
  to_linear s.wave.(if t.short_noise then 0x300 else 0x100) s 0

let calc_hihat t =
  let s = modulator t 7 in
  let phase =
    match t.short_noise, noise_bit t with
    | true, true -> 0x2d0
    | true, false -> 0x234
    | false, true -> 0x34
    | false, false -> 0xd0
  in
  to_linear s.wave.(phase) s 0

(* A melody channel's carrier is halved and inverted on its way out; the
   rhythm outputs are not. *)
let melody v = -v asr 1

let channel t ch = melody (calc_carrier t ch (calc_modulator t ch))

let step t =
  update_ampm t;
  update_short_noise t;
  update_slots t;
  for ch = 0 to 5 do
    t.out.(ch) <- channel t ch
  done;
  (* Channels 6-8 sound either as melody or as drums. The outputs of the
     other mode read 0: emu2413 keeps their last sample, a constant offset in
     the mix, where openMSX's NukeYKT core outputs 0. *)
  if t.rhythm then begin
    t.out.(6) <- 0;
    t.out.(out_bd) <- calc_carrier t 6 (calc_modulator t 6)
  end
  else begin
    t.out.(6) <- channel t 6;
    t.out.(out_bd) <- 0
  end;
  update_noise t noise_steps_bd;
  if t.rhythm then begin
    t.out.(7) <- 0;
    t.out.(out_hh) <- calc_hihat t;
    t.out.(out_sd) <- calc_snare t
  end
  else begin
    t.out.(7) <- channel t 7;
    t.out.(out_hh) <- 0;
    t.out.(out_sd) <- 0
  end;
  update_noise t noise_steps_other;
  if t.rhythm then begin
    t.out.(8) <- 0;
    t.out.(out_tom) <- calc_tom t;
    t.out.(out_cym) <- calc_cymbal t
  end
  else begin
    t.out.(8) <- channel t 8;
    t.out.(out_tom) <- 0;
    t.out.(out_cym) <- 0
  end;
  update_noise t noise_steps_other

let output t i = t.out.(i)

let mix t = int16 (Array.fold_left ( + ) 0 t.out)

(* Registers 0-7 hold the user instrument; a change reaches every channel
   playing it at the next sample. *)
let update_user t slot_of flag =
  for ch = 0 to channels - 1 do
    if t.instrument.(ch) = user_instrument then request (slot_of t ch) flag
  done

let write_register t reg data =
  if reg >= 0 && reg < registers then begin
    let reg =
      if (0x19 <= reg && reg <= 0x1f) || (0x29 <= reg && reg <= 0x2f) || (0x39 <= reg && reg <= 0x3f)
      then reg - 9
      else reg
    in
    let data = data land 0xff in
    t.reg.(reg) <- data;
    let um = t.patches.(0) and uc = t.patches.(1) in
    match reg with
    | 0x00 | 0x01 ->
      let p, slot_of = if reg = 0 then um, modulator else uc, carrier in
      p.am <- data land 0x80 <> 0;
      p.pm <- data land 0x40 <> 0;
      p.eg <- data land 0x20 <> 0;
      p.kr <- (data lsr 4) land 1;
      p.ml <- data land 15;
      update_user t slot_of (update_rks lor update_eg)
    | 0x02 ->
      um.kl <- (data lsr 6) land 3;
      um.tl <- data land 63;
      update_user t modulator update_tll
    | 0x03 ->
      uc.kl <- (data lsr 6) land 3;
      uc.ws <- (data lsr 4) land 1;
      um.ws <- (data lsr 3) land 1;
      um.fb <- data land 7;
      update_user t modulator update_ws;
      update_user t carrier (update_ws lor update_tll)
    | 0x04 | 0x05 ->
      let p, slot_of = if reg = 4 then um, modulator else uc, carrier in
      p.ar <- (data lsr 4) land 15;
      p.dr <- data land 15;
      update_user t slot_of update_eg
    | 0x06 | 0x07 ->
      let p, slot_of = if reg = 6 then um, modulator else uc, carrier in
      p.sl <- (data lsr 4) land 15;
      p.rr <- data land 15;
      update_user t slot_of update_eg
    | r when r = r_rhythm ->
      update_rhythm_mode t;
      update_key_status t
    | r when r = r_test -> t.test <- data
    | r when r >= r_fnum_low && r < r_fnum_low + channels ->
      let ch = r - r_fnum_low in
      set_fnumber t ch (data + ((t.reg.(r_fnum_high + ch) land 1) lsl 8))
    | r when r >= r_fnum_high && r < r_fnum_high + channels ->
      let ch = r - r_fnum_high in
      set_fnumber t ch (((data land 1) lsl 8) + t.reg.(r_fnum_low + ch));
      set_block t ch ((data lsr 1) land 7);
      set_sustain t ch (bit data 5 = 1);
      update_key_status t
    | r when r >= r_instrument && r < r_instrument + channels ->
      let ch = r - r_instrument in
      if t.reg.(r_rhythm) land 0x20 <> 0 && ch >= 6 then begin
        (* In rhythm mode the high nibble of 0x37/0x38 is the hi-hat/tom
           volume, not an instrument. *)
        if ch = 7 then set_slot_volume (modulator t 7) (((data lsr 4) land 15) lsl 2)
        else if ch = 8 then set_slot_volume (modulator t 8) (((data lsr 4) land 15) lsl 2)
      end
      else set_instrument t ch ((data lsr 4) land 15);
      set_slot_volume (carrier t ch) ((data land 15) lsl 2)
    | _ -> ()
  end

let write_address t v = t.address <- v land 0xff
let write_data t v = write_register t t.address v

let reset t =
  t.address <- 0;
  Array.fill t.reg 0 registers 0;
  t.pm_phase <- 0;
  t.am_phase <- 0;
  t.noise <- 1;
  t.short_noise <- false;
  t.test <- 0;
  t.lfo_am <- 0;
  t.rhythm <- false;
  t.key_status <- 0;
  t.eg_counter <- 0;
  let null = blank_patch () in
  Array.iteri (fun i _ -> t.slot.(i) <- fresh_slot i null) t.slot;
  for ch = 0 to channels - 1 do
    set_instrument t ch 0
  done;
  for r = 0 to registers - 1 do
    write_register t r 0
  done;
  Array.fill t.out 0 outputs 0

let create () =
  let patches = Array.init (instruments * 2) (fun _ -> blank_patch ()) in
  Array.iteri
    (fun i d -> load_instrument patches.(i * 2) patches.((i * 2) + 1) d)
    default_instruments;
  let null = blank_patch () in
  let t =
    { address = 0; reg = Array.make registers 0; test = 0; key_status = 0; rhythm = false;
      eg_counter = 0; pm_phase = 0; am_phase = 0; lfo_am = 0; noise = 1; short_noise = false;
      instrument = Array.make channels 0; slot = Array.init slots (fun i -> fresh_slot i null);
      patches; out = Array.make outputs 0 }
  in
  reset t;
  t

let register t r = t.reg.(r)
