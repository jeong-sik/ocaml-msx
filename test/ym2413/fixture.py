"""Make the YM2413 reference fixture.

usage: python3 -I test/ym2413/fixture.py OPENMSX MACHINE_XML WORK_DIR

OPENMSX is the openMSX 21.0 binary and MACHINE_XML its C-BIOS_MSX2+.xml.
The script writes events.txt (the stimulus) and nukeykt.txt (what openMSX
plays for it) next to itself; WORK_DIR keeps the recordings.

The stimulus writes YM2413 registers at chip-sample times (49716 Hz). openMSX
runs it on a copy of C-BIOS_MSX2+ whose MSX-MUSIC uses the NukeYKT core, the
one openMSX's FM-PAC uses, and records the 14 channels from 0.02 s. Muted,
unthrottled, no renderer. For each channel and each 2000-sample window,
nukeykt.txt keeps the RMS and how often the signal rises through its mean:
the loudness and the pitch, which do not depend on the start phase of a
note or on the sign of the output.

Writes are 4 samples apart. The address goes a quarter sample after the tick
and the data half a sample later: NukeYKT ignores a data write that lands at
the same time as its address."""
import math
import os
import re
import subprocess
import sys
import wave

RATE = 49716  # 3579545 / 72
GAP = 4
WINDOW = 2000
RECORD_FROM = 0.02  # s
DEVICE = 'C-BIOS MSX-MUSIC'
# openMSX channel n (1-14) is our output OUT_OF_CHANNEL[n - 1]: the nine
# melody channels, then hi-hat, tom-tom, bass drum, snare drum, top cymbal.
OUT_OF_CHANNEL = [0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 9, 11, 13]


def rom_patches():
    """emu2413's instrument ROM as lib/ym2413_tables.ml holds it."""
    here = os.path.dirname(os.path.abspath(__file__))
    src = open(os.path.join(here, '..', '..', 'lib', 'ym2413_tables.ml')).read()
    body = src[src.index('let default_instruments'):]
    rows = [[int(b, 16) for b in re.findall(r'0x([0-9a-f]{2})', m)]
            for m in re.findall(r'\[\|([^|]*)\|\]', body)]
    if len(rows) != 19 or any(len(r) != 8 for r in rows):
        sys.exit('lib/ym2413_tables.ml: expected 19 instruments of 8 bytes')
    return rows


def stimulus():
    ev = []
    t = 2000
    rom = rom_patches()

    def w(reg, val):
        s = max(t, ev[-1][0] + GAP) if ev else t
        ev.append((s, reg, val))

    def wait(n):
        nonlocal t
        t += n

    # Melody plays the user instrument loaded with the ROM bytes: openMSX's
    # NukeYKT has its own instrument ROM, and this compares the chip, not
    # the tables.
    def instrument(n):
        for r, v in enumerate(rom[n]):
            w(r, v)

    hold, rest = 12000, 6000
    # each ROM instrument on its own channel, then key off and let it fade
    fnums = [0x0ad, 0x0b7, 0x0c2, 0x0cd, 0x0d9, 0x0e6, 0x102, 0x122, 0x158]
    for inst in range(1, 16):
        ch = (inst - 1) % 9
        fn = fnums[ch]
        blk = 3 + (inst % 3)
        instrument(inst)
        w(0x10 + ch, fn & 0xff)
        w(0x30 + ch, inst % 4)
        w(0x20 + ch, 0x10 | (blk << 1) | (fn >> 8))
        wait(hold)
        w(0x20 + ch, (blk << 1) | (fn >> 8))
        wait(rest + 12000)
    # sustain on at key off: a slower release
    instrument(3)
    w(0x10, 0x80)
    w(0x30, 0x00)
    w(0x20, 0x30 | (4 << 1))
    wait(hold)
    w(0x20, 0x20 | (4 << 1))
    wait(rest * 2)
    # user instrument with feedback, half sine, vibrato and tremolo,
    # changed while it sounds
    for r, v in enumerate([0xe1, 0xc1, 0x11, 0x1d, 0xd2, 0xa4, 0x24, 0x35]):
        w(r, v)
    w(0x11, 0x40)
    w(0x31, 0x02)
    w(0x21, 0x10 | (5 << 1) | 1)
    wait(hold * 2)
    w(0x02, 0x08)
    w(0x03, 0x05)
    wait(hold)
    w(0x21, (5 << 1) | 1)
    wait(rest)
    # blocks 0-7 on one channel
    for blk in range(8):
        instrument(5)
        w(0x12, 0x40)
        w(0x32, 0x00)
        w(0x22, 0x10 | (blk << 1) | 1)
        wait(4000)
        w(0x22, (blk << 1) | 1)
        wait(2000)
    # two channels at once, one instrument
    instrument(7)
    w(0x13, 0x60)
    w(0x33, 0x00)
    w(0x14, 0x90)
    w(0x34, 0x01)
    w(0x23, 0x10 | (4 << 1))
    w(0x24, 0x10 | (4 << 1))
    wait(hold)
    w(0x23, 4 << 1)
    w(0x24, 4 << 1)
    wait(rest)
    # one user-instrument feature at a time on channel 0
    base = [0x01, 0x01, 0x10, 0x00, 0xf0, 0xf0, 0x00, 0x05]
    for patch in [base,
                  base[:3] + [0x07] + base[4:],                   # feedback 7
                  base[:3] + [0x08] + base[4:],                   # modulator half sine
                  base[:3] + [0x10] + base[4:],                   # carrier half sine
                  [0x41, 0x41] + base[2:],                        # vibrato
                  [0x81, 0x81] + base[2:],                        # tremolo
                  [0x01, 0x01, 0x10, 0x00, 0x8f, 0x8f, 0x00, 0x05],  # decay to SL 0
                  [0x21, 0x21, 0x10, 0x00, 0xf4, 0xf4, 0x44, 0x44]]:  # sustained, SL 4
        for r, v in enumerate(patch):
            w(r, v)
        w(0x10, 0x40)
        w(0x30, 0x00)
        w(0x20, 0x10 | (4 << 1) | 1)
        wait(hold * 2)
        w(0x20, (4 << 1) | 1)
        wait(rest)
    # rhythm mode: each drum, all five, two pairs
    w(0x16, 0x20)
    w(0x17, 0x50)
    w(0x18, 0xc0)
    w(0x26, 0x05)
    w(0x27, 0x05)
    w(0x28, 0x01)
    w(0x36, 0x02)
    w(0x37, 0x31)
    w(0x38, 0x13)
    w(0x0e, 0x20)
    wait(500)
    for bits in [0x10, 0x08, 0x04, 0x02, 0x01, 0x1f, 0x11, 0x0a]:
        w(0x0e, 0x20 | bits)
        wait(6000)
        w(0x0e, 0x20)
        wait(3000)
    # rhythm off again, melody on channel 6
    instrument(4)
    w(0x0e, 0x00)
    w(0x36, 0x00)
    w(0x26, 0x10 | (4 << 1))
    wait(hold)
    w(0x26, 4 << 1)
    wait(rest)
    return ev, t + 2000


def record(openmsx, machine_xml, work, ev, end):
    home = os.path.join(work, 'home')
    machines = os.path.join(home, 'share', 'machines')
    os.makedirs(machines, exist_ok=True)
    xml = open(machine_xml).read()
    tag = f'<MSX-MUSIC id="{DEVICE}">'
    if xml.count(tag) != 1:
        sys.exit(f'{machine_xml}: expected one {tag}')
    xml = xml.replace(tag, tag + '\n<ym2413-core>NukeYKT</ym2413-core>')
    # the ROMs sit next to the original machine file
    rom_dir = os.path.dirname(os.path.abspath(machine_xml))
    xml = re.sub(r'<filename>([^/<]+)</filename>',
                 lambda m: f'<filename>{rom_dir}/{m.group(1)}</filename>', xml)
    open(os.path.join(machines, 'C-BIOS_MSX2+_nuke.xml'), 'w').write(xml)
    rec = os.path.join(work, 'rec')
    os.makedirs(rec, exist_ok=True)
    lines = ['set save_settings_on_exit false', 'set mute on', 'set master_volume 0',
             'set throttle off', 'set renderer none',
             'after time %.9f {' % RECORD_FROM]
    lines += [f'  set "::{DEVICE}_ch{i}_record" "{rec}/ch{i}.wav"' for i in range(1, 15)]
    lines.append('}')
    for s, r, v in ev:
        lines.append('after time %.9f {debug write ioports 0x7c %d}' % ((s + 0.25) / RATE, r))
        lines.append('after time %.9f {debug write ioports 0x7d %d}' % ((s + 0.75) / RATE, v))
    lines.append('after time %.9f {' % ((end + 0.5) / RATE + 0.01))
    lines += [f'  set "::{DEVICE}_ch{i}_record" ""' for i in range(1, 15)]
    lines += ['  exit', '}']
    script = os.path.join(work, 'run.tcl')
    open(script, 'w').write('\n'.join(lines) + '\n')
    # silent from the start, before the script runs
    settings = os.path.join(work, 'settings.xml')
    open(settings, 'w').write(
        "<!DOCTYPE settings SYSTEM 'settings.dtd'>\n<settings><settings>\n"
        '<setting id="mute">true</setting>\n<setting id="master_volume">0</setting>\n'
        '<setting id="throttle">off</setting>\n</settings></settings>\n')
    env = dict(os.environ, OPENMSX_HOME=home)
    r = subprocess.run([openmsx, '-setting', settings, '-machine', 'C-BIOS_MSX2+_nuke',
                        '-script', script],
                       env=env, capture_output=True, text=True, timeout=900)
    open(os.path.join(work, 'openmsx.log'), 'w').write(r.stdout + r.stderr)
    if r.returncode != 0:
        sys.exit(f'openMSX exited with {r.returncode}; see {work}/openmsx.log')
    return rec


def channel(path):
    with wave.open(path) as f:
        if f.getsampwidth() != 2 or f.getnchannels() != 1 or f.getframerate() != RATE:
            sys.exit(f'{path}: expected 16-bit mono at {RATE} Hz')
        data = f.readframes(f.getnframes())
    return [int.from_bytes(data[i:i + 2], 'little', signed=True) for i in range(0, len(data), 2)]


def measure(x, windows):
    rms, zc = [], []
    for k in range(windows):
        seg = x[k * WINDOW:(k + 1) * WINDOW]
        rms.append(round(math.sqrt(sum(v * v for v in seg) / WINDOW)))
        # around the window's mean: a half-wave carrier never goes below 0
        mean = sum(seg) / WINDOW
        zc.append(sum(1 for a, b in zip(seg, seg[1:]) if a <= mean < b))
    return rms, zc


def main():
    openmsx, machine_xml, work = sys.argv[1:4]
    here = os.path.dirname(os.path.abspath(__file__))
    ev, end = stimulus()
    with open(os.path.join(here, 'events.txt'), 'w') as f:
        for s, r, v in ev:
            f.write(f'{s} {r} {v}\n')
        f.write(f'end {end}\n')
    rec = record(openmsx, machine_xml, work, ev, end)
    chans = [channel(os.path.join(rec, f'ch{i}.wav')) for i in range(1, 15)]
    first = round(RECORD_FROM * RATE)
    windows = min(min(len(c) for c in chans), end - first) // WINDOW
    rows = {OUT_OF_CHANNEL[n]: measure(x, windows) for n, x in enumerate(chans)}
    with open(os.path.join(here, 'nukeykt.txt'), 'w') as f:
        f.write('# openMSX 21.0, YM2413 core NukeYKT, for events.txt; made by fixture.py\n')
        f.write(f'first {first} window {WINDOW} windows {windows}\n')
        for out in range(14):
            rms, zc = rows[out]
            f.write(f'rms {out} ' + ' '.join(map(str, rms)) + '\n')
            f.write(f'zc {out} ' + ' '.join(map(str, zc)) + '\n')
    print(len(ev), 'writes,', end, 'samples,', windows, 'windows')


main()
