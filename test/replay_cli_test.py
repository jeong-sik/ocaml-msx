"""Exercise the compiled replay CLI with synthetic firmware and isolated files."""

import hashlib
import json
from pathlib import Path
import re
import struct
import subprocess
import sys
import tempfile


def firmware(directory):
    directory.mkdir()
    # Select RAM in page 3 and keyboard row 8. Increment a RAM counter and
    # continuously sample SPACE, so restored execution has a guest-visible effect.
    setup = bytes.fromhex("31 00 f0 3e c0 d3 a8 3e 08 d3 aa 21 00 c0")
    loop = bytes.fromhex("34 db a9 32 01 c0 c3") + struct.pack("<H", len(setup))
    (directory / "cbios_main_msx2.rom").write_bytes((setup + loop).ljust(32768, b"\0"))
    for name in ("cbios_logo_msx2.rom", "cbios_sub.rom"):
        (directory / name).write_bytes(bytes(16384))


def ledger(path, edges):
    path.write_text("".join(json.dumps(
        {"frame": frame, "who": "cli-test", "key": key, "edge": edge},
        separators=(",", ":"),
    ) + "\n" for frame, key, edge in edges), encoding="utf-8")
    return str(path)


def ram(path):
    state = path.read_bytes()
    magic = b"OCAML-MSX\0\1"
    assert state.startswith(magic), "saved state header"
    digest_end = len(magic) + 16
    payload = state[digest_end:]
    assert state[len(magic):digest_end] == hashlib.md5(payload).digest(), "saved state checksum"
    size = struct.unpack_from(">q", payload)[0]
    assert size == 512 * 1024, "synthetic machine RAM size"
    return payload[8:8 + size]


def main():
    binary = str(Path(sys.argv[1]).resolve())
    with tempfile.TemporaryDirectory(prefix="msx-replay-cli-") as directory:
        root = Path(directory)
        roms = root / "roms"
        firmware(roms)
        empty = ledger(root / "empty.jsonl", [])
        run_number = 0

        def run(arguments, *, ok=True, frame=None):
            nonlocal run_number
            run_number += 1
            result = subprocess.run(
                [binary, "--out-dir", str(root / f"frames-{run_number}"),
                 "--every", "1000", *map(str, arguments)],
                capture_output=True, text=True, timeout=20,
            )
            assert (result.returncode == 0) == ok, (
                f"unexpected exit {result.returncode}: {result.stdout}\n{result.stderr}"
            )
            if frame is not None:
                match = re.search(r"final frame=(\d+)\b", result.stdout)
                assert match and int(match[1]) == frame, result.stdout
            return result

        edge = ledger(root / "edge.jsonl", [(45, "space", "down")])
        boundary = root / "boundary.state"
        run(["--roms", roms, "--ledger", edge, "--tail", 0,
             "--save-state", boundary], frame=45)
        assert ram(boundary)[0xc001] & 1 == 1, "CPU has not yet sampled final edge"

        unchanged = root / "unchanged.state"
        run(["--restore-state", boundary, "--ledger", empty, "--tail", 0,
             "--save-state", unchanged], frame=45)
        assert boundary.read_bytes() == unchanged.read_bytes(), "restore must not reboot or advance"

        sampled = root / "sampled.state"
        run(["--restore-state", boundary, "--ledger", empty, "--tail", 1,
             "--save-state", sampled], frame=46)
        assert ram(sampled)[0xc001] & 1 == 0, "tail-zero final key edge survives save and restore"

        edges = [(45, "space", "down"), (48, "space", "up"),
                 (52, "space", "down"), (56, "space", "up")]
        full = ledger(root / "full.jsonl", edges)
        first = ledger(root / "first.jsonl", edges[:3])
        rest = ledger(root / "rest.jsonl", edges[3:])
        whole = root / "whole.state"
        checkpoint = root / "checkpoint.state"
        resumed = root / "resumed.state"
        run(["--roms", roms, "--ledger", full, "--tail", 3,
             "--save-state", whole], frame=59)
        run(["--roms", roms, "--ledger", first, "--tail", 0,
             "--save-state", checkpoint], frame=52)
        run(["--restore-state", checkpoint, "--ledger", rest, "--tail", 3,
             "--save-state", resumed], frame=59)
        assert whole.read_bytes() == resumed.read_bytes(), "split replay must equal uninterrupted state"

        # A pre-existing destination is never replaced by an invalid request.
        destination = root / "preserved.state"
        sentinel = b"existing destination must survive"
        destination.write_bytes(sentinel)

        def reject(arguments, message=None):
            result = run([*arguments, "--save-state", destination], ok=False)
            assert destination.read_bytes() == sentinel, "error overwrote existing state"
            if message:
                assert message in result.stderr, result.stderr

        valid = ["--roms", roms, "--ledger", empty, "--tail", 0]
        reject([*valid, "--every", 0], "--every must be positive")
        reject([*valid, "--every", -1], "--every must be positive")
        reject([*valid, "--tail", -1], "--tail nonnegative")
        reject([*valid, "--cart", "absent.rom", "--disk", "absent.dsk"], "choose cartridge")
        for option, value in [("--roms", roms), ("--cart", "absent.rom"), ("--disk", "absent.dsk")]:
            reject(["--restore-state", boundary, option, value,
                    "--ledger", empty, "--tail", 0], "choose cartridge")
        # CI hosts run 64-bit OCaml, whose tagged integers have 62 value bits.
        ocaml_max_int = (1 << 62) - 1
        reject([*valid, "--tail", ocaml_max_int], "final frame exceeds")
        late = ledger(root / "late.jsonl", [(ocaml_max_int, "space", "down")])
        reject(["--roms", roms, "--ledger", late, "--tail", 1], "final frame exceeds")
        old = ledger(root / "old.jsonl", [(44, "space", "down")])
        reject(["--roms", roms, "--ledger", old, "--tail", 0], "predates initial machine state")
        unknown = ledger(root / "unknown.jsonl", [(45, "not-a-key", "down")])
        reject(["--roms", roms, "--ledger", unknown, "--tail", 0], "unknown ledger key")
        print("replay CLI: final edges, restored clock, split continuation and safe failures passed")


if __name__ == "__main__":
    main()
