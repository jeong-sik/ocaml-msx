"""Exercise the production identity rule across Git-only changes (CI only).

The tiny projects copy the real rule and generator, not their Git algorithm.
Repeated builds intentionally reuse _build and a shared cache, so a stale
generated value cannot pass merely because a fresh generator was invoked.
"""

import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import tempfile


SOURCE = Path(__file__).resolve().parents[1]
TARGET = Path("lib/identity/msx_core_identity.ml")


def run(args, *, cwd, env):
    result = subprocess.run(
        list(map(str, args)), cwd=cwd, env=env,
        capture_output=True, text=True,
    )
    assert result.returncode == 0, (
        f"{args}: exit {result.returncode}\n{result.stdout}\n{result.stderr}"
    )
    return result.stdout.strip()


def project(root):
    (root / "lib").mkdir(parents=True)
    shutil.copytree(SOURCE / "lib/identity", root / "lib/identity")
    (root / "dune-project").write_text(
        "(lang dune 3.0)\n(name ocaml-msx)\n(package (name ocaml-msx))\n"
    )
    (root / "ocaml-msx.opam").write_text('opam-version: "2.0"\n')
    (root / "lib/dune").write_text("(library (name identity_probe))\n")
    (root / "lib/identity_probe.ml").write_text("let value = 1\n")
    (root / ".gitignore").write_text("_build/\n")


def main():
    # Fixtures must not inherit the developer/runner's Git index, signing,
    # hooks, identity, or Dune source-root hints.
    env = {key: value for key, value in os.environ.items()
           if not key.startswith(("GIT_", "DUNE_"))}
    env.update({
        "GIT_CONFIG_NOSYSTEM": "1", "GIT_CONFIG_GLOBAL": os.devnull,
        "GIT_AUTHOR_NAME": "Core identity test",
        "GIT_AUTHOR_EMAIL": "identity@example.invalid",
        "GIT_COMMITTER_NAME": "Core identity test",
        "GIT_COMMITTER_EMAIL": "identity@example.invalid",
        "DUNE_CACHE": "enabled",
    })
    git = shutil.which("git")
    dune = shutil.which("dune")
    assert git and dune, "Git and Dune are required for the lifecycle regression"

    with tempfile.TemporaryDirectory(prefix="msx-core-identity-") as directory:
        root = Path(directory).resolve()
        env["DUNE_CACHE_ROOT"] = str(root / "cache")

        def git_run(repo, *args):
            return run([git, "-C", repo, "-c", "core.hooksPath=" + os.devnull,
                        *args], cwd=root, env=env)

        def commit(repo, *, empty=False):
            if not empty:
                git_run(repo, "add", ".")
            git_run(repo, "commit", "--allow-empty", "-m", "identity fixture")
            return git_run(repo, "rev-parse", "HEAD")

        def init(repo):
            git_run(repo, "init")
            return commit(repo)

        def build(repo, expected, label, *, subproject=Path("."), build_env=None):
            target = subproject / TARGET
            run([dune, "build", "--root", repo, target],
                cwd=repo, env=build_env or env)
            generated = (repo / "_build/default" / target).read_text()
            expected_value = "None" if expected is None else f'Some "{expected}"'
            assert f"let source_commit = {expected_value}\n" in generated, (
                f"{label}: expected {expected_value}\n{generated}"
            )
            digest = re.search(r'let source_digest = "([0-9a-f]{32})"', generated)
            assert digest, f"{label}: missing source digest"
            return digest.group(1)

        core = root / "core"
        project(core)
        first = init(core)
        original_digest = build(core, first, "clean checkout")
        (core / "lib/identity_probe.ml").write_text("let value = 2\n")
        dirty_digest = build(core, None, "dirty checkout")
        assert original_digest != dirty_digest, "source change must move the digest"
        second = commit(core)
        assert build(core, second, "dirty to committed") == dirty_digest
        third = commit(core, empty=True)
        assert third != second, "empty commit must advance HEAD"
        assert build(core, third, "empty commit with unchanged sources") == dirty_digest

        # Dotfiles are not part of source_tree. They still affect cleanliness.
        (core / ".untracked-input").write_text("dirty\n")
        build(core, None, "untracked dotfile")
        (core / ".untracked-input").unlink()
        build(core, third, "removed untracked dotfile")

        # A failed Git process may write plausible output but has no identity.
        shim = root / "failing-bin"
        shim.mkdir()
        fake_git = shim / "git"
        fake_git.write_text(
            "#!/bin/sh\nprintf '%s\\n' " + shlex.quote(str(core)) + "\nexit 1\n"
        )
        fake_git.chmod(0o755)
        failed_git_env = {**env, "PATH": str(shim) + os.pathsep + env["PATH"]}
        build(core, None, "Git failure", build_env=failed_git_env)
        build(core, third, "Git recovery")

        # Rebuilding from the shared cache must not import another checkout's
        # prior Git observation, even when all digested source bytes match.
        shutil.rmtree(core / "_build")
        build(core, third, "fresh build with shared cache")

        worktree = root / "worktree"
        git_run(core, "worktree", "add", "--detach", worktree, third)
        assert (worktree / ".git").is_file(), "exercise a Git worktree, not a clone"
        worktree_head = commit(worktree, empty=True)
        build(worktree, worktree_head, "worktree's own HEAD")
        build(core, third, "original checkout keeps its own HEAD")

        archive = root / "archive"
        project(archive)
        build(archive, None, "archive without Git metadata")

        parent = root / "parent"
        vendor = Path("vendor/ocaml-msx")
        project(parent / vendor)
        (parent / "dune-project").write_text("(lang dune 3.0)\n(name consumer)\n")
        (parent / "dune").write_text("(vendored_dirs vendor)\n")
        (parent / ".gitignore").write_text("_build/\n")
        parent_head = init(parent)
        build(parent, None, "vendored core in a clean parent", subproject=vendor)
        assert git_run(parent, "status", "--porcelain") == "", "parent stays clean"
        # Even when selected as the workspace root, this archive cannot claim
        # the parent checkout's SHA through Git's upward repository discovery.
        build(parent / vendor, None, "archive workspace inside a clean parent")

        # A nested *independent* core checkout does have an identity. Its
        # workspace still belongs to the consumer, whose SHA must not leak in.
        nested_head = init(parent / vendor)
        assert nested_head != parent_head, "fixture repositories have distinct heads"
        build(parent, nested_head, "nested core checkout", subproject=vendor)

    print("core identity Git lifecycle: all passed")


if __name__ == "__main__":
    main()
