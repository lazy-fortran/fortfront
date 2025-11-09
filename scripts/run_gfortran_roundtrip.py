#!/usr/bin/env python3
"""Run fortfront round-trip tests over GCC's gfortran DejaGNU suite."""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
import time
from concurrent.futures import ThreadPoolExecutor
from functools import partial
from pathlib import Path
from typing import Dict, List, Sequence, Set

FORTRAN_SUFFIXES: Sequence[str] = (
    ".f",
    ".f90",
    ".f95",
    ".f03",
    ".f08",
    ".for",
    ".fpp",
)

DEFAULT_OUTPUT = Path("logs") / "gfortran_dejagnu_roundtrip_results.jsonl"
DEFAULT_GCC_ROOT = Path("..") / "gcc-dev" / "gcc"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Standard-Fortran round-trip harness for the GCC gfortran "
            "DejaGNU test corpus."
        )
    )
    parser.add_argument(
        "--gcc-root",
        type=Path,
        default=DEFAULT_GCC_ROOT,
        help=(
            "Path to the GCC source tree containing gcc/testsuite "
            "(default: ../gcc-dev/gcc relative to repository root)."
        ),
    )
    parser.add_argument(
        "--fortfront",
        type=Path,
        help=(
            "Path to an existing fortfront executable. When omitted, the script "
            "searches build/gfortran_*/app/fortfront and uses the newest binary."
        ),
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=DEFAULT_OUTPUT,
        help=f"Result file to write (default: {DEFAULT_OUTPUT}).",
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=30.0,
        help="Per-test timeout in seconds (default: 30).",
    )
    parser.add_argument(
        "--jobs",
        type=int,
        default=1,
        help="Maximum concurrent fortfront processes (default: 1).",
    )
    parser.add_argument(
        "--resume",
        action="store_true",
        help="Skip tests already recorded in the output file.",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="List discovered tests and exit without running fortfront.",
    )
    return parser.parse_args()


def resolve_project_root() -> Path:
    return Path(__file__).resolve().parent.parent


def resolve_fortfront_binary(explicit: Path | None, project_root: Path) -> Path:
    if explicit is not None:
        candidate = explicit if explicit.is_absolute() else (Path.cwd() / explicit)
        candidate = candidate.resolve()
        if candidate.is_file() and os.access(candidate, os.X_OK):
            return candidate
        raise FileNotFoundError(f"fortfront binary not found or not executable: {candidate}")

    build_root = project_root / "build"
    if not build_root.exists():
        raise FileNotFoundError(
            "No build directory found. Run `fpm build --profile release` first "
            "or provide --fortfront."
        )

    newest_binary = None
    newest_mtime = -1.0
    for build_dir in build_root.glob("gfortran_*"):
        binary = build_dir / "app" / "fortfront"
        if binary.is_file() and os.access(binary, os.X_OK):
            mtime = binary.stat().st_mtime
            if mtime > newest_mtime:
                newest_binary = binary
                newest_mtime = mtime

    if newest_binary is None:
        raise FileNotFoundError(
            "Could not locate fortfront binary under build/. Run `fpm build` "
            "or pass --fortfront."
        )
    return newest_binary


def discover_gfortran_tests(gcc_root: Path) -> List[Path]:
    gcc_root = gcc_root.resolve()
    testsuite_root = gcc_root / "gcc" / "testsuite"
    if not testsuite_root.is_dir():
        raise FileNotFoundError(f"Missing GCC testsuite directory: {testsuite_root}")

    test_dirs = sorted(
        path
        for path in testsuite_root.iterdir()
        if path.is_dir() and path.name.startswith("gfortran")
    )
    if not test_dirs:
        raise FileNotFoundError(
            f"No gfortran* directories found under {testsuite_root}. "
            "Verify --gcc-root is correct."
        )

    files: List[Path] = []
    for directory in test_dirs:
        for candidate in directory.rglob("*"):
            if candidate.is_file() and candidate.suffix.lower() in FORTRAN_SUFFIXES:
                files.append(candidate.resolve())
    files.sort()
    return files


def load_existing_results(path: Path) -> Set[str]:
    processed: Set[str] = set()
    if not path.is_file():
        return processed
    with path.open("r", encoding="utf-8") as handle:
        for line in handle:
            line = line.strip()
            if not line:
                continue
            try:
                entry = json.loads(line)
            except json.JSONDecodeError:
                continue
            if entry.get("type") == "meta":
                continue
            rel_path = entry.get("file")
            if rel_path:
                processed.add(rel_path)
    return processed


def truncate_text(text: str, limit: int = 400) -> str:
    if len(text) <= limit:
        return text
    return text[: limit - 3] + "..."


def format_seconds(seconds: float) -> str:
    if seconds == float("inf"):
        return "inf"
    seconds = max(0.0, seconds)
    mins, secs = divmod(int(seconds + 0.5), 60)
    hours, mins = divmod(mins, 60)
    if hours:
        return f"{hours}h{mins:02d}m"
    if mins:
        return f"{mins}m{secs:02d}s"
    return f"{secs}s"


def run_case(
    test_path: Path,
    fortfront_bin: Path,
    gcc_root: Path,
    timeout: float,
) -> Dict[str, object]:
    rel_path = str(test_path.relative_to(gcc_root))
    started = time.monotonic()
    cmd = [str(fortfront_bin), str(test_path)]
    try:
        completed = subprocess.run(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=False,
            timeout=timeout,
        )
        duration = time.monotonic() - started
        status = "pass" if completed.returncode == 0 else "fail"
        stderr_text = completed.stderr.decode("utf-8", errors="replace").strip()
        record = {
            "file": rel_path,
            "status": status,
            "exit_code": completed.returncode,
            "duration_s": round(duration, 4),
            "stdout_bytes": len(completed.stdout),
        }
        if status != "pass":
            record["stderr_preview"] = truncate_text(stderr_text, 600)
        return record
    except subprocess.TimeoutExpired as exc:
        duration = time.monotonic() - started
        return {
            "file": rel_path,
            "status": "timeout",
            "exit_code": None,
            "duration_s": round(duration, 4),
            "stdout_bytes": exc.output and len(exc.output) or 0,
            "stderr_preview": truncate_text(
                (exc.stderr or b"").decode("utf-8", errors="replace"), 600
            ),
        }


def print_progress(
    processed: int,
    total: int,
    passes: int,
    failures: int,
    start_time: float,
) -> None:
    elapsed = time.monotonic() - start_time
    rate = processed / elapsed if elapsed > 0 else 0.0
    remaining = total - processed
    eta = remaining / rate if rate > 0 else float("inf")
    percent = (processed / total * 100.0) if total else 100.0
    message = (
        f"\r[{percent:6.2f}%] {processed}/{total} "
        f"(pass {passes} | fail {failures}) "
        f"elapsed {format_seconds(elapsed)} "
        f"eta {format_seconds(eta)} "
        f"rate {rate:.1f}/s"
    )
    sys.stdout.write(message)
    sys.stdout.flush()


def write_meta_block(handle, gcc_root: Path, fortfront_bin: Path, total: int) -> None:
    meta = {
        "type": "meta",
        "gcc_root": str(gcc_root),
        "fortfront": str(fortfront_bin),
        "total_tests": total,
        "timestamp": int(time.time()),
    }
    handle.write(json.dumps(meta) + "\n")
    handle.flush()


def main() -> int:
    args = parse_args()
    project_root = resolve_project_root()
    gcc_root = (
        args.gcc_root
        if args.gcc_root.is_absolute()
        else (project_root / args.gcc_root)
    ).resolve()

    fortfront_bin = resolve_fortfront_binary(args.fortfront, project_root)
    tests = discover_gfortran_tests(gcc_root)
    if args.dry_run:
        print(f"Discovered {len(tests)} gfortran test files:")
        for path in tests:
            print(f"  {path}")
        return 0

    output_path = (
        args.output if args.output.is_absolute() else (project_root / args.output)
    ).resolve()
    output_path.parent.mkdir(parents=True, exist_ok=True)

    already_done: Set[str] = set()
    if args.resume and output_path.exists():
        already_done = load_existing_results(output_path)

    queue = [
        path
        for path in tests
        if str(path.relative_to(gcc_root)) not in already_done
    ]
    total_to_run = len(queue)
    if total_to_run == 0:
        print("All tests already recorded; nothing to do.")
        return 0

    mode = "a" if args.resume and output_path.exists() else "w"
    with output_path.open(mode, encoding="utf-8") as handle:
        if mode == "w" or (mode == "a" and output_path.stat().st_size == 0):
            write_meta_block(handle, gcc_root, fortfront_bin, len(tests))

        processed = len(tests) - len(queue)
        passes = 0
        failures = 0
        start_time = time.monotonic()
        print(
            f"Running fortfront round-trip on {total_to_run} tests "
            f"(skipped {processed}) using {fortfront_bin}"
        )
        print_progress(processed, len(tests), passes, failures, start_time)

        worker = partial(
            run_case,
            fortfront_bin=fortfront_bin,
            gcc_root=gcc_root,
            timeout=args.timeout,
        )

        if args.jobs <= 1:
            for test_path in queue:
                record = worker(test_path)
                processed += 1
                if record["status"] == "pass":
                    passes += 1
                else:
                    failures += 1
                handle.write(json.dumps(record) + "\n")
                handle.flush()
                print_progress(processed, len(tests), passes, failures, start_time)
        else:
            with ThreadPoolExecutor(max_workers=args.jobs) as pool:
                for record in pool.map(worker, queue):
                    processed += 1
                    if record["status"] == "pass":
                        passes += 1
                    else:
                        failures += 1
                    handle.write(json.dumps(record) + "\n")
                    handle.flush()
                    print_progress(processed, len(tests), passes, failures, start_time)

    sys.stdout.write("\n")
    sys.stdout.flush()
    print(
        f"Complete. PASS: {passes}, FAIL/TIMEOUT: {failures}. "
        f"Results: {output_path}"
    )
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except KeyboardInterrupt:
        sys.stdout.write("\nInterrupted by user.\n")
        raise
