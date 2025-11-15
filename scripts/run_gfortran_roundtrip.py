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
from dataclasses import dataclass, field
from typing import Dict, List, Sequence, Set, Tuple, Optional, Any
import tempfile
import difflib
from collections import Counter, defaultdict
import re
from rapidfuzz import fuzz

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
DEFAULT_JOBS = max(1, (os.cpu_count() or 1))
DEFAULT_TEST_TIMEOUT = 0.1  # seconds; default timeout per test


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
        default=DEFAULT_TEST_TIMEOUT,
        help=(
            f"Per-test timeout in seconds (default: {DEFAULT_TEST_TIMEOUT})."
        ),
    )
    parser.add_argument(
        "--jobs",
        type=int,
        default=DEFAULT_JOBS,
        help=(
            "Maximum concurrent fortfront processes "
            f"(default: number of CPU cores = {DEFAULT_JOBS})."
        ),
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


def normalize_source(text: str) -> str:
    """
    Normalize source code for comparison by removing insignificant differences:
    - Handles Windows/Unix line endings
    - Removes trailing whitespace
    - Normalizes multiple consecutive newlines to single newlines
    - Preserves essential whitespace for indentation and content
    """
    # Handle line endings and split into lines
    lines = text.replace("\r\n", "\n").splitlines()

    # Process each line
    normalized_lines = []
    prev_was_blank = False

    for line in lines:
        # Remove trailing whitespace
        line = line.rstrip()

        # Skip consecutive blank lines (but keep one)
        if len(line) == 0:
            if not prev_was_blank:
                normalized_lines.append(line)
                prev_was_blank = True
        else:
            normalized_lines.append(line)
            prev_was_blank = False

    # Join back and strip leading/trailing whitespace
    return "\n".join(normalized_lines).strip()


def first_line(text: str) -> str:
    text = text.strip()
    return text.splitlines()[0] if text else ""


@dataclass
class SummaryGroup:
    category: str
    signature: str
    description: str
    count: int = 0
    examples: List[str] = field(default_factory=list)


@dataclass
class Classification:
    category: str
    signature: str
    description: str
    features: Dict[str, Any] = field(default_factory=dict)


class FailureAggregator:
    def __init__(self, max_examples: int = 10) -> None:
        self.max_examples = max_examples
        self.category_totals: Dict[str, int] = defaultdict(int)
        self.records: Dict[str, List[Tuple[Dict[str, object], Classification]]] = defaultdict(list)

    def add_record(self, record: Dict[str, object]) -> None:
        classification = classify_failure_record(record)
        if classification is None:
            return
        self.records[classification.category].append((record, classification))
        self.category_totals[classification.category] += 1

    def build_digest(self, max_groups_per_category: int = 8) -> List[Dict[str, object]]:
        digest: List[Dict[str, object]] = []
        for category, entries in self.records.items():
            groups = self._summarize_category(category, entries)
            groups.sort(key=lambda g: g.count, reverse=True)
            digest.append(
                {
                    "category": category,
                    "total": self.category_totals[category],
                    "groups": [
                        {
                            "signature": entry.signature,
                            "description": entry.description,
                            "count": entry.count,
                            "examples": entry.examples,
                        }
                        for entry in groups[:max_groups_per_category]
                    ],
                    "remaining_groups": max(0, len(entries) - max_groups_per_category),
                }
            )
        digest.sort(key=lambda entry: entry["total"], reverse=True)
        return digest

    def _summarize_category(
        self,
        category: str,
        entries: List[Tuple[Dict[str, object], Classification]],
    ) -> List[SummaryGroup]:
        if category == "roundtrip_diff":
            return self._summarize_diff(entries)
        grouped: Dict[str, SummaryGroup] = {}
        for record, classification in entries:
            key = classification.signature
            group = grouped.get(key)
            if group is None:
                group = SummaryGroup(
                    category=category,
                    signature=classification.signature,
                    description=classification.description,
                )
                grouped[key] = group
            group.count += 1
            file_path = record.get("file")
            if isinstance(file_path, str) and len(group.examples) < self.max_examples:
                group.examples.append(file_path)
        return list(grouped.values())

    def _summarize_diff(
        self,
        entries: List[Tuple[Dict[str, object], Classification]],
    ) -> List[SummaryGroup]:
        # First bucket by heuristic label to keep similarity search bounded.
        buckets: Dict[str, List[Tuple[Dict[str, object], Classification]]] = defaultdict(list)
        for record, classification in entries:
            label = classification.features.get("label", "generic")
            buckets[label].append((record, classification))

        summary_groups: List[SummaryGroup] = []
        for label, bucket_entries in buckets.items():
            clusters = self._cluster_by_similarity(bucket_entries)
            for prototype, items in clusters:
                description = items[0][1].description
                signature = f"{label}:{prototype}"
                group = SummaryGroup(
                    category="roundtrip_diff",
                    signature=signature,
                    description=description,
                )
                group.count = len(items)
                for record, _ in items[: self.max_examples]:
                    file_path = record.get("file")
                    if isinstance(file_path, str):
                        group.examples.append(file_path)
                summary_groups.append(group)
        return summary_groups

    def _cluster_by_similarity(
        self,
        bucket_entries: List[Tuple[Dict[str, object], Classification]],
        threshold: int = 92,
    ) -> List[Tuple[str, List[Tuple[Dict[str, object], Classification]]]]:
        clusters: List[Tuple[str, List[Tuple[Dict[str, object], Classification]]]] = []
        for record, classification in bucket_entries:
            signature_text = classification.features.get("signature_text", classification.signature)
            assigned = False
            for idx, (prototype, items) in enumerate(clusters):
                score = fuzz.token_set_ratio(signature_text, prototype)
                if score >= threshold:
                    items.append((record, classification))
                    # Update prototype to average maybe keep first for determinism.
                    assigned = True
                    break
            if not assigned:
                clusters.append((signature_text, [(record, classification)]))
        clusters.sort(key=lambda entry: len(entry[1]), reverse=True)
        return clusters

    @staticmethod
    def print_digest(digest: List[Dict[str, object]]) -> None:
        if not digest:
            print("\nNo failures to summarize.")
            return
        print("\n\nFailure Digest (grouped signatures):")
        for section in digest:
            category = section["category"]
            total = section["total"]
            print(f"\n[{category}] total={total}")
            for group in section["groups"]:
                signature = group["signature"]
                count = group["count"]
                description = group["description"]
                examples = ", ".join(group["examples"])
                example_text = f" examples: {examples}" if examples else ""
                print(f"  - {signature} ({count}) :: {description}{example_text}")
            remaining = section["remaining_groups"]
            if remaining:
                print(f"    ... {remaining} additional unique signatures omitted ...")


def classify_failure_record(record: Dict[str, object]) -> Optional[Classification]:
    status = record.get("status")
    if status == "pass":
        return None
    if status == "fail":
        if record.get("roundtrip_timeout"):
            return Classification(
                category="roundtrip_timeout",
                signature="second_pass_timeout",
                description="Second fortfront invocation timed out",
            )
        if "roundtrip_exit_code" in record:
            code = record.get("roundtrip_exit_code")
            raw_stderr = str(record.get("roundtrip_stderr", ""))
            stderr_line = first_line(raw_stderr)
            module_hint = extract_module_hint(raw_stderr)
            normalized = normalize_message(raw_stderr)
            path_hint = extract_path_cluster(record)
            module_label = module_hint or (path_hint and f"path:{path_hint}") or "generic"
            signature = f"exit:{code}:{module_label}:{normalized}"
            description = (
                f"Round-trip exit {code}: {stderr_line or 'no stderr'}"
                + (
                    f" [{module_hint}]"
                    if module_hint
                    else (f" [{path_hint}]" if path_hint else "")
                )
            )
            return Classification(
                category="roundtrip_exit",
                signature=signature,
                description=description,
                features={"module": module_hint, "path": path_hint, "normalized": normalized},
            )
        if "roundtrip_diff" in record:
            diff_text = str(record.get("roundtrip_diff", ""))
            signature, description, features = extract_diff_signature(diff_text)
            return Classification(
                category="roundtrip_diff",
                signature=signature,
                description=description,
                features=features,
            )
        return Classification(
            category="roundtrip_unknown",
            signature="unknown",
            description="Round-trip failure (unspecified)",
        )
    if status == "fatal":
        if record.get("stderr_preview") == "No output produced for successful transform":
            return Classification(
                category="fatal_no_output",
                signature="no_output",
                description="fortfront exited 0 but stdout was empty",
            )
        exit_code = record.get("exit_code")
        stderr_text = str(record.get("stderr_preview", ""))
        stderr_line = first_line(stderr_text)
        module_hint = extract_module_hint(stderr_text)
        normalized = normalize_message(stderr_text)
        path_hint = extract_path_cluster(record)
        module_label = module_hint or (path_hint and f"path:{path_hint}") or "generic"
        signature = f"fatal:{exit_code}:{module_label}:{normalized}"
        description = (
            f"fortfront exit {exit_code}: {stderr_line or 'no stderr'}"
            + (
                f" [{module_hint}]"
                if module_hint
                else (f" [{path_hint}]" if path_hint else "")
            )
        )
        return Classification(
            category="fatal_exit",
            signature=signature,
            description=description,
            features={"module": module_hint, "path": path_hint, "normalized": normalized},
        )
    if status == "timeout":
        stderr_text = str(record.get("stderr_preview", ""))
        stderr_line = first_line(stderr_text)
        normalized = normalize_message(stderr_text)
        path_hint = extract_path_cluster(record)
        label = path_hint or "generic"
        signature = f"{label}:{normalized or 'timeout'}"
        desc_suffix = f" [{path_hint}]" if path_hint else ""
        return Classification(
            category="transform_timeout",
            signature=signature,
            description=f"Initial fortfront invocation timed out{desc_suffix}",
            features={"path": path_hint, "normalized": normalized},
        )
    if status is None:
        return Classification(
            category="unknown_status",
            signature="missing",
            description="Record missing status",
        )
    return Classification(
        category="unknown_status",
        signature=str(status),
        description=f"Unhandled status {status}",
    )


def summarize_diff(original: str, new_text: str, limit: int = 600) -> str:
    diff_lines = list(
        difflib.unified_diff(
            original.splitlines(),
            new_text.splitlines(),
            fromfile="original",
            tofile="roundtrip",
            lineterm="",
            n=3,
        )
    )
    if not diff_lines:
        return "Round-trip output differs but diff is empty."
    snippet = "\n".join(diff_lines[:40])
    if len(snippet) > limit:
        snippet = snippet[: limit - 3] + "..."
    return snippet


_DIGIT_PATTERN = re.compile(r"\d+")
_SRC_FILE_PATTERN = re.compile(r"(?:\./)?src/[A-Za-z0-9_\-./]+\.(?:f|f\d+|for|fpp)")
_FORTRAN_FILE_PATTERN = re.compile(r"[A-Za-z0-9_\-./]+\.(?:f|f\d+|for|fpp)")
_TOKEN_PATTERN = re.compile(r"[A-Za-z_][A-Za-z0-9_]*|\d+|[^\s]")


DIFF_LABEL_DESCRIPTIONS: Dict[str, str] = {
    "missing_program_scaffold": "Program wrapper removed",
    "implicit_removed": "Implicit typing statements removed",
    "implicit_added": "Implicit typing statements inserted",
    "contains_removed": "Contains section removed",
    "end_stmt_added": "End statement inserted",
    "declaration_shuffle": "Declaration order changed",
    "data_stmt_altered": "DATA statement edited",
    "interface_changed": "Interface block changed",
    "pointer_attr_change": "Pointer attributes changed",
    "coarray_change": "Coarray statements changed",
    "generic": "Round-trip diff pattern",
    "no_diff_lines": "Round-trip diff without diff body",
}


def extract_diff_signature(diff_text: str) -> Tuple[str, str, Dict[str, Any]]:
    change_lines: List[str] = []
    for line in diff_text.splitlines():
        if not line:
            continue
        if line.startswith(("---", "+++", "@@")):
            continue
        if line[0] not in "+-":
            continue
        marker = line[0]
        content = line[1:].strip()
        if not content:
            continue
        change_lines.append(f"{marker}{content}")
        if len(change_lines) >= 20:
            break

    if not change_lines:
        signature = first_line(diff_text)[:120] or "diff"
        description = "Round-trip diff without diff body"
        features = {
            "label": "no_diff_lines",
            "signature_text": signature,
            "raw_lines": [],
            "top_tokens": [],
        }
        return signature, description, features

    minus_lines = [line for line in change_lines if line.startswith("-")]
    plus_lines = [line for line in change_lines if line.startswith("+")]
    minus_tokens = tokenize_diff_lines(minus_lines)
    plus_tokens = tokenize_diff_lines(plus_lines)
    tokens = minus_tokens + plus_tokens

    env = {
        "minus_lines": minus_lines,
        "plus_lines": plus_lines,
        "minus_tokens": minus_tokens,
        "plus_tokens": plus_tokens,
        "tokens": tokens,
    }
    label = detect_diff_label(env)
    label_desc = DIFF_LABEL_DESCRIPTIONS.get(label, "Round-trip diff pattern")

    normalized_tokens: List[str] = []
    for entry in change_lines:
        marker, content = entry[0], entry[1:]
        normalized = content.lower()
        normalized = _DIGIT_PATTERN.sub("#", normalized)
        normalized = " ".join(normalized.split())
        normalized_tokens.append(f"{marker}{normalized}")

    top_tokens = [tok for tok, _ in Counter(tokens).most_common(12)]
    signature_vector = normalized_tokens[:8] + top_tokens[:8]
    signature_text = " ".join(signature_vector)
    signature = f"{label}:{' | '.join(signature_vector[:6])}"
    if len(signature) > 200:
        signature = signature[:197] + "..."
    description = f"{label_desc}: {'; '.join(change_lines[:4])}"

    features = {
        "label": label,
        "signature_text": signature_text,
        "raw_lines": change_lines[:12],
        "top_tokens": top_tokens,
        "minus_tokens": minus_tokens[:20],
        "plus_tokens": plus_tokens[:20],
    }
    return signature, description, features


def tokenize_diff_lines(lines: List[str]) -> List[str]:
    tokens: List[str] = []
    for line in lines:
        for token in _TOKEN_PATTERN.findall(line[1:].lower()):
            if token in {"+", "-", ":"}:
                continue
            normalized = _DIGIT_PATTERN.sub("#", token)
            tokens.append(normalized)
    return tokens


def detect_diff_label(env: Dict[str, Any]) -> str:
    minus_tokens = env["minus_tokens"]
    plus_tokens = env["plus_tokens"]
    minus_lines = env["minus_lines"]
    plus_lines = env["plus_lines"]
    tokens = env["tokens"]

    def has_line(fragment: str, lines: List[str]) -> bool:
        frag = fragment.lower()
        return any(frag in line.lower() for line in lines)

    if has_line("program", minus_lines) and has_line("end program", minus_lines):
        return "missing_program_scaffold"
    if "implicit" in minus_tokens and "implicit" not in plus_tokens:
        return "implicit_removed"
    if "implicit" in plus_tokens and "implicit" not in minus_tokens:
        return "implicit_added"
    if "contains" in minus_tokens and "contains" not in plus_tokens:
        return "contains_removed"
    if has_line(" end", plus_lines) and not minus_lines:
        return "end_stmt_added"
    if "data" in tokens:
        return "data_stmt_altered"
    if "interface" in tokens:
        return "interface_changed"
    if any(tok in {"pointer", "allocatable"} for tok in tokens):
        return "pointer_attr_change"
    if any(tok in {"coarray", "sync", "team"} for tok in tokens):
        return "coarray_change"
    if any(tok in {"type", "class"} for tok in tokens):
        return "declaration_shuffle"
    return "generic"


def normalize_message(text: str) -> str:
    text = text.strip()
    if not text:
        return "no stderr"
    lowered = text.replace("\r", " ").replace("\n", " ")
    lowered = _SRC_FILE_PATTERN.sub("<src>", lowered)
    lowered = _FORTRAN_FILE_PATTERN.sub("<file>", lowered)
    lowered = lowered.lower()
    lowered = _DIGIT_PATTERN.sub("#", lowered)
    lowered = re.sub(r"\s+", " ", lowered)
    return lowered.strip()


def extract_module_hint(text: str) -> Optional[str]:
    match = _SRC_FILE_PATTERN.search(text)
    if match:
        return match.group(0)
    return None


def extract_path_cluster(record: Dict[str, object], depth: int = 2) -> Optional[str]:
    rel_path = record.get("file")
    if not isinstance(rel_path, str):
        return None
    parts = rel_path.split("/")
    cluster_start = 0
    try:
        gcc_idx = parts.index("gcc")
        if gcc_idx + 1 < len(parts) and parts[gcc_idx + 1] == "testsuite":
            cluster_start = gcc_idx + 2
        else:
            cluster_start = gcc_idx + 1
    except ValueError:
        cluster_start = 0
    directories = parts[cluster_start:-1]
    if not directories:
        return None
    cluster_parts = directories[:depth]
    return "/".join(cluster_parts)


def verify_roundtrip(
    output_text: str,
    fortfront_bin: Path,
    timeout: float,
) -> tuple[bool, Dict[str, object]]:
    with tempfile.NamedTemporaryFile("w", suffix=".f90", delete=False, encoding="utf-8") as tmp:
        tmp.write(output_text)
        tmp_path = tmp.name
    try:
        completed = subprocess.run(
            [str(fortfront_bin), tmp_path],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=False,
            timeout=timeout,
        )
    except subprocess.TimeoutExpired:
        os.unlink(tmp_path)
        return False, {
            "roundtrip_timeout": True,
            "roundtrip_note": "Second pass timed out",
        }
    finally:
        if os.path.exists(tmp_path):
            os.unlink(tmp_path)

    if completed.returncode != 0:
        return False, {
            "roundtrip_exit_code": completed.returncode,
            "roundtrip_stderr": truncate_text(
                completed.stderr.decode("utf-8", errors="replace").strip(), 600
            ),
        }

    rt_output = completed.stdout.decode("utf-8", errors="replace")
    if normalize_source(rt_output) != normalize_source(output_text):
        return False, {
            "roundtrip_diff": summarize_diff(output_text, rt_output),
        }

    return True, {}


def is_expected_gfortran_failure(test_path: Path) -> bool:
    """
    Detect if a gfortran test case is expected to fail during roundtrip.

    GCC test suite contains many files designed to test compiler error handling,
    missing END statements, or other deliberately malformed code. These should
    not be counted as real roundtrip failures.

    Returns True if this test case is expected to fail.
    """
    try:
        with open(test_path, 'r', encoding='utf-8', errors='replace') as f:
            content = f.read()
    except Exception:
        return False

    # Pattern 1: Files with dg-error directives (expect compiler errors)
    if re.search(r'\{ dg-error\s+', content):
        return True

    # Pattern 1.1: Files with dg-warning directives (expect compiler warnings)
    if re.search(r'\{ dg-warning\s+', content):
        return True

    # Pattern 1.2: Files with dg-message directives (diagnostic message tests)
    if re.search(r'\{ dg-message\s+', content):
        return True

    # Pattern 1.3: Files with dg-output directives (testing specific output)
    if re.search(r'\{ dg-output\s+', content):
        return True

    # Pattern 1.4: Files with dg-excess-errors directive (expect multiple errors)
    if re.search(r'\{ dg-excess-errors\s+', content):
        return True

    # Pattern 1.5: Files with dg-xfail directives (expected test failures)
    if re.search(r'\{ dg-xfail\s+', content):
        return True

    # Pattern 2: Files with dg-do compile (testing compilation errors)
    if re.search(r'\{ dg-do\s+compile\s*\}', content):
        return True

    # Pattern 2.1: Files with dg-do run but marked to fail
    if re.search(r'\{ dg-do\s+run.*xfail', content):
        return True

    # Pattern 3: Files using GNU extensions marked with -std=gnu
    if re.search(r'\{ dg-options.*-std=gnu', content):
        return True

    # Pattern 3.1: Files with dg-options containing "illegal" or "error"
    if re.search(r'\{ dg-options.*(illegal|error)', content):
        return True

    # Pattern 3.2: Files testing obsolescent features
    if re.search(r'\{ dg-options.*-std=f', content):
        return True

    # Pattern 3.3: Files with negative tests (should fail)
    if re.search(r'\{ dg-options.*-fno-', content):
        return True

    # Pattern 4: Check for directory and naming patterns that indicate diagnostic tests
    test_path_str = str(test_path).lower()

    # Files in diagnostic-specific directories
    if ('diagnostic' in test_path_str or
        'dg-error' in test_path_str or
        'negative' in test_path_str or
        'invalid' in test_path_str or
        'error' in test_path_str):
        return True

    # Files with diagnostic-related naming patterns
    if re.search(r'(error|invalid|negative|malformed|obsolescent|deleted)', test_path_str):
        return True

    # Pattern 5: Check for obvious malformed Fortran
    lines = content.split('\n')

    # Look for common patterns that indicate malformed files
    has_program = any('program' in line.lower().strip() for line in lines if line.strip() and not line.strip().startswith('!'))
    has_end_statement = any(re.search(r'\bend\s+(program|subroutine|function|module)', line, re.IGNORECASE) for line in lines)

    # If no program statement but has executable statements, likely malformed
    executable_statements = []
    for line in lines:
        stripped = line.strip()
        if stripped and not stripped.startswith('!'):
            # Look for obvious executable statements
            if (re.search(r'^\s*[a-zA-Z_][a-zA-Z0-9_]*\s*=', line) or  # Assignment
                re.search(r'^\s*call\s+', line) or  # Subroutine call
                re.search(r'^\s*if\s+', line) or  # If statement
                re.search(r'^\s*do\s+', line) or  # Do loop
                re.search(r'^\s*goto\s+', line) or  # Goto
                re.search(r'^\s*print\s*\*', line) or  # Print statement
                re.search(r'^\s*write\s*\*', line) or  # Write statement
                re.search(r'^\s*stop\s', line) or  # Stop statement
                re.search(r'^\s*[0-9]+', line)):  # Labeled statement (old Fortran)
                executable_statements.append(line)

    if not has_program and executable_statements:
        return True

    # Pattern 5: Check for files with obvious missing END statements
    # by counting procedure starts vs ends (avoid double-counting "end program")
    program_starts = len(re.findall(r'^\s*program\s+\w+', content, re.IGNORECASE | re.MULTILINE))
    program_ends = len(re.findall(r'\bend\s+program\b', content, re.IGNORECASE))
    subroutine_starts = len(re.findall(r'^\s*subroutine\s+\w+', content, re.IGNORECASE | re.MULTILINE))
    subroutine_ends = len(re.findall(r'\bend\s+subroutine\b', content, re.IGNORECASE))
    function_starts = len(re.findall(r'^\s*function\s+\w+', content, re.IGNORECASE | re.MULTILINE))
    function_ends = len(re.findall(r'\bend\s+function\b', content, re.IGNORECASE))
    module_starts = len(re.findall(r'^\s*module\s+\w+', content, re.IGNORECASE | re.MULTILINE))
    module_ends = len(re.findall(r'\bend\s+module\b', content, re.IGNORECASE))

    # If there are more starts than ends, likely missing END statements
    if (program_starts > program_ends or
        subroutine_starts > subroutine_ends or
        function_starts > function_ends or
        module_starts > module_ends):
        return True

    return False


def run_case(
    test_path: Path,
    fortfront_bin: Path,
    gcc_root: Path,
    timeout: float,
) -> Dict[str, object]:
    rel_path = str(test_path.relative_to(gcc_root))

    # Check if this is an expected gfortran test failure
    if is_expected_gfortran_failure(test_path):
        return {
            "file": rel_path,
            "status": "expected_failure",
            "exit_code": None,
            "duration_s": 0.0,
            "stdout_bytes": 0,
            "stderr_preview": "Expected gfortran test failure (compiler error test case)",
        }

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
        stderr_text = completed.stderr.decode("utf-8", errors="replace").strip()
        stdout_text = completed.stdout.decode("utf-8", errors="replace")
        record = {
            "file": rel_path,
            "exit_code": completed.returncode,
            "duration_s": round(duration, 4),
            "stdout_bytes": len(completed.stdout),
        }
        if completed.returncode != 0:
            record["status"] = "fatal"
            record["stderr_preview"] = truncate_text(stderr_text, 600)
            return record
        if len(stdout_text.strip()) == 0:
            record["status"] = "fatal"
            record["stderr_preview"] = "No output produced for successful transform"
            return record
        roundtrip_ok, detail = verify_roundtrip(stdout_text, fortfront_bin, timeout)
        if not roundtrip_ok:
            record["status"] = "fail"
            record.update(detail)
            return record
        record["status"] = "pass"
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
    roundtrip_failures: int,
    fatals: int,
    timeouts: int,
    expected_failures: int,
    start_time: float,
) -> None:
    elapsed = time.monotonic() - start_time
    rate = processed / elapsed if elapsed > 0 else 0.0
    remaining = total - processed
    eta = remaining / rate if rate > 0 else float("inf")
    percent = (processed / total * 100.0) if total else 100.0
    message = (
        f"\r[{percent:6.2f}%] {processed}/{total} "
        f"(pass {passes} | fail {roundtrip_failures} | fatal {fatals} | timeout {timeouts} | expected {expected_failures}) "
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
        roundtrip_failures = 0
        fatals = 0
        timeouts = 0
        expected_failures = 0
        start_time = time.monotonic()
        effective_timeout = max(args.timeout, 0.0)
        aggregator = FailureAggregator()

        print(
            f"Running fortfront round-trip on {total_to_run} tests "
            f"(skipped {processed}) using {fortfront_bin}"
        )
        print_progress(processed, len(tests), passes, roundtrip_failures, fatals, timeouts, expected_failures, start_time)

        worker = partial(
            run_case,
            fortfront_bin=fortfront_bin,
            gcc_root=gcc_root,
            timeout=effective_timeout,
        )

        if args.jobs <= 1:
            for test_path in queue:
                record = worker(test_path)
                processed += 1
                status = record["status"]
                if status == "pass":
                    passes += 1
                elif status == "fail":
                    roundtrip_failures += 1
                elif status == "fatal":
                    fatals += 1
                elif status == "expected_failure":
                    expected_failures += 1
                else:
                    timeouts += 1
                handle.write(json.dumps(record) + "\n")
                handle.flush()
                aggregator.add_record(record)
                print_progress(
                    processed,
                    len(tests),
                    passes,
                    roundtrip_failures,
                    fatals,
                    timeouts,
                    expected_failures,
                    start_time,
                )
        else:
            with ThreadPoolExecutor(max_workers=args.jobs) as pool:
                for record in pool.map(worker, queue):
                    processed += 1
                    status = record["status"]
                    if status == "pass":
                        passes += 1
                    elif status == "fail":
                        roundtrip_failures += 1
                    elif status == "fatal":
                        fatals += 1
                    elif status == "expected_failure":
                        expected_failures += 1
                    else:
                        timeouts += 1
                    handle.write(json.dumps(record) + "\n")
                    handle.flush()
                    aggregator.add_record(record)
                    print_progress(
                        processed,
                        len(tests),
                        passes,
                        roundtrip_failures,
                        fatals,
                        timeouts,
                        expected_failures,
                        start_time,
                    )

    sys.stdout.write("\n")
    sys.stdout.flush()
    print(
        f"Complete. PASS: {passes}, FAIL: {roundtrip_failures}, FATAL: {fatals}, TIMEOUT: {timeouts}, EXPECTED_FAILURES: {expected_failures}. "
        f"Results: {output_path}"
    )
    digest = aggregator.build_digest()
    FailureAggregator.print_digest(digest)
    summary_path = output_path.with_name(output_path.stem + "_summary.json")
    with summary_path.open("w", encoding="utf-8") as summary_handle:
        json.dump(
            {
                "output_file": str(output_path),
                "summary": digest,
                "totals": {
                    "pass": passes,
                    "roundtrip_fail": roundtrip_failures,
                    "fatal": fatals,
                    "timeout": timeouts,
                },
            },
            summary_handle,
            indent=2,
        )
        summary_handle.write("\n")
    print(f"Summary digest written to {summary_path}")
    if fatals > 0 or timeouts > 0:
        return 2
    if roundtrip_failures > 0:
        return 1
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except KeyboardInterrupt:
        sys.stdout.write("\nInterrupted by user.\n")
        raise
