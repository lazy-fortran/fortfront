#!/usr/bin/env python3
"""Run fortfront round-trip tests over GCC's gfortran DejaGNU suite.

This script validates fortfront's standard Fortran parsing and emission by:
1. Parsing each test file with fortfront
2. Re-parsing the output (round-trip verification)
3. Optionally compiling and running both versions to check semantic equivalence

Key outputs:
- JSONL file with per-test results including status, duration, and metadata
- Summary JSON with aggregated statistics, keyword analysis, and failure patterns
- Console output with live progress and detailed keyword statistics

Usage:
    python run_gfortran_roundtrip.py --gcc-root ../gcc-dev/gcc
    python run_gfortran_roundtrip.py --resume  # Continue interrupted run
    python run_gfortran_roundtrip.py --max-tests 100  # Quick iteration

The script detects Fortran language constructs in failing tests to help identify
which features cause the most failures (e.g., OpenMP, coarray, pointer attributes).
"""

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
import shlex
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

# File extensions for lfortran test corpus
LFORTRAN_SUFFIXES: Sequence[str] = (
    ".f90",
    ".f95",
    ".f03",
    ".f08",
)

DEFAULT_OUTPUT = Path("logs") / "gfortran_dejagnu_roundtrip_results.jsonl"
DEFAULT_GCC_ROOT = Path(os.environ.get("FF_GFORTRAN_DG_DIR", "../gcc"))
DEFAULT_LFORTRAN_ROOT = Path(os.environ.get("FF_LFORTRAN_DIR", "../lfortran"))
DEFAULT_JOBS = min(32, max(1, (os.cpu_count() or 1)))
DEFAULT_TEST_TIMEOUT = 0.05  # seconds; default timeout per test (fast path)
DEFAULT_LIVE_DIGEST = 5.0  # seconds between live digest updates (fast feedback)
DEFAULT_COMPILE_TIMEOUT = 0.5  # seconds for gfortran compile
DEFAULT_RUN_TIMEOUT = 0.5  # seconds for execution of compiled program


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Standard-Fortran round-trip harness for the GCC gfortran "
            "DejaGNU test corpus."
        )
    )
    parser.add_argument(
        "--suite",
        choices=["gfortran-dg", "lfortran"],
        default="gfortran-dg",
        help=(
            "Test suite to run (default: gfortran-dg). "
            "Use 'lfortran' for the lfortran test corpus."
        ),
    )
    parser.add_argument(
        "--gcc-root",
        type=Path,
        default=DEFAULT_GCC_ROOT,
        help=(
            "Path to the GCC source tree containing gcc/testsuite "
            "or directly to a gfortran testsuite directory "
            "(default: $FF_GFORTRAN_DG_DIR or ../gcc relative to repository root)."
        ),
    )
    parser.add_argument(
        "--lfortran-root",
        type=Path,
        default=DEFAULT_LFORTRAN_ROOT,
        help=(
            "Path to the lfortran source tree root "
            "(default: $FF_LFORTRAN_DIR or ../lfortran relative to repository root)."
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
        "--frontend-probe",
        type=Path,
        help=(
            "Path to the frontend_probe executable. When omitted, the script "
            "searches build/gfortran_*/app/frontend_probe and uses the newest binary."
        ),
    )
    parser.add_argument(
        "--output",
        "--report",
        dest="output",
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
    parser.add_argument(
        "--max-tests",
        type=int,
        default=None,
        help="Optional cap on number of tests (for quick iteration).",
    )
    parser.add_argument(
        "--live-digest-interval",
        type=float,
        default=DEFAULT_LIVE_DIGEST,
        help=(
            "Print a compact top-categories digest every N seconds while tests run "
            "(0 disables live digests; default: 5s)."
        ),
    )
    parser.add_argument(
        "--live-digest-limit",
        type=int,
        default=3,
        help=(
            "Maximum categories/signatures to show in each live digest "
            "(default: 3)."
        ),
    )
    parser.add_argument(
        "--compile-timeout",
        type=float,
        default=DEFAULT_COMPILE_TIMEOUT,
        help="Per-source compile timeout in seconds when semantic checking diffs (default: 0.5).",
    )
    parser.add_argument(
        "--run-timeout",
        type=float,
        default=DEFAULT_RUN_TIMEOUT,
        help="Per-binary run timeout in seconds when semantic checking diffs (default: 0.5).",
    )
    return parser.parse_args()


def resolve_project_root() -> Path:
    return Path(__file__).resolve().parent.parent


def resolve_build_binary(
    binary_name: str, explicit: Path | None, project_root: Path
) -> Path:
    if explicit is not None:
        candidate = explicit if explicit.is_absolute() else (Path.cwd() / explicit)
        candidate = candidate.resolve()
        if candidate.is_file() and os.access(candidate, os.X_OK):
            return candidate
        raise FileNotFoundError(
            f"{binary_name} binary not found or not executable: {candidate}"
        )

    build_root = project_root / "build"
    if not build_root.exists():
        raise FileNotFoundError(
            "No build directory found. Run `fpm build --profile release` first "
            f"or provide --{binary_name}."
        )

    newest_binary = None
    newest_mtime = -1.0
    for build_dir in build_root.glob("gfortran_*"):
        binary = build_dir / "app" / binary_name
        if binary.is_file() and os.access(binary, os.X_OK):
            mtime = binary.stat().st_mtime
            if mtime > newest_mtime:
                newest_binary = binary
                newest_mtime = mtime

    if newest_binary is None:
        raise FileNotFoundError(
            f"Could not locate {binary_name} binary under build/. Run `fpm build` "
            f"or pass --{binary_name}."
        )
    return newest_binary


def resolve_fortfront_binary(explicit: Path | None, project_root: Path) -> Path:
    return resolve_build_binary("fortfront", explicit, project_root)


def resolve_frontend_probe_binary(explicit: Path | None, project_root: Path) -> Path:
    return resolve_build_binary("frontend_probe", explicit, project_root)


def resolve_gfortran_testsuite_root(gcc_root: Path) -> Optional[Path]:
    gcc_root = gcc_root.resolve()
    if gcc_root.is_dir() and gcc_root.name.startswith("gfortran"):
        return gcc_root.parent
    if gcc_root.is_dir() and gcc_root.name == "testsuite":
        return gcc_root
    for candidate in (gcc_root / "gcc" / "testsuite", gcc_root / "testsuite"):
        if candidate.is_dir():
            return candidate.resolve()
    return None


def discover_gfortran_tests(gcc_root: Path) -> List[Path]:
    gcc_root = gcc_root.resolve()
    if gcc_root.is_dir() and gcc_root.name.startswith("gfortran"):
        test_dirs = [gcc_root]
    else:
        testsuite_root = resolve_gfortran_testsuite_root(gcc_root)
        if testsuite_root is None:
            return []

        test_dirs = sorted(
            path
            for path in testsuite_root.iterdir()
            if path.is_dir() and path.name.startswith("gfortran")
        )
        if not test_dirs:
            return []

    files: List[Path] = []
    for directory in test_dirs:
        for candidate in directory.rglob("*"):
            if candidate.is_file() and candidate.suffix.lower() in FORTRAN_SUFFIXES:
                files.append(candidate.resolve())
    files.sort()
    return files


def discover_lfortran_tests(lfortran_root: Path) -> List[Path]:
    """Discover Fortran test files in the lfortran testsuite.

    Returns an empty list (no exception) when the lfortran source tree
    is absent so callers can skip the suite cleanly.
    """
    lfortran_root = lfortran_root.resolve()
    suite_roots = [
        lfortran_root / "integration_tests",
        lfortran_root / "testsuite",
    ]
    testsuite_root = next((root for root in suite_roots if root.is_dir()), None)
    if testsuite_root is None:
        return []

    files: List[Path] = []
    for candidate in testsuite_root.rglob("*"):
        if candidate.is_file() and candidate.suffix.lower() in LFORTRAN_SUFFIXES:
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


def load_manifest_paths(path: Path) -> Set[str]:
    paths: Set[str] = set()
    if not path.is_file():
        return paths
    with path.open("r", encoding="utf-8") as handle:
        for line in handle:
            stripped = line.strip()
            if not stripped or stripped.startswith("#"):
                continue
            paths.add(stripped)
    return paths


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
    keywords: List[str] = field(default_factory=list)


@dataclass
class Classification:
    category: str
    signature: str
    description: str
    features: Dict[str, Any] = field(default_factory=dict)


class FailureAggregator:
    """Aggregates test failures and computes detailed keyword statistics."""

    def __init__(self, total_tests: int, max_examples: int = 10) -> None:
        self.max_examples = max_examples
        self.category_totals: Dict[str, int] = defaultdict(int)
        self.records: Dict[str, List[Tuple[Dict[str, object], Classification]]] = (
            defaultdict(list)
        )
        self.total_tests = total_tests
        # Detailed keyword tracking per category
        self.category_keywords: Dict[str, Counter[str]] = defaultdict(Counter)
        self.category_patterns: Dict[str, Counter[str]] = defaultdict(Counter)
        # Co-occurrence tracking: pairs of patterns that appear together
        self.pattern_cooccurrence: Counter[Tuple[str, str]] = Counter()
        # Global keyword/pattern counters for failures only
        self.global_keywords: Counter[str] = Counter()
        self.global_patterns: Counter[str] = Counter()

    def add_record(self, record: Dict[str, object]) -> None:
        classification = classify_failure_record(record)
        if classification is None:
            return
        category = classification.category
        self.records[category].append((record, classification))
        self.category_totals[category] += 1

        # Track keywords and patterns per category
        source_keywords = record.get("source_keywords") or []
        source_patterns = record.get("source_patterns") or []

        for kw in source_keywords:
            self.category_keywords[category][kw] += 1
            self.global_keywords[kw] += 1

        for pat in source_patterns:
            self.category_patterns[category][pat] += 1
            self.global_patterns[pat] += 1

        # Track pattern co-occurrence (sorted pairs to avoid duplicates)
        if len(source_patterns) >= 2:
            for i, pat1 in enumerate(source_patterns):
                for pat2 in source_patterns[i + 1 :]:
                    pair = tuple(sorted([pat1, pat2]))
                    self.pattern_cooccurrence[pair] += 1

    def build_digest(self, max_groups_per_category: int = 8) -> List[Dict[str, object]]:
        digest: List[Dict[str, object]] = []
        for category, entries in self.records.items():
            groups = self._summarize_category(category, entries)
            groups.sort(key=lambda g: g.count, reverse=True)
            digest.append(
                {
                    "category": category,
                    "total": self.category_totals[category],
                    "percent_all_tests": round(
                        100.0 * self.category_totals[category] / self.total_tests, 3
                    ),
                    "percent_failures": None,  # filled later
                    "groups": [
                        {
                            "signature": entry.signature,
                            "description": entry.description,
                            "count": entry.count,
                            "examples": entry.examples,
                            "keywords": getattr(entry, "keywords", []),
                        }
                        for entry in groups[:max_groups_per_category]
                    ],
                    "remaining_groups": max(0, len(entries) - max_groups_per_category),
                }
            )
        digest.sort(key=lambda entry: entry["total"], reverse=True)
        total_failed = sum(entry["total"] for entry in digest)
        if total_failed:
            for entry in digest:
                entry["percent_failures"] = round(100.0 * entry["total"] / total_failed, 3)
        return digest

    def _summarize_category(
        self,
        category: str,
        entries: List[Tuple[Dict[str, object], Classification]],
    ) -> List[SummaryGroup]:
        if category == "roundtrip_diff":
            return self._summarize_diff(entries)
        grouped: Dict[str, SummaryGroup] = {}
        keyword_buckets: Dict[str, Counter[str]] = {}
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
                keyword_buckets[key] = Counter()
            group.count += 1
            file_path = record.get("file")
            if isinstance(file_path, str) and len(group.examples) < self.max_examples:
                group.examples.append(file_path)
            features = classification.features or {}
            bucket = keyword_buckets[key]
            for kw in features.get("top_keywords", []):
                bucket[kw] += 1
            for kw in features.get("source_patterns", []):
                bucket[kw] += 1
            for kw in features.get("source_keywords", []):
                bucket[kw] += 1
        groups = list(grouped.values())
        for group in groups:
            kw_counter = keyword_buckets.get(group.signature, Counter())
            if kw_counter:
                group.keywords = [kw for kw, _ in kw_counter.most_common(8)]
        return groups

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
                # attach keyword hints
                features = items[0][1].features
                if isinstance(features, dict):
                    group.keywords = features.get("top_keywords", [])
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
            pct_all = section.get("percent_all_tests")
            pct_fail = section.get("percent_failures")
            hdr_parts = [f"[{category}] total={total}"]
            if pct_all is not None:
                hdr_parts.append(f"{pct_all:.3f}% of all tests")
            if pct_fail is not None:
                hdr_parts.append(f"{pct_fail:.3f}% of failures")
            print("\n" + " | ".join(hdr_parts))
            for group in section["groups"]:
                signature = group["signature"]
                count = group["count"]
                description = group["description"]
                examples = ", ".join(group["examples"])
                keywords = group.get("keywords", [])
                kw_text = f" keywords={','.join(keywords)}" if keywords else ""
                example_text = f" examples: {examples}" if examples else ""
                print(f"  - {signature} ({count}) :: {description}{kw_text}{example_text}")
            remaining = section["remaining_groups"]
            if remaining:
                print(f"    ... {remaining} additional unique signatures omitted ...")

    def build_heatmaps(self, top_n: int = 12) -> Dict[str, object]:
        """Build comprehensive heatmaps for paths, keywords, and patterns."""
        path_counter: Counter[str] = Counter()
        for entries in self.records.values():
            for record, _ in entries:
                cluster = extract_path_cluster(record) or "unknown"
                path_counter[cluster] += 1

        return {
            "paths": path_counter.most_common(top_n),
            "keywords": self.global_keywords.most_common(top_n),
            "patterns": self.global_patterns.most_common(top_n),
            "cooccurrence": [
                (f"{p1}+{p2}", cnt)
                for (p1, p2), cnt in self.pattern_cooccurrence.most_common(top_n)
            ],
        }

    def build_category_keyword_stats(self, top_n: int = 10) -> Dict[str, object]:
        """Build per-category keyword and pattern statistics."""
        stats: Dict[str, object] = {}
        for category in self.records:
            cat_total = self.category_totals[category]
            kw_counter = self.category_keywords[category]
            pat_counter = self.category_patterns[category]
            stats[category] = {
                "total": cat_total,
                "top_keywords": [
                    {"keyword": kw, "count": cnt, "percent": round(100 * cnt / cat_total, 1)}
                    for kw, cnt in kw_counter.most_common(top_n)
                ],
                "top_patterns": [
                    {"pattern": pat, "count": cnt, "percent": round(100 * cnt / cat_total, 1)}
                    for pat, cnt in pat_counter.most_common(top_n)
                ],
            }
        return stats


def classify_failure_record(record: Dict[str, object]) -> Optional[Classification]:
    status = record.get("status")
    expected = bool(record.get("expected_failure", False))

    def finalize(cls: Classification) -> Classification:
        merge_source_features(cls.features, record)
        return cls

    if status == "skipped_ref_no_compile":
        # Don't count skipped tests as failures
        return None
    if status in {"pass", "pass_equivalent"}:
        if expected:
            path_hint = extract_path_cluster(record)
            desc_suffix = f" [{path_hint}]" if path_hint else ""
            return finalize(
                Classification(
                    category="unexpected_pass",
                    signature=f"XPASS:{path_hint or 'generic'}",
                    description=f"Expected failure passed{desc_suffix}",
                    features={"expected": True, "path": path_hint},
                )
            )
        return None
    if status == "parse_fail":
        diagnostic = str(record.get("diagnostic_text", ""))
        path_hint = extract_path_cluster(record)
        label = categorize_message(diagnostic)
        signature = f"parse:{path_hint or label or 'generic'}"
        description = (
            f"Frontend parse failed: {first_line(diagnostic) or path_hint or 'no diagnostic'}"
        )
        return finalize(
            Classification(
                category="frontend_parse_fail",
                signature=signature,
                description=description,
                features={"label": label, "path": path_hint},
            )
        )
    if status == "sema_fail":
        diagnostic = str(record.get("diagnostic_text", ""))
        path_hint = extract_path_cluster(record)
        label = categorize_message(diagnostic)
        signature = f"sema:{path_hint or label or 'generic'}"
        description = (
            f"Frontend semantic analysis failed: {first_line(diagnostic) or path_hint or 'no diagnostic'}"
        )
        return finalize(
            Classification(
                category="frontend_sema_fail",
                signature=signature,
                description=description,
                features={"label": label, "path": path_hint},
            )
        )
    if status == "probe_timeout":
        return finalize(
            Classification(
                category="frontend_probe_timeout",
                signature="probe_timeout",
                description="Frontend probe timed out",
            )
        )
    if status == "probe_json":
        return finalize(
            Classification(
                category="frontend_probe_json",
                signature="probe_json",
                description="Frontend probe produced invalid JSON",
            )
        )
    if status == "probe_exit":
        code = record.get("probe_exit_code")
        raw_stderr = str(record.get("probe_stderr", ""))
        stderr_line = first_line(raw_stderr)
        label = categorize_message(raw_stderr)
        signature = f"probe_exit:{code}:{label or 'generic'}"
        description = f"Frontend probe exit {code}: {label or stderr_line or 'no stderr'}"
        return finalize(
            Classification(
                category="frontend_probe_exit",
                signature=signature,
                description=description,
                features={"label": label},
            )
        )
    if expected:
        path_hint = extract_path_cluster(record)
        desc_suffix = f" [{path_hint}]" if path_hint else ""
        return finalize(
            Classification(
                category="expected_failure",
                signature=f"XFAIL:{path_hint or 'generic'}",
                description=f"Expected failure (per dg directives){desc_suffix}",
                features={"expected": True, "path": path_hint},
            )
        )
    if status in {"fail", "roundtrip_fail"}:
        # legacy catch-all
        if "roundtrip_diff" in record:
            diff_text = str(record.get("roundtrip_diff", ""))
            signature, description, features = extract_diff_signature(diff_text)
            return finalize(
                Classification(
                    category="roundtrip_diff",
                    signature=signature,
                    description=description,
                    features=features,
                )
            )
        return finalize(
            Classification(
                category="roundtrip_unknown",
                signature="unknown",
                description="Round-trip failure (unspecified)",
            )
        )
    if status == "roundtrip_timeout":
        return finalize(
            Classification(
                category="roundtrip_timeout",
                signature="second_pass_timeout",
                description="Second fortfront invocation timed out",
            )
        )
    if status == "roundtrip_exit":
        code = record.get("roundtrip_exit_code")
        raw_stderr = str(record.get("roundtrip_stderr", ""))
        stderr_line = first_line(raw_stderr)
        label = categorize_message(raw_stderr)
        path_hint = extract_path_cluster(record)
        signature = f"exit:{code}:{label or 'generic'}"
        description = f"Round-trip exit {code}: {label or stderr_line or 'no stderr'}"
        return finalize(
            Classification(
                category="roundtrip_exit",
                signature=signature,
                description=description,
                features={"label": label, "path": path_hint},
            )
        )
    if status == "pass_equivalent":
        path_hint = extract_path_cluster(record)
        return finalize(
            Classification(
                category="equivalent_not_identical",
                signature=f"equiv:{path_hint or 'generic'}",
                description="Byte diff but compile+run outputs match",
                features={"path": path_hint},
            )
        )
    if status == "compile_fail_ref":
        return finalize(
            Classification(
                category="compile_fail_ref",
                signature="ref_compile",
                description="Reference source failed to compile during semantic check",
            )
        )
    if status == "compile_fail_roundtrip":
        return finalize(
            Classification(
                category="compile_fail_roundtrip",
                signature="rt_compile",
                description="Round-trip source failed to compile during semantic check",
            )
        )
    if status == "runtime_fail_ref":
        return finalize(
            Classification(
                category="runtime_fail_ref",
                signature="ref_runtime",
                description="Reference binary failed or timed out during semantic check",
            )
        )
    if status == "runtime_fail_roundtrip":
        return finalize(
            Classification(
                category="runtime_fail_roundtrip",
                signature="rt_runtime",
                description="Round-trip binary failed or timed out during semantic check",
            )
        )
    if status == "output_mismatch":
        path_hint = extract_path_cluster(record)
        return finalize(
            Classification(
                category="output_mismatch",
                signature=f"output_diff:{path_hint or 'generic'}",
                description="Compile+run succeeded but outputs differ",
                features={"path": path_hint},
            )
        )
    if status == "compare_failure":
        return finalize(
            Classification(
                category="compare_failure",
                signature="compare_failure",
                description="Structural diff and semantic check did not match outputs",
            )
        )
    if status == "fatal":
        if record.get("stderr_preview") == "No output produced for successful transform":
            return finalize(
                Classification(
                    category="fatal_no_output",
                    signature="no_output",
                    description="fortfront exited 0 but stdout was empty",
                )
            )
        exit_code = record.get("exit_code")
        stderr_text = str(record.get("stderr_preview", ""))
        label = categorize_message(stderr_text)
        signature = f"fatal:{exit_code}:{label or 'generic'}"
        description = f"fortfront exit {exit_code}: {label or first_line(stderr_text) or 'no stderr'}"
        return finalize(
            Classification(
                category="fatal_exit",
                signature=signature,
                description=description,
                features={"label": label},
            )
        )
    if status == "timeout":
        stderr_text = str(record.get("stderr_preview", ""))
        stderr_line = first_line(stderr_text)
        normalized = normalize_message(stderr_text)
        path_hint = extract_path_cluster(record)
        label = path_hint or "generic"
        signature = f"{label}:{normalized or 'timeout'}"
        desc_suffix = f" [{path_hint}]" if path_hint else ""
        return finalize(
            Classification(
                category="transform_timeout",
                signature=signature,
                description=f"Initial fortfront invocation timed out{desc_suffix}",
                features={"path": path_hint, "normalized": normalized},
            )
        )
    if status is None:
        return finalize(
            Classification(
                category="unknown_status",
                signature="missing",
                description="Record missing status",
            )
        )
    return finalize(
        Classification(
            category="unknown_status",
            signature=str(status),
            description=f"Unhandled status {status}",
        )
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
    "program_wrapper_added": "Program wrapper inserted",
    "implicit_removed": "Implicit typing statements removed",
    "implicit_added": "Implicit typing statements inserted",
    "contains_removed": "Contains section removed",
    "end_stmt_added": "End statement inserted",
    "declaration_shuffle": "Declaration order changed",
    "data_stmt_altered": "DATA statement edited",
    "interface_changed": "Interface block changed",
    "pointer_attr_change": "Pointer attributes changed",
    "procedure_signature_change": "Subprogram signature adjusted",
    "unnamed_module_wrapper": "Unnamed module wrapper added",
    "unnamed_subroutine_wrapper": "Unnamed subroutine wrapper added",
    "bind_c_change": "BIND(C)/interop adjusted",
    "openmp_change": "OpenMP directives adjusted",
    "openacc_change": "OpenACC directives adjusted",
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
    top_keywords = keyword_signature(change_lines, top_k=8)
    signature_vector = normalized_tokens[:8] + top_tokens[:4] + top_keywords[:4]
    signature_text = " ".join(signature_vector)
    signature = f"{label}:{' | '.join(signature_vector[:6])}"
    if len(signature) > 200:
        signature = signature[:197] + "..."
    keyword_str = f" keywords={','.join(top_keywords[:4])}" if top_keywords else ""
    description = f"{label_desc}: {'; '.join(change_lines[:4])}{keyword_str}"

    features = {
        "label": label,
        "signature_text": signature_text,
        "raw_lines": change_lines[:12],
        "top_tokens": top_tokens,
        "top_keywords": top_keywords,
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


STOPWORDS = {
    "implicit",
    "none",
    "program",
    "module",
    "end",
    "subroutine",
    "function",
    "integer",
    "real",
    "logical",
    "type",
    "class",
    "contains",
    "public",
    "private",
    "use",
    "bind",
    "c",
    "omp",
    "acc",
    "coarray",
}


SOURCE_STOPWORDS = STOPWORDS | {
    "call",
    "if",
    "then",
    "else",
    "endif",
    "enddo",
    "do",
    "cycle",
    "exit",
    "return",
    "stop",
    "select",
    "case",
    "block",
    "where",
    "forall",
    "allocate",
    "deallocate",
    "character",
    "complex",
    "double",
    "precision",
    "dimension",
    "parameter",
    "intent",
    "procedure",
    "interface",
    "contains",
    "program",
    "module",
    "subroutine",
    "function",
    "type",
    "class",
    "end",
    "use",
}


SOURCE_PATTERN_RULES: Sequence[Tuple[str, str]] = (
    # Parallel/directive-based extensions
    ("openmp", r"!\$omp|\bopenmp\b|\bomp_"),
    ("openacc", r"!\$acc|\bopenacc\b|\bacc_"),
    ("coarray", r"\bcoarray\b|\bteam\b|\bsync\b(?:all|images|memory)?|\bco_\w+\b"),
    # C interoperability
    ("bind_c", r"bind\s*\(c\)|iso_c_binding|c_f_pointer|c_loc|c_funloc|c_ptr"),
    # Memory/pointer attributes
    ("pointer_attr", r"\bpointer\b|\ballocatable\b"),
    ("target_attr", r"\btarget\b"),
    # Legacy constructs
    ("equivalence", r"\bequivalence\b"),
    ("common_block", r"\bcommon\s*/"),
    ("namelist", r"\bnamelist\s*/"),
    ("entry_stmt", r"\bentry\s+\w"),
    ("block_data", r"\bblock\s+data\b"),
    ("save_stmt", r"\bsave\b"),
    # Type system features
    ("enum", r"\benum\b|\benumerator\b"),
    ("select_type", r"\bselect\s+type\b"),
    ("select_rank", r"\bselect\s+rank\b"),
    ("associate", r"\bassociate\b"),
    ("class_type", r"\bclass\s*\("),
    ("abstract_type", r"\babstract\s+type\b"),
    ("extends_type", r"\bextends\s*\("),
    ("deferred_proc", r"\bdeferred\b"),
    ("final_proc", r"\bfinal\s*::"),
    ("generic_interface", r"\bgeneric\s*::"),
    # Array features
    ("assumed_shape", r"dimension\s*\([^)]*:"),
    ("assumed_rank", r"dimension\s*\(\.\.\)|\(\.\.\)"),
    ("array_constructor", r"\[\s*\w"),
    ("implied_do", r"\(\s*\w+\s*,\s*\w+\s*="),
    ("reshape_call", r"\breshape\s*\("),
    ("spread_call", r"\bspread\s*\("),
    ("pack_unpack", r"\b(?:pack|unpack)\s*\("),
    # I/O features
    ("namelist_io", r"read\s*\([^)]*nml\s*=|write\s*\([^)]*nml\s*="),
    ("async_io", r"asynchronous\b|wait\s*\("),
    ("stream_io", r"access\s*=\s*['\"]stream['\"]"),
    ("internal_file", r"read\s*\(\s*\w+\s*,|write\s*\(\s*\w+\s*,"),
    ("format_stmt", r"^\s*\d+\s+format\b"),
    # Kind/precision
    ("complex_kind", r"complex\s*\(kind|complex\s*\(\d"),
    ("real_kind", r"real\s*\(kind|real\s*\(\d"),
    ("int_kind", r"integer\s*\(kind|integer\s*\(\d"),
    ("selected_kind", r"selected_\w+_kind\s*\("),
    # Procedures
    ("elemental", r"\belemental\b"),
    ("pure_proc", r"\bpure\b"),
    ("impure_elemental", r"\bimpure\s+elemental\b"),
    ("recursive", r"\brecursive\b"),
    ("result_clause", r"\bresult\s*\("),
    ("procedure_ptr", r"\bprocedure\s*\("),
    # Module features
    ("submodule", r"\bsubmodule\b"),
    ("use_only", r"\buse\s+\w+\s*,\s*only\s*:"),
    ("use_rename", r"\buse\s+\w+\s*,.*=>"),
    ("protected_attr", r"\bprotected\b"),
    # Misc constructs
    ("forall_stmt", r"\bforall\b"),
    ("where_stmt", r"\bwhere\s*\("),
    ("block_construct", r"\bblock\b[^_]"),
    ("critical_section", r"\bcritical\b"),
    ("error_stop", r"\berror\s+stop\b"),
    ("ieee_module", r"\bieee_\w+\b"),
    ("iso_fortran_env", r"\biso_fortran_env\b"),
)


def _strip_fortran_comments(line: str) -> str:
    """Remove trailing ! comments and skip full-line Fortran comments."""
    if not line:
        return ""
    # Column-1 comment markers (fixed form) and leading !
    trimmed = line.rstrip("\n\r")
    leading = trimmed.lstrip()
    if leading.startswith("!"):
        return ""
    if trimmed and trimmed[0] in {"c", "C", "*"} and (len(trimmed) == 1 or trimmed[1].isspace()):
        return ""
    # Remove trailing inline comment introduced by !
    return trimmed.split("!", 1)[0]


def analyze_source_text(text: str, top_k: int = 8) -> Dict[str, List[str]]:
    """Extract keyword and Fortran construct patterns from source code.

    Analyzes Fortran source to identify:
    - Top keywords (identifiers excluding common stopwords)
    - Detected language constructs (OpenMP, coarray, pointer attrs, etc.)

    Args:
        text: Fortran source code string
        top_k: Maximum number of keywords to return

    Returns:
        Dictionary with keys:
        - source_keywords: List of top identifiers in the source
        - source_patterns: List of detected Fortran construct labels
    """
    tokens: Counter[str] = Counter()
    cleaned_lines: List[str] = []
    for line in text.splitlines():
        stripped = _strip_fortran_comments(line)
        if not stripped.strip():
            continue
        cleaned_lines.append(stripped)
        no_strings = re.sub(r"(['\"])(?:\\.|(?!\1).)*\1", " ", stripped)
        for tok in _TOKEN_PATTERN.findall(no_strings.lower()):
            if tok.isdigit() or len(tok) < 3:
                continue
            normalized = _DIGIT_PATTERN.sub("#", tok)
            if normalized in SOURCE_STOPWORDS:
                continue
            tokens[normalized] += 1

    combined_text = "\n".join(cleaned_lines).lower()
    pattern_hits: List[str] = []
    for label, pattern in SOURCE_PATTERN_RULES:
        try:
            if re.search(pattern, combined_text, flags=re.IGNORECASE):
                pattern_hits.append(label)
        except re.error:
            continue

    return {
        "source_keywords": [kw for kw, _ in tokens.most_common(top_k)],
        "source_patterns": pattern_hits,
    }


def merge_source_features(features: Dict[str, Any], record: Dict[str, object]) -> None:
    """Attach source-derived keywords/patterns to a feature dict in-place."""
    if features is None:
        return
    source_keywords = record.get("source_keywords") or []
    source_patterns = record.get("source_patterns") or []
    if source_keywords:
        existing_kw = list(dict.fromkeys(features.get("top_keywords", [])))
        merged_kw = list(dict.fromkeys(existing_kw + list(source_keywords)))
        features["top_keywords"] = merged_kw
        features.setdefault("source_keywords", list(source_keywords))
    if source_patterns:
        existing_patterns = list(dict.fromkeys(features.get("source_patterns", [])))
        merged_patterns = list(dict.fromkeys(existing_patterns + list(source_patterns)))
        features["source_patterns"] = merged_patterns


def keyword_signature(lines: List[str], top_k: int = 6) -> List[str]:
    tokens: List[str] = []
    for line in lines:
        words = re.split(r"[^A-Za-z0-9_]+", line.lower())
        for word in words:
            if not word or word.isdigit() or word in STOPWORDS:
                continue
            if len(word) < 3:
                continue
            tokens.append(_DIGIT_PATTERN.sub("#", word))
    counts = Counter(tokens)
    return [word for word, _ in counts.most_common(top_k)]


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
    if has_line("program", plus_lines) and has_line("end program", plus_lines):
        return "program_wrapper_added"
    if any("unnamed_module" in line.lower() for line in plus_lines):
        return "unnamed_module_wrapper"
    if any("unnamed_subroutine" in line.lower() for line in plus_lines):
        return "unnamed_subroutine_wrapper"
    if any("bind(c" in line.lower() or "iso_c_binding" in line.lower() for line in tokens):
        return "bind_c_change"
    if any(tok.startswith("omp") or tok == "openmp" for tok in tokens):
        return "openmp_change"
    if any(tok.startswith("acc") or tok == "openacc" for tok in tokens):
        return "openacc_change"
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
    if any(tok in {"subroutine", "function"} for tok in tokens):
        return "procedure_signature_change"
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


def detect_expected_failure(test_path: Path, text: Optional[str] = None) -> bool:
    """
    Lightweight dg directive detector: if the file contains dg-shouldfail or
    dg-xfail, treat it as an expected failure.
    """
    if text is None:
        try:
            text = test_path.read_text(encoding="utf-8", errors="ignore")
        except OSError:
            return False
    text = text.lower()
    return ("dg-shouldfail" in text) or ("dg-xfail" in text)


def parse_dg_metadata(
    test_path: Path, gcc_root: Path, text: Optional[str] = None
) -> Dict[str, object]:
    """
    Minimal dg directive parser to approximate GCC harness context without copying code.
    Supports:
      - dg-options / dg-additional-options: extra flags
      - dg-additional-source / dg-additional-files: extra source paths
      - dg-do compile|run : decide whether to run
    """
    meta: Dict[str, object] = {
        "options": [],
        "extra_sources": [],
        "do_run": True,
    }
    if text is None:
        try:
            text = test_path.read_text(encoding="utf-8", errors="ignore")
        except OSError:
            return meta
    # options
    opt_patterns = [
        r"dg-options\\s*\"([^\"]+)\"",
        r"dg-additional-options\\s*\"([^\"]+)\"",
    ]
    opts: List[str] = []
    for pat in opt_patterns:
        for match in re.findall(pat, text, flags=re.IGNORECASE):
            opts.extend(shlex.split(match))
    meta["options"] = opts
    # additional sources/files
    extra_patterns = [
        r"dg-additional-source\\s*\"([^\"]+)\"",
        r"dg-additional-files\\s*\"([^\"]+)\"",
    ]
    extras: List[str] = []
    for pat in extra_patterns:
        for match in re.findall(pat, text, flags=re.IGNORECASE):
            for token in shlex.split(match):
                extras.append(token)
    testsuite_root = resolve_gfortran_testsuite_root(gcc_root)
    extra_paths: List[str] = []
    if testsuite_root is not None:
        for rel in extras:
            candidate = (testsuite_root / rel).resolve()
            if candidate.exists():
                extra_paths.append(str(candidate))
    meta["extra_sources"] = extra_paths
    # dg-do
    do_compile = re.search(r"dg-do\\s+compile", text, flags=re.IGNORECASE)
    do_run = re.search(r"dg-do\\s+run", text, flags=re.IGNORECASE)
    if do_compile and not do_run:
        meta["do_run"] = False
    else:
        meta["do_run"] = True
    return meta


# Lightweight message categorization for clearer diagnostics
_MSG_PATTERNS: List[Tuple[str, str]] = [
    ("binary_input", r"input appears to be binary data"),
    ("missing_then", r"missing 'then'"),
    ("unexpected_token_data", r"unexpected token in data"),
    ("unexpected_token_interface", r"unexpected token .*interface"),
    ("unexpected_token_module", r"unexpected token .*module"),
    ("unexpected_token_subprogram", r"unexpected token .*(subroutine|function)"),
    ("no_output", r"no output produced"),
    ("program_wrapper", r"program main"),
    ("module_wrapper", r"module unnamed_module"),
    ("unnamed_subroutine", r"subroutine unnamed_subroutine"),
    ("unnamed_function", r"function unnamed_function"),
    ("data_stmt", r"data statement"),
    ("io_format", r"expected '\\)' after write unit"),
    ("pointer_attr", r"pointer|allocatable attribute"),
    ("case_diff", r"uppercase|lowercase|case conversion"),
    ("openmp", r"openmp|omp"),
    ("openacc", r"openacc|acc"),
    ("coarray", r"coarray|sync|team"),
    ("bind_c", r"bind\(c\)|iso_c_binding|c_f_pointer"),
]


def categorize_message(text: str) -> Optional[str]:
    lowered = text.lower()
    for label, pattern in _MSG_PATTERNS:
        try:
            if re.search(pattern, lowered):
                return label
        except re.error:
            continue
    # fallback: largest keyword in stderr
    keywords = keyword_signature([text], top_k=1)
    return keywords[0] if keywords else None


def compile_and_run(
    source_text: str,
    suffix: str,
    compile_timeout: float,
    run_timeout: float,
    options: Optional[List[str]] = None,
    extra_sources: Optional[List[str]] = None,
    include_dirs: Optional[List[Path]] = None,
    do_run: bool = True,
) -> Dict[str, object]:
    """
    Compile and run provided Fortran source. Returns a dict describing status and outputs.
    Status values: compile_fail, compile_timeout, run_fail, run_timeout, run_ok.
    """
    options = options or []
    extra_sources = extra_sources or []
    include_dirs = include_dirs or []
    with tempfile.TemporaryDirectory() as tmpdir:
        src_path = Path(tmpdir) / f"code{suffix}"
        exe_path = Path(tmpdir) / "a.out"
        src_path.write_text(source_text, encoding="utf-8")
        include_flags: List[str] = []
        for inc in include_dirs:
            include_flags.append(f"-I{inc}")
        cmd = ["gfortran", *include_flags, *options, str(src_path), *extra_sources, "-o", str(exe_path)]
        try:
            compiled = subprocess.run(
                cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                timeout=max(compile_timeout, 0.01),
                check=False,
            )
        except subprocess.TimeoutExpired as exc:
            return {
                "status": "compile_timeout",
                "stderr": truncate_text((exc.stderr or b"").decode("utf-8", errors="replace"), 400),
            }
        if compiled.returncode != 0:
            return {
                "status": "compile_fail",
                "stdout": truncate_text(compiled.stdout.decode("utf-8", errors="replace"), 200),
                "stderr": truncate_text(compiled.stderr.decode("utf-8", errors="replace"), 400),
            }
        if not do_run:
            return {
                "status": "compile_ok",
                "stdout": "",
                "stderr": "",
            }
        try:
            run = subprocess.run(
                [str(exe_path)],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                timeout=max(run_timeout, 0.01),
                check=False,
            )
        except subprocess.TimeoutExpired as exc:
            return {
                "status": "run_timeout",
                "stderr": truncate_text((exc.stderr or b"").decode("utf-8", errors="replace"), 400),
            }
        if run.returncode != 0:
            return {
                "status": "run_fail",
                "returncode": run.returncode,
                "stdout": truncate_text(run.stdout.decode("utf-8", errors="replace"), 200),
                "stderr": truncate_text(run.stderr.decode("utf-8", errors="replace"), 400),
            }
        return {
            "status": "run_ok",
            "stdout": run.stdout.decode("utf-8", errors="replace"),
            "stderr": truncate_text(run.stderr.decode("utf-8", errors="replace"), 200),
        }


def print_live_digest(digest: List[Dict[str, object]], cat_limit: int, example_limit: int) -> None:
    if not digest:
        return
    sys.stdout.write("\n>> Live digest (top categories)\n")
    for section in digest[:cat_limit]:
        category = section["category"]
        total = section["total"]
        pct_all = section.get("percent_all_tests")
        header = f"[{category}] {total}"
        if pct_all is not None:
            header += f" ({pct_all:.2f}% of all tests)"
        sys.stdout.write(header + "\n")
        for group in section["groups"][:cat_limit]:
            kw = ""
            if "keywords" in group:
                kw = f" keywords={','.join(group['keywords'])}"
            examples = ", ".join(group["examples"][:example_limit])
            suffix = f" :: {examples}" if examples else ""
            sys.stdout.write(
                f"  - {group['signature']} ({group['count']}) :: {group['description']}{kw}{suffix}\n"
            )
    sys.stdout.flush()


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


def semantic_compare_sources(
    original_text: str,
    roundtrip_text: str,
    compile_timeout: float,
    run_timeout: float,
    options: List[str],
    extra_sources: List[str],
    include_dirs: List[Path],
    do_run: bool,
) -> Dict[str, object]:
    """
    Attempt semantic equivalence by compiling and running both versions.
    Returns a dict with keys describing statuses for reference and roundtrip builds,
    plus output comparison when both run_ok.
    """
    ref = compile_and_run(
        original_text,
        suffix=".f90",
        compile_timeout=compile_timeout,
        run_timeout=run_timeout,
        options=options,
        extra_sources=extra_sources,
        include_dirs=include_dirs,
        do_run=do_run,
    )
    rt = compile_and_run(
        roundtrip_text,
        suffix=".f90",
        compile_timeout=compile_timeout,
        run_timeout=run_timeout,
        options=options,
        extra_sources=extra_sources,
        include_dirs=include_dirs,
        do_run=do_run,
    )
    result = {
        "ref": ref,
        "roundtrip": rt,
    }
    success_status = {"run_ok", "compile_ok"}
    if ref.get("status") in success_status and rt.get("status") in success_status:
        if do_run and ref.get("status") == "run_ok" and rt.get("status") == "run_ok":
            ref_out = ref.get("stdout", "")
            rt_out = rt.get("stdout", "")
            result["output_match"] = ref_out == rt_out
        else:
            result["output_match"] = True
    return result


def verify_roundtrip(
    output_text: str,
    fortfront_bin: Path,
    timeout: float,
) -> tuple[str, Dict[str, object]]:
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
        return "roundtrip_timeout", {
            "roundtrip_timeout": True,
            "roundtrip_note": "Second pass timed out",
        }
    finally:
        if os.path.exists(tmp_path):
            os.unlink(tmp_path)

    if completed.returncode != 0:
        return "roundtrip_exit", {
            "roundtrip_exit_code": completed.returncode,
            "roundtrip_stderr": truncate_text(
                completed.stderr.decode("utf-8", errors="replace").strip(), 600
            ),
        }

    rt_output = completed.stdout.decode("utf-8", errors="replace")
    if normalize_source(rt_output) == normalize_source(output_text):
        return "pass", {}

    return "diff", {
        "roundtrip_diff": summarize_diff(output_text, rt_output),
        "roundtrip_output": rt_output,
    }


def probe_frontend(
    probe_bin: Path,
    test_path: Path,
    timeout: float,
) -> tuple[str, Dict[str, object]]:
    try:
        completed = subprocess.run(
            [str(probe_bin), str(test_path)],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=False,
            timeout=timeout,
        )
    except subprocess.TimeoutExpired:
        return "probe_timeout", {
            "probe_timeout": True,
            "probe_note": "Frontend probe timed out",
        }

    stdout_text = completed.stdout.decode("utf-8", errors="replace").strip()
    stderr_text = completed.stderr.decode("utf-8", errors="replace").strip()
    if completed.returncode != 0:
        return "probe_exit", {
            "probe_exit_code": completed.returncode,
            "probe_stderr": truncate_text(stderr_text, 600),
            "probe_stdout": truncate_text(stdout_text, 600),
        }

    try:
        payload = json.loads(stdout_text)
    except json.JSONDecodeError:
        return "probe_json", {
            "probe_stdout": truncate_text(stdout_text, 600),
            "probe_stderr": truncate_text(stderr_text, 600),
        }

    return "ok", {
        "parse_ok": bool(payload.get("parse_ok", False)),
        "semantic_ok": bool(payload.get("semantic_ok", False)),
        "diagnostic_text": str(payload.get("diagnostic_text", "")),
        "source_path": str(payload.get("source_path", test_path)),
    }


def run_case(
    test_path: Path,
    suite: str,
    fortfront_bin: Path,
    frontend_probe_bin: Path,
    gcc_root: Path,
    rel_root: Path,
    manifest_paths: Set[str],
    timeout: float,
    compile_timeout: float,
    run_timeout: float,
) -> Dict[str, object]:
    """Execute a single frontend-conformance test case."""
    rel_path = str(test_path.relative_to(rel_root))
    try:
        source_text = test_path.read_text(encoding="utf-8", errors="ignore")
    except OSError:
        source_text = ""

    source_features = analyze_source_text(source_text)
    expected_failure = rel_path in manifest_paths
    if suite == "gfortran-dg":
        expected_failure = expected_failure or detect_expected_failure(
            test_path, text=source_text
        )

    record: Dict[str, object] = {
        "file": rel_path,
        "suite": suite,
        "expected_failure": expected_failure,
        "source_keywords": source_features.get("source_keywords", []),
        "source_patterns": source_features.get("source_patterns", []),
    }

    probe_status, probe_detail = probe_frontend(frontend_probe_bin, test_path, timeout)
    if probe_status != "ok":
        record.update(
            {
                "status": probe_status,
                "parse_ok": False,
                "semantic_ok": False,
                "parse_state": "PARSE_FAIL",
                "sema_state": "SEMA_SKIP",
                "roundtrip_state": "ROUNDTRIP_SKIP",
                "probe_detail": probe_detail,
            }
        )
        return record

    parse_ok = bool(probe_detail.get("parse_ok", False))
    semantic_ok = bool(probe_detail.get("semantic_ok", False))
    diagnostic_text = str(probe_detail.get("diagnostic_text", ""))
    record["parse_ok"] = parse_ok
    record["semantic_ok"] = semantic_ok
    record["diagnostic_text"] = truncate_text(diagnostic_text, 600)
    record["parse_state"] = "PARSE_OK" if parse_ok else "PARSE_FAIL"
    record["sema_state"] = "SEMA_OK" if semantic_ok else "SEMA_FAIL"

    if not parse_ok:
        record["status"] = "parse_fail"
        record["roundtrip_state"] = "ROUNDTRIP_SKIP"
        return record

    if not semantic_ok:
        record["status"] = "sema_fail"
        record["roundtrip_state"] = "ROUNDTRIP_SKIP"
        return record

    started = time.monotonic()
    try:
        completed = subprocess.run(
            [str(fortfront_bin), str(test_path)],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=False,
            timeout=timeout,
        )
        duration = time.monotonic() - started
        stderr_text = completed.stderr.decode("utf-8", errors="replace").strip()
        stdout_text = completed.stdout.decode("utf-8", errors="replace")
        record["exit_code"] = completed.returncode
        record["duration_s"] = round(duration, 4)
        record["stdout_bytes"] = len(completed.stdout)
        if completed.returncode != 0:
            record["status"] = "fatal"
            record["roundtrip_state"] = "ROUNDTRIP_FAIL"
            record["stderr_preview"] = truncate_text(stderr_text, 600)
            return record
        if len(stdout_text.strip()) == 0:
            record["status"] = "fatal"
            record["roundtrip_state"] = "ROUNDTRIP_FAIL"
            record["stderr_preview"] = "No output produced for successful transform"
            return record

        rt_status, detail = verify_roundtrip(stdout_text, fortfront_bin, timeout)
        if rt_status == "pass":
            record["status"] = "pass"
            record["roundtrip_state"] = "ROUNDTRIP_OK"
            return record
        if rt_status == "roundtrip_timeout":
            record["status"] = "roundtrip_timeout"
            record["roundtrip_state"] = "ROUNDTRIP_FAIL"
            record.update(detail)
            return record
        if rt_status == "roundtrip_exit":
            record["status"] = "roundtrip_exit"
            record["roundtrip_state"] = "ROUNDTRIP_FAIL"
            record.update(detail)
            return record

        roundtrip_text = detail.get("roundtrip_output", "")
        record["roundtrip_state"] = "ROUNDTRIP_FAIL"
        record["roundtrip_diff"] = detail.get("roundtrip_diff", "")

        if suite == "gfortran-dg":
            dg_meta = parse_dg_metadata(test_path, gcc_root, text=source_text)
            testsuite_root = resolve_gfortran_testsuite_root(gcc_root)
            include_dirs = []
            if testsuite_root is not None:
                include_dirs = [
                    testsuite_root,
                    testsuite_root / "gfortran.dg",
                    testsuite_root / "gfortran.dg" / "include",
                ]
            semantic = semantic_compare_sources(
                stdout_text,
                roundtrip_text,
                compile_timeout=compile_timeout,
                run_timeout=run_timeout,
                options=dg_meta.get("options", []),
                extra_sources=dg_meta.get("extra_sources", []),
                include_dirs=include_dirs,
                do_run=dg_meta.get("do_run", True),
            )
            record["semantic_check"] = semantic
            ref_status = semantic["ref"].get("status")
            rt_status_compile = semantic["roundtrip"].get("status")
            success_status = {"run_ok", "compile_ok"}
            if (
                ref_status in success_status
                and rt_status_compile in success_status
                and semantic.get("output_match", False)
            ):
                record["status"] = "pass_equivalent"
                return record
            if ref_status in {"compile_fail", "compile_timeout"}:
                record["status"] = "compile_fail_ref"
                return record
            if rt_status_compile in {"compile_fail", "compile_timeout"}:
                record["status"] = "compile_fail_roundtrip"
                return record
            if ref_status in {"run_fail", "run_timeout"}:
                record["status"] = "runtime_fail_ref"
                return record
            if rt_status_compile in {"run_fail", "run_timeout"}:
                record["status"] = "runtime_fail_roundtrip"
                return record
            if (
                ref_status == "run_ok"
                and rt_status_compile == "run_ok"
                and not semantic.get("output_match", True)
            ):
                record["status"] = "output_mismatch"
                record["ref_stdout"] = truncate_text(
                    semantic["ref"].get("stdout", ""), 400
                )
                record["roundtrip_stdout"] = truncate_text(
                    semantic["roundtrip"].get("stdout", ""), 400
                )
                return record
            record["status"] = "compare_failure"
            return record

        record["status"] = "roundtrip_fail"
        return record
    except subprocess.TimeoutExpired as exc:
        duration = time.monotonic() - started
        record["status"] = "timeout"
        record["roundtrip_state"] = "ROUNDTRIP_FAIL"
        record["exit_code"] = None
        record["duration_s"] = round(duration, 4)
        record["stdout_bytes"] = exc.output and len(exc.output) or 0
        record["stderr_preview"] = truncate_text(
            (exc.stderr or b"").decode("utf-8", errors="replace"), 600
        )
        return record


STATUS_PASS = {"pass", "pass_equivalent"}
STATUS_FAILURE = {
    "parse_fail",
    "sema_fail",
    "roundtrip_fail",
    "fail",
    "roundtrip_exit",
    "roundtrip_timeout",
    "compile_fail_ref",
    "compile_fail_roundtrip",
    "runtime_fail_ref",
    "runtime_fail_roundtrip",
    "output_mismatch",
    "compare_failure",
}


@dataclass
class TestCounters:
    """Mutable counters for test result tracking."""

    passes: int = 0
    roundtrip_failures: int = 0
    fatals: int = 0
    timeouts: int = 0
    xfails: int = 0
    xpasses: int = 0
    skipped: int = 0

    def update_from_record(self, record: Dict[str, object]) -> None:
        """Update counters based on a test record."""
        status = record.get("status")
        expected = record.get("expected_failure", False)

        if status == "skipped_ref_no_compile":
            self.skipped += 1
        elif status in STATUS_PASS:
            self.passes += 1
            if expected:
                self.xpasses += 1
        elif expected and (status in STATUS_FAILURE or status == "fatal"):
            self.xfails += 1
        elif status in {"timeout", "probe_timeout"}:
            self.timeouts += 1
        elif status in {"fatal", "probe_exit", "probe_json"}:
            self.fatals += 1
        elif status in STATUS_FAILURE:
            self.roundtrip_failures += 1
        else:
            self.timeouts += 1


def print_progress(
    processed: int,
    total: int,
    passes: int,
    roundtrip_failures: int,
    fatals: int,
    timeouts: int,
    xfails: int,
    xpasses: int,
    skipped: int,
    start_time: float,
) -> None:
    elapsed = time.monotonic() - start_time
    rate = processed / elapsed if elapsed > 0 else 0.0
    remaining = total - processed
    eta = remaining / rate if rate > 0 else float("inf")
    percent = (processed / total * 100.0) if total else 100.0
    message = (
        f"\r[{percent:6.2f}%] {processed}/{total} "
        f"(pass {passes} | fail {roundtrip_failures} | fatal {fatals} | timeout {timeouts} | "
        f"skip {skipped} | xfail {xfails} | xpass {xpasses}) "
        f"elapsed {format_seconds(elapsed)} "
        f"eta {format_seconds(eta)} "
        f"rate {rate:.1f}/s"
    )
    sys.stdout.write(message)
    sys.stdout.flush()


def write_meta_block(
    handle,
    suite: str,
    rel_root: Path,
    fortfront_bin: Path,
    frontend_probe_bin: Path,
    total: int,
) -> None:
    meta = {
        "type": "meta",
        "suite": suite,
        "rel_root": str(rel_root),
        "fortfront": str(fortfront_bin),
        "frontend_probe": str(frontend_probe_bin),
        "total_tests": total,
        "timestamp": int(time.time()),
    }
    handle.write(json.dumps(meta) + "\n")
    handle.flush()


def main() -> int:
    args = parse_args()
    project_root = resolve_project_root()
    gcc_root = (
        args.gcc_root if args.gcc_root.is_absolute() else (project_root / args.gcc_root)
    ).resolve()
    lfortran_root = (
        args.lfortran_root
        if args.lfortran_root.is_absolute()
        else (project_root / args.lfortran_root)
    ).resolve()

    if args.suite == "gfortran-dg":
        suite = "gfortran-dg"
        rel_root = resolve_gfortran_testsuite_root(gcc_root) or gcc_root
        tests = discover_gfortran_tests(gcc_root)
        manifest_path = (
            project_root / "test" / "conformance" / "frontend_xfail_gfortran_dg.txt"
        )
    else:
        suite = "lfortran"
        rel_root = lfortran_root
        tests = discover_lfortran_tests(lfortran_root)
        manifest_path = (
            project_root / "test" / "conformance" / "frontend_xfail_lfortran.txt"
        )

    if args.dry_run:
        if tests:
            print(f"Discovered {len(tests)} {suite} test files:")
            for path in tests:
                print(f"  {path}")
        else:
            print(f"SKIP: {suite} suite unavailable at {rel_root}")
        return 0

    if not tests:
        print(f"SKIP: {suite} suite unavailable at {rel_root}")
        return 0

    fortfront_bin = resolve_fortfront_binary(args.fortfront, project_root)
    frontend_probe_bin = resolve_frontend_probe_binary(args.frontend_probe, project_root)

    output_path = (
        args.output if args.output.is_absolute() else (project_root / args.output)
    ).resolve()
    output_path.parent.mkdir(parents=True, exist_ok=True)

    manifest_paths = load_manifest_paths(manifest_path)

    already_done: Set[str] = set()
    if args.resume and output_path.exists():
        already_done = load_existing_results(output_path)

    queue = [
        path
        for path in tests
        if str(path.relative_to(rel_root)) not in already_done
    ]
    total_to_run = len(queue)
    if total_to_run == 0:
        print("All tests already recorded; nothing to do.")
        return 0

    mode = "a" if args.resume and output_path.exists() else "w"
    with output_path.open(mode, encoding="utf-8") as handle:
        if mode == "w" or (mode == "a" and output_path.stat().st_size == 0):
            write_meta_block(
                handle,
                suite,
                rel_root,
                fortfront_bin,
                frontend_probe_bin,
                len(tests),
            )

        processed = len(tests) - len(queue)
        counters = TestCounters()
        start_time = time.monotonic()
        effective_timeout = max(args.timeout, 0.0)
        aggregator = FailureAggregator(total_tests=len(tests))
        last_digest = start_time

        print(
            f"Running {suite} frontend conformance on {total_to_run} tests "
            f"(skipped {processed}) using {fortfront_bin}"
        )
        print_progress(
            processed, len(tests), counters.passes, counters.roundtrip_failures,
            counters.fatals, counters.timeouts, counters.xfails, counters.xpasses,
            counters.skipped, start_time,
        )

        worker = partial(
            run_case,
            suite=suite,
            fortfront_bin=fortfront_bin,
            frontend_probe_bin=frontend_probe_bin,
            gcc_root=gcc_root,
            rel_root=rel_root,
            manifest_paths=manifest_paths,
            timeout=effective_timeout,
            compile_timeout=args.compile_timeout,
            run_timeout=args.run_timeout,
        )

        def process_record(record: Dict[str, object]) -> None:
            """Process a single test record: update counters, write, and report."""
            nonlocal processed, last_digest
            processed += 1
            counters.update_from_record(record)
            handle.write(json.dumps(record) + "\n")
            handle.flush()
            aggregator.add_record(record)
            if args.live_digest_interval > 0 and (
                time.monotonic() - last_digest >= args.live_digest_interval
            ):
                digest = aggregator.build_digest(
                    max_groups_per_category=args.live_digest_limit
                )
                print_live_digest(digest, args.live_digest_limit, example_limit=2)
                last_digest = time.monotonic()
            print_progress(
                processed, len(tests), counters.passes, counters.roundtrip_failures,
                counters.fatals, counters.timeouts, counters.xfails, counters.xpasses,
                counters.skipped, start_time,
            )

        if args.jobs <= 1:
            for test_path in queue:
                process_record(worker(test_path))
        else:
            with ThreadPoolExecutor(max_workers=args.jobs) as pool:
                for record in pool.map(worker, queue):
                    process_record(record)

    sys.stdout.write("\n")
    sys.stdout.flush()
    print(
        f"Complete [{suite}]. PASS: {counters.passes}, FAIL: {counters.roundtrip_failures}, "
        f"FATAL: {counters.fatals}, TIMEOUT: {counters.timeouts}, "
        f"SKIP: {counters.skipped}. XFAIL: {counters.xfails}, XPASS: {counters.xpasses}. "
        f"Results: {output_path}"
    )
    digest = aggregator.build_digest()
    FailureAggregator.print_digest(digest)
    heatmaps = aggregator.build_heatmaps()
    category_stats = aggregator.build_category_keyword_stats()

    # Print comprehensive statistics
    print("\n" + "=" * 70)
    print("KEYWORD STATISTICS FOR FAILING TESTS")
    print("=" * 70)

    print("\nPath clusters (directories with most failures):")
    for path, count in heatmaps["paths"]:
        print(f"  {path}: {count}")

    print("\nTop keywords in failing tests:")
    for kw, count in heatmaps["keywords"]:
        print(f"  {kw}: {count}")

    print("\nFortran constructs detected in failing tests:")
    for pat, count in heatmaps["patterns"]:
        print(f"  {pat}: {count}")

    if heatmaps.get("cooccurrence"):
        print("\nPattern co-occurrence (constructs appearing together):")
        for pair, count in heatmaps["cooccurrence"]:
            print(f"  {pair}: {count}")

    # Per-category breakdown
    print("\n" + "-" * 70)
    print("PER-CATEGORY KEYWORD BREAKDOWN")
    print("-" * 70)
    for category, stats in sorted(
        category_stats.items(), key=lambda x: x[1]["total"], reverse=True
    ):
        cat_total = stats["total"]
        print(f"\n[{category}] ({cat_total} tests)")
        if stats["top_patterns"]:
            print("  Fortran constructs:")
            for item in stats["top_patterns"][:5]:
                print(f"    {item['pattern']}: {item['count']} ({item['percent']}%)")
        if stats["top_keywords"]:
            print("  Keywords:")
            for item in stats["top_keywords"][:5]:
                print(f"    {item['keyword']}: {item['count']} ({item['percent']}%)")

    # Write summary JSON
    summary_path = output_path.with_name(output_path.stem + "_summary.json")
    pass_equiv = next(
        (
            entry["total"]
            for entry in digest
            if entry["category"] == "equivalent_not_identical"
        ),
        0,
    )
    summary_data = {
        "suite": suite,
        "rel_root": str(rel_root),
        "output_file": str(output_path),
        "summary": digest,
        "totals": {
            "pass": counters.passes,
            "pass_equivalent": pass_equiv,
            "roundtrip_fail": counters.roundtrip_failures,
            "fatal": counters.fatals,
            "timeout": counters.timeouts,
            "skipped": counters.skipped,
            "xfail": counters.xfails,
            "xpass": counters.xpasses,
        },
        "heatmap": heatmaps,
        "category_keyword_stats": category_stats,
    }
    with summary_path.open("w", encoding="utf-8") as summary_handle:
        json.dump(summary_data, summary_handle, indent=2)
        summary_handle.write("\n")
    print(f"\nSummary digest written to {summary_path}")
    if counters.fatals > 0 or counters.timeouts > 0:
        return 2
    if counters.roundtrip_failures > 0:
        return 1
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except KeyboardInterrupt:
        sys.stdout.write("\nInterrupted by user.\n")
        raise
