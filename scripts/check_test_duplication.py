#!/usr/bin/env python3
"""Check for inline multi-line source blocks in tests.

CLAUDE.md zero-duplication policy applies to end-to-end tests:
- End-to-end: extract full programs to examples/ and use read_example()
- Unit tests: small inline snippets are allowed

This script flags *single inline string blocks* built with new_line('a'), not
total file counts (which produce false positives across multiple small cases).
"""

from __future__ import annotations

import sys
from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class InlineBlock:
    start_line: int
    newline_count: int


def _strip_fortran_comment(line: str) -> str:
    in_single = False
    in_double = False
    i = 0
    while i < len(line):
        ch = line[i]
        if ch == "'" and not in_double:
            if in_single and i + 1 < len(line) and line[i + 1] == "'":
                i += 2
                continue
            in_single = not in_single
        elif ch == '"' and not in_single:
            if in_double and i + 1 < len(line) and line[i + 1] == '"':
                i += 2
                continue
            in_double = not in_double
        elif ch == "!" and not in_single and not in_double:
            return line[:i]
        i += 1
    return line


def _find_fortran_assignment_operator(statement: str) -> int | None:
    in_single = False
    in_double = False
    idx = 0
    while idx < len(statement):
        ch = statement[idx]
        if ch == "'" and not in_double:
            if in_single and idx + 1 < len(statement) and statement[idx + 1] == "'":
                idx += 2
                continue
            in_single = not in_single
            idx += 1
            continue
        if ch == '"' and not in_single:
            if in_double and idx + 1 < len(statement) and statement[idx + 1] == '"':
                idx += 2
                continue
            in_double = not in_double
            idx += 1
            continue
        if in_single or in_double:
            idx += 1
            continue
        if ch == "=":
            prev_ch = statement[idx - 1] if idx > 0 else ""
            next_ch = statement[idx + 1] if idx + 1 < len(statement) else ""
            if prev_ch in {"<", ">", "/", "="}:
                idx += 1
                continue
            if next_ch in {">", "="}:
                idx += 1
                continue
            return idx
        idx += 1
    return None


def _leading_identifier(text: str) -> str | None:
    stripped = text.lstrip()
    i = 0
    while i < len(stripped) and stripped[i].isdigit():
        i += 1
    stripped = stripped[i:].lstrip()
    if not stripped:
        return None
    first = stripped[0]
    if not (first.isalpha() or first == "_"):
        return None
    i = 1
    while i < len(stripped) and (stripped[i].isalnum() or stripped[i] == "_"):
        i += 1
    return stripped[:i]


def _count_fortran_new_line_calls(statement: str) -> int:
    in_single = False
    in_double = False
    idx = 0
    count = 0
    while idx < len(statement):
        ch = statement[idx]
        if ch == "'" and not in_double:
            if in_single and idx + 1 < len(statement) and statement[idx + 1] == "'":
                idx += 2
                continue
            in_single = not in_single
            idx += 1
            continue
        if ch == '"' and not in_single:
            if in_double and idx + 1 < len(statement) and statement[idx + 1] == '"':
                idx += 2
                continue
            in_double = not in_double
            idx += 1
            continue
        if in_single or in_double:
            idx += 1
            continue
        if statement[idx : idx + 8].lower() == "new_line":
            j = idx + 8
            while j < len(statement) and statement[j].isspace():
                j += 1
            if j >= len(statement) or statement[j] != "(":
                idx += 1
                continue
            k = j + 1
            while k < len(statement) and statement[k].isspace():
                k += 1
            if k < len(statement) and statement[k] in {"'", '"'}:
                count += 1
                idx = k + 1
                continue
        idx += 1
    return count


def _is_likely_inline_source_statement(statement: str) -> bool:
    op_index = _find_fortran_assignment_operator(statement)
    if op_index is None:
        return False
    lhs = statement[:op_index]
    name = _leading_identifier(lhs)
    if name is None:
        return False
    var = name.lower()
    if var == "src":
        return True
    if var == "source":
        return True
    if var.startswith("source"):
        return True
    if var.startswith("input"):
        return True
    if var.endswith("_source"):
        return True
    if var in {"input", "input_source", "program_source", "source_text"}:
        return True
    return False


def _statements_with_newlines(lines: list[str]) -> list[InlineBlock]:
    blocks: list[InlineBlock] = []
    statement_lines: list[str] = []
    start_line = 1

    def flush(end_line: int) -> None:
        nonlocal statement_lines, start_line
        if not statement_lines:
            return
        statement = "\n".join(statement_lines)
        newline_count = _count_fortran_new_line_calls(statement)
        if newline_count > 0 and _is_likely_inline_source_statement(statement):
            blocks.append(InlineBlock(start_line=start_line, newline_count=newline_count))
        statement_lines = []
        start_line = end_line + 1

    for idx, raw_line in enumerate(lines, start=1):
        code = _strip_fortran_comment(raw_line).rstrip()
        if not statement_lines:
            start_line = idx
        statement_lines.append(code)
        if not code:
            flush(idx)
            continue
        continues = code.endswith("&")
        if not continues:
            flush(idx)

    flush(len(lines))
    return blocks


def main() -> int:
    project_root = Path(__file__).parent.parent
    test_dir = project_root / "test"
    warn_scopes = (
        test_dir / "api",
        test_dir / "integration",
        test_dir / "lazy_fortran",
        test_dir / "system",
    )

    print("=== Checking for End-to-End Test Duplication Violations ===")
    print()
    print("CLAUDE.md policy: End-to-end tests MUST use examples/, not inline code")
    print("Unit tests with small inline snippets are OK")
    print()

    violations: list[tuple[Path, InlineBlock]] = []
    warnings: list[tuple[Path, InlineBlock]] = []

    test_files = sorted(test_dir.glob("**/*.f90"))

    print(f"Scanning {len(test_files)} test files for inline source blocks...")
    print()

    for test_file in test_files:
        try:
            lines = test_file.read_text().splitlines()
        except Exception as exc:
            print(f"Warning: Could not read {test_file}: {exc}")
            continue

        blocks = _statements_with_newlines(lines)
        if not blocks:
            continue

        max_block = max(blocks, key=lambda b: b.newline_count)
        warn_scope = any(test_file.is_relative_to(scope) for scope in warn_scopes)

        if max_block.newline_count > 15:
            violations.append((test_file, max_block))
        elif warn_scope and 6 <= max_block.newline_count <= 15:
            warnings.append((test_file, max_block))

    if violations:
        print("VIOLATIONS (single inline block >15 new_line('a') occurrences):")
        print()
        for test_file, block in sorted(violations, key=lambda x: x[1].newline_count, reverse=True):
            rel_path = test_file.relative_to(project_root)
            print(f"  {rel_path}:{block.start_line} ({block.newline_count} new_line('a'))")
        print()

    if warnings:
        print("WARNINGS (single inline block 6-15 new_line('a') occurrences):")
        print()
        for test_file, block in sorted(warnings, key=lambda x: x[1].newline_count, reverse=True):
            rel_path = test_file.relative_to(project_root)
            print(f"  {rel_path}:{block.start_line} ({block.newline_count} new_line('a'))")
        print()

    print("=== Summary ===")
    print(f"VIOLATIONS (must fix): {len(violations)} tests")
    print(f"WARNINGS (review recommended): {len(warnings)} tests")
    print()

    if violations:
        print(f"❌ FAIL: Found {len(violations)} end-to-end test violations")
        print()
        print("Migration pattern:")
        print("  1. Extract inline code to examples/f90/ or examples/lf/")
        print("  2. Update test to use: call read_example('examples/.../file.ext', source)")
        print("  3. Verify test still passes with: fpm test <test_name>")
        print()
        print("See CLAUDE.md for complete migration guide")
        return 1

    print("✓ PASS: No end-to-end test violations found")
    if warnings:
        print(f"Note: {len(warnings)} tests flagged for review (may be acceptable unit/integration tests)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
