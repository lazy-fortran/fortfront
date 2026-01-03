#!/usr/bin/env python3
"""Check repository Markdown files for broken relative links.

This checker validates links that point to files or directories within the repo:
- Ignores URLs (http/https), anchors (#...), and mailto links
- Skips fenced code blocks to avoid false positives
- Supports inline links: [text](path) and reference definitions: [id]: path
"""

from __future__ import annotations

import re
import sys
from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class BrokenLink:
    markdown_file: Path
    line_no: int
    link: str
    resolved_path: Path


_INLINE_LINK_RE = re.compile(r"\[[^\]]*\]\(([^)]+)\)")
_REF_DEF_RE = re.compile(r"^\s*\[[^\]]+\]:\s+(\S+)\s*$")


def _strip_code_fences(lines: list[str]) -> list[tuple[int, str]]:
    stripped: list[tuple[int, str]] = []
    in_fence = False
    for i, line in enumerate(lines, start=1):
        if line.lstrip().startswith("```"):
            in_fence = not in_fence
            continue
        if in_fence:
            continue
        stripped.append((i, line))
    return stripped


def _normalize_target(raw: str) -> str | None:
    target = raw.strip()
    if not target:
        return None

    if target.startswith("<") and target.endswith(">"):
        target = target[1:-1].strip()
        if not target:
            return None

    if target.startswith("#"):
        return None

    lower = target.lower()
    if lower.startswith(("http://", "https://", "mailto:")):
        return None

    if lower.startswith(("vscode://", "file://")):
        return None

    if lower.startswith(("data:", "tel:")):
        return None

    if "#" in target:
        target = target.split("#", 1)[0].strip()
        if not target:
            return None

    return target


def _is_repo_relative(target: str) -> bool:
    if target.startswith("/"):
        return False
    if re.match(r"^[a-zA-Z][a-zA-Z0-9+.-]*:", target):
        return False
    return True


def _check_markdown_file(path: Path, repo_root: Path) -> list[BrokenLink]:
    lines = path.read_text(encoding="utf-8").splitlines()
    logical_lines = _strip_code_fences(lines)

    broken: list[BrokenLink] = []
    for line_no, line in logical_lines:
        for match in _INLINE_LINK_RE.finditer(line):
            target = _normalize_target(match.group(1))
            if target is None or not _is_repo_relative(target):
                continue
            resolved = (path.parent / target).resolve()
            if repo_root not in resolved.parents and resolved != repo_root:
                continue
            if not resolved.exists():
                broken.append(BrokenLink(path, line_no, target, resolved))

        ref_match = _REF_DEF_RE.match(line)
        if ref_match:
            target = _normalize_target(ref_match.group(1))
            if target is None or not _is_repo_relative(target):
                continue
            resolved = (path.parent / target).resolve()
            if repo_root not in resolved.parents and resolved != repo_root:
                continue
            if not resolved.exists():
                broken.append(BrokenLink(path, line_no, target, resolved))

    return broken


def main() -> int:
    repo_root = Path(__file__).resolve().parents[1]

    markdown_files: list[Path] = []
    for candidate in [repo_root / "README.md", repo_root / "DESIGN.md"]:
        if candidate.exists():
            markdown_files.append(candidate)

    docs_dir = repo_root / "docs"
    if docs_dir.exists():
        markdown_files.extend(sorted(docs_dir.rglob("*.md")))

    broken: list[BrokenLink] = []
    for md in markdown_files:
        broken.extend(_check_markdown_file(md, repo_root))

    if not broken:
        print(f"✅ Markdown link check passed ({len(markdown_files)} files)")
        return 0

    print(f"❌ Broken Markdown links found: {len(broken)}")
    for item in broken:
        rel_md = item.markdown_file.relative_to(repo_root)
        rel_target = item.resolved_path.relative_to(repo_root)
        print(f"{rel_md}:{item.line_no}: {item.link} -> {rel_target}")
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
