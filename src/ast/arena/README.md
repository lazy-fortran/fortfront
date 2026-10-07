# AST Arena

## Purpose

This directory owns AST node storage, indexed tree links, generation-based
handles, and source-text lookup.

## File Index

| File | Description |
|------|-------------|
| ast_arena_core.f90 | Handle-based storage and slot management |
| ast_arena_modern.f90 | Indexed AST entries, tree links, and the `ast_arena_t` interface |
| ast_arena_source_text.f90 | Source text storage and retrieval utilities for arenas |

## Key Concepts

For complete arena allocation design principles, see [AST README](../README.md#key-concepts) and [src/memory/README.md](../../../src/memory/README.md).

`ast_arena_t` extends the core arena and owns its indexed entries. Source-text
helpers operate on the same arena type.

### Source Text Retrieval API Conventions

The source text retrieval API lives in `src/ast/arena/ast_arena_source_text.f90`.
Behavioral coverage is in `test/api/test_source_text_retrieval_api.f90`.

- Source text is normalized to LF line endings when stored (`CRLF` becomes
  `new_line('A')`).
- Lines and columns are 1-based.
- Range queries are inclusive on both ends: `(start_line, start_col)` through
  `(end_line, end_col)`.
- If the stored source ends with a newline, there is a trailing empty line and
  `get_source_line` returns `found = .true.` with empty text for that line.
- An empty range at EOF is treated as found: when the range maps to
  `start_pos == end_pos == len(source) + 1`, `get_source_range` returns
  `found = .true.` with empty text.

## Dependencies

**Memory Infrastructure**
- `memory/arena_memory` - General-purpose arena allocator
- `memory/compiler_arena` - Compiler-wide allocation context

**AST Types**
- `ast/ast_base` - Base node types for allocation
- `ast/ast_types` - Type metadata for size calculations
