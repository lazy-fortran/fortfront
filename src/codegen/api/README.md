# src/codegen/api

## Purpose

Public entry points and orchestration for code generation, including indentation
management used throughout the emitter.

## File Index

| File | Description |
|------|-------------|
| `codegen_api.f90` | Public API facade for code generation |
| `codegen_core.f90` | Core orchestration, main generation entry point |
| `codegen_indent.f90` | Indentation management and pretty-printing |

## Key Concepts

- Keep public API stable: callers should only need `codegen_api`.
- Centralize indentation logic to ensure consistent formatting.

## Dependencies

- Uses lower-level emitters in `../decls/`, `../expressions/`, `../programs/`,
  and `../statements/`.
- Uses shared helpers in `../utils/` and arena access helpers in `../arena/`.

