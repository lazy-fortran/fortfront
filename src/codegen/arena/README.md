# src/codegen/arena

## Purpose

Helpers for interacting with the compiler arena during code generation, keeping
node access and traversal safe and consistent.

## File Index

| File | Description |
|------|-------------|
| `codegen_arena_interface.f90` | Arena memory interface for safe node access |
| `codegen_arena_utils.f90` | Arena-based utilities for node traversal |

## Key Concepts

- Prefer arena access helpers over ad-hoc node access.
- Keep traversal utilities centralized to avoid duplication.

## Dependencies

- Depends on arena and AST infrastructure under `src/memory/` and `src/ast/`.

