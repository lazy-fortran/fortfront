# Shims

## Purpose

This directory holds compatibility shims for external libraries that may not be
available or may have incompatible interfaces. Shims provide a stable interface
while allowing the underlying implementation to vary based on availability or
platform.

## File Index

| File | Description |
|------|-------------|

No shims are currently in use. The previous `json_module.f90` no-op was removed
because AST JSON serialization is emitted directly by `ast_to_json` in
`src/utilities/fortfront_utils.f90`.

## Key Concepts

**When to Add Shims**
- External library with unstable API
- Optional dependency for non-critical features
- Platform-specific library with alternatives
- Dependency with licensing concerns

## Dependencies

None.
