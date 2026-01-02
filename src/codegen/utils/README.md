# src/codegen/utils

## Purpose

Shared helpers used across code generation: string handling, character utilities,
name mangling, and import reordering.

## File Index

| File | Description |
|------|-------------|
| `codegen_basic_utils.f90` | Basic string utilities and formatting helpers |
| `codegen_character_normalization.f90` | Character length normalization |
| `codegen_character_types.f90` | Character type handling |
| `codegen_entry_utils.f90` | Entry statement handling (legacy Fortran) |
| `codegen_import_reorder.f90` | Reorder use/import statements |
| `codegen_name_mangling.f90` | Name mangling for monomorphization |

## Key Concepts

- Keep low-level utilities here to avoid circular dependencies in emitters.
- Keep formatting helpers deterministic and allocation-aware.

## Dependencies

- Used by most modules under `src/codegen/`.

