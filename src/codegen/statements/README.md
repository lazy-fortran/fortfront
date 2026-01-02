# src/codegen/statements

## Purpose

Emission of executable statements and control flow constructs.

## File Index

| File | Description |
|------|-------------|
| `codegen_control_flow.f90` | If/elseif/else, select, where constructs |
| `codegen_loop_vars_mod.f90` | Loop variable tracking and emission |
| `codegen_statements.f90` | Statement emission facade |
| `codegen_statements_part1.inc` | Assignments, print, read, write |
| `codegen_statements_part2.inc` | Return, continue, stop, cycle |

## Key Concepts

- Split large statement emitters into `.inc` parts to keep modules small.
- Keep statement emission focused on output generation, not analysis.

## Dependencies

- Uses shared helpers from `../utils/` and indentation utilities from `../api/`.

