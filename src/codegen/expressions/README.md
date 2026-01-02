# src/codegen/expressions

## Purpose

Expression emission for literals, operators, function calls, and indexing.

## File Index

| File | Description |
|------|-------------|
| `codegen_expressions.f90` | Expression emission facade |
| `codegen_expressions_part1.inc` | Binary/unary ops, literals |
| `codegen_expressions_part2.inc` | Function calls, array indexing |

## Key Concepts

- Split large emitters with `.inc` parts to keep module size manageable.
- Keep expression emission side-effect free and output-only.

## Dependencies

- Uses shared helpers from `../utils/` and indentation utilities from `../api/`.

