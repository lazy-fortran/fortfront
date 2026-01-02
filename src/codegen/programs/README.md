# src/codegen/programs

## Purpose

Emission of program and module structure, including headers, bodies, and
specialization-aware grouped body generation.

## File Index

| File | Description |
|------|-------------|
| `codegen_grouped_body.f90` | Group procedure bodies by specialization |
| `codegen_grouped_body_params.f90` | Parameter grouping |
| `codegen_grouped_body_params_helpers.f90` | Parameter grouping helpers |
| `codegen_program_body.f90` | Statement emission and internal procedures |
| `codegen_program_generation.f90` | Program structure generation |
| `codegen_program_header.f90` | Program/module/function statements |
| `codegen_program_variables.f90` | Include wrapper for variable declaration sections |
| `codegen_program_variables_analysis.inc` | Infer used identifiers and types |
| `codegen_program_variables_collect.inc` | Collect declarations and uses |
| `codegen_program_variables_emit.inc` | Emit declaration blocks |

## Key Concepts

- Keep program structure emission centralized to avoid cross-module coupling.
- Use `.inc` parts where a single module would exceed size limits.

## Dependencies

- Uses declaration emitters in `../decls/` and statement emitters in
  `../statements/`.
- Uses shared helpers in `../utils/` and indentation utilities in `../api/`.

