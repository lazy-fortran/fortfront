# src/codegen/decls

## Purpose

Declaration emission and supporting utilities for variables, types, procedures,
and program/module declaration structure.

## File Index

| File | Description |
|------|-------------|
| `codegen_declaration_grouping.f90` | Group by type/intent for clean output |
| `codegen_declarations.f90` | Declaration emission facade |
| `codegen_declarations_core.f90` | Core variable, type, parameter emission |
| `codegen_declarations_inference.f90` | Inferred declarations for lazy Fortran |
| `codegen_declarations_procedures.f90` | Procedure declarations |
| `codegen_declarations_programs.f90` | Program declarations |
| `codegen_function_declarations.f90` | Function declaration facade |
| `codegen_function_declarations_part1.inc` | Signature and parameter emission |
| `codegen_function_declarations_part2.inc` | Result type and attribute handling |
| `codegen_module_generation.f90` | Module structure generation |
| `codegen_parameter_info.f90` | Parameter metadata extraction |
| `codegen_parameter_mapping.f90` | Parameter name mapping |
| `codegen_procedure_shared.f90` | Shared procedure utilities |
| `codegen_program_decl_utils.f90` | Program declaration utilities |
| `codegen_subroutine_declarations.f90` | Subroutine declaration emission |
| `codegen_type_inference_utils.f90` | Type inference integration |
| `codegen_type_utils.f90` | Type utility functions |

## Key Concepts

- Keep declaration emission grouped and deterministic for clean diffs.
- Use `.inc` split files to keep facade modules within size limits.

## Dependencies

- Uses indentation and shared helpers from `../api/` and `../utils/`.
- Uses typed AST information provided by the semantic pipeline.

