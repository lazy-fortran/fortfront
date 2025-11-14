# Code Generation

## Purpose

The code generation subsystem transforms typed ASTs back into standard Fortran source code. This is the final stage of the lazy Fortran transformation pipeline and also supports round-trip validation for standard Fortran. The codegen emits properly formatted, idiomatic Fortran with inferred declarations, proper indentation, and correct program structure.

**Important**: This codegen emits **standard Fortran source**, not LLVM IR. LLVM HLIR emission is handled by the separate `ffc` compiler which uses fortfront as a library.

## File Index

| File | Description |
|------|-------------|
| codegen_api.f90 | Public API facade for code generation |
| codegen_core.f90 | Core orchestration, main generation entry point |
| codegen_arena_interface.f90 | Arena memory interface for safe node access |
| codegen_arena_utils.f90 | Arena-based utilities for node traversal |
| codegen_basic_utils.f90 | Basic string utilities, formatting helpers |
| codegen_indent.f90 | Indentation management, pretty-printing |
| codegen_declarations.f90 | Declaration emission facade |
| codegen_declarations_core.f90 | Core declaration emission: variables, types, parameters |
| codegen_declarations_inference.f90 | Inferred declaration generation for lazy Fortran |
| codegen_declarations_procedures.f90 | Procedure declaration emission |
| codegen_declarations_programs.f90 | Program declaration emission |
| codegen_declaration_grouping.f90 | Group declarations by type/intent for clean output |
| codegen_expressions.f90 | Expression emission facade (includes two parts below) |
| codegen_expressions_part1.inc | Expression emission part 1: binary ops, unary ops, literals |
| codegen_expressions_part2.inc | Expression emission part 2: function calls, array indexing |
| codegen_statements.f90 | Statement emission facade (includes two parts below) |
| codegen_statements_part1.inc | Statement emission part 1: assignments, print, read, write |
| codegen_statements_part2.inc | Statement emission part 2: return, continue, stop, cycle |
| codegen_control_flow.f90 | Control flow emission: if/elseif/else, select, where |
| codegen_function_declarations.f90 | Function declaration facade (includes two parts) |
| codegen_function_declarations_part1.inc | Function declaration part 1: signature, parameters |
| codegen_function_declarations_part2.inc | Function declaration part 2: result type, attributes |
| codegen_subroutine_declarations.f90 | Subroutine declaration emission |
| codegen_procedure_shared.f90 | Shared procedure utilities (intent inference, attribute handling) |
| codegen_program_generation.f90 | Program structure generation |
| codegen_program_header.f90 | Program header emission: program/module/function statement |
| codegen_program_body.f90 | Program body emission: statements, internal procedures |
| codegen_program_variables.f90 | Variable declaration section generation |
| codegen_program_decl_utils.f90 | Program declaration utilities |
| codegen_module_generation.f90 | Module structure generation |
| codegen_grouped_body.f90 | Group procedure bodies by specialization |
| codegen_grouped_body_params.f90 | Parameter grouping for monomorphization |
| codegen_grouped_body_params_helpers.f90 | Helper utilities for parameter grouping |
| codegen_parameter_info.f90 | Parameter metadata extraction |
| codegen_parameter_mapping.f90 | Parameter name mapping for specializations |
| codegen_name_mangling.f90 | Name mangling for monomorphized procedures |
| codegen_character_types.f90 | Character type handling |
| codegen_character_normalization.f90 | Character length normalization |
| codegen_type_utils.f90 | Type utility functions |
| codegen_type_inference_utils.f90 | Type inference integration utilities |
| codegen_import_reorder.f90 | Reorder use/import statements for dependencies |
| codegen_entry_utils.f90 | Entry statement handling (legacy Fortran) |
| codegen_loop_vars_mod.f90 | Loop variable tracking and emission |

## Key Concepts

**Emission Pipeline**
1. **Program Structure**: Emit program/module/function wrapper
2. **Declarations**: Group and emit variable/type declarations
3. **Body**: Emit executable statements
4. **Internal Procedures**: Emit contains section with internal procedures
5. **Formatting**: Apply indentation and spacing rules

**Declaration Grouping**
- Group variables by type and intent for clean output
- Emit `implicit none` at procedure boundaries
- Handle inferred vs. explicit declarations differently
- Respect parameter attributes and fixed-size arrays

**Type Inference Integration**
- Query semantic context for inferred types
- Generate declarations for untyped variables (lazy Fortran)
- Preserve explicit declarations (standard Fortran)
- Handle monomorphized procedure variants

**Name Mangling**
- Format: `<name>__<kind1>_<kind2>`
- Example: `add__i32_i32` for `integer(4) add(integer(4), integer(4))`
- Deterministic to avoid collisions
- Only applied to lazy Fortran specializations

**Indentation Management**
- Track indentation level throughout emission
- Indent body statements relative to structure
- Handle nested structures (if within do, etc.)
- Configurable indentation width (default 4 spaces)

**Character Handling**
- Normalize character lengths (`*` to explicit length)
- Handle assumed-length parameters
- Preserve character kind parameters
- See `docs/CHARACTER_TYPE_GUIDE.md`

**Round-Trip Validation**
- Standard Fortran: parse → AST → codegen → parse again
- Output should match input (modulo formatting)
- Used to validate parser correctness
- Examples in `examples/f90/`

## Dependencies

**AST Infrastructure**
- `ast/` - AST nodes, traversal, arena
- `ast/nodes/` - All node type definitions

**Semantic Context**
- `semantic/` - Type information for declarations
- `semantic/types/` - Type system for emission

**Analysis**
- `analysis/call_graph` - Procedure signature information

**Common Utilities**
- `common/identifier_table` - Identifier string retrieval
- `utilities/` - String utilities, formatting
