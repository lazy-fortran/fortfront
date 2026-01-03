# Code Generation

## Purpose

The code generation subsystem transforms typed ASTs back into standard Fortran source code. This is the final stage of the lazy Fortran transformation pipeline and also supports round-trip validation for standard Fortran. The codegen emits properly formatted, idiomatic Fortran with inferred declarations, proper indentation, and correct program structure.

**Important**: This codegen emits **standard Fortran source**, not LLVM IR. LLVM HLIR emission is handled by the separate `ffc` compiler which uses fortfront as a library.

## File Index

### Directory Layout

| Directory | Description |
|-----------|-------------|
| `api/` | Public codegen API and orchestration entry points |
| `arena/` | Arena node access helpers for safe traversal |
| `decls/` | Declaration emission, type/parameter utilities, module/program decl helpers |
| `expressions/` | Expression emission (with `.inc` parts) |
| `programs/` | Program/module structure emission and grouped body specialization support |
| `statements/` | Statement and control-flow emission (with `.inc` parts) |
| `utils/` | Shared helpers (indent, name mangling, character handling, import reorder) |

### Core Generation (3 files)

| File | Description |
|------|-------------|
| `api/codegen_api.f90` | Public API facade for code generation |
| `api/codegen_core.f90` | Core orchestration, main generation entry point |
| `api/codegen_indent.f90` | Indentation management and pretty-printing |

### Arena Interface (2 files)

| File | Description |
|------|-------------|
| `arena/codegen_arena_interface.f90` | Arena memory interface for safe node access |
| `arena/codegen_arena_utils.f90` | Arena-based utilities for node traversal |

### Declarations (7 files)
Emission of variable, type, and procedure declarations with grouping and inference support.

| File | Description |
|------|-------------|
| `decls/codegen_declarations.f90` | Declaration emission facade |
| `decls/codegen_declarations_core.f90` | Core variable, type, parameter emission |
| `decls/codegen_declarations_inference.f90` | Inferred declarations for lazy Fortran |
| `decls/codegen_declarations_procedures.f90` | Procedure declarations |
| `decls/codegen_declarations_programs.f90` | Program declarations |
| `decls/codegen_declaration_grouping.f90` | Group by type/intent for clean output |
| `decls/codegen_procedure_shared.f90` | Shared procedure utilities |

### Expressions (3 files)
Emission of expressions, operators, literals, function calls, and array indexing.

| File | Description |
|------|-------------|
| `expressions/codegen_expressions.f90` | Expression emission facade |
| `expressions/codegen_expressions_part1.inc` | Binary/unary ops, literals |
| `expressions/codegen_expressions_part2.inc` | Function calls, array indexing |

### Statements (3 files)
Emission of executable statements including assignments, I/O, and control flow.

| File | Description |
|------|-------------|
| `statements/codegen_statements.f90` | Statement emission facade |
| `statements/codegen_statements_part1.inc` | Assignments, print, read, write |
| `statements/codegen_statements_part2.inc` | Return, continue, stop, cycle |

### Control Flow (1 file)

| File | Description |
|------|-------------|
| `statements/codegen_control_flow.f90` | If/elseif/else, select, where constructs |

### Function & Subroutine Declarations (4 files)
Procedure signature emission with parameters, result types, and attributes.

| File | Description |
|------|-------------|
| `decls/codegen_function_declarations.f90` | Function declaration facade |
| `decls/codegen_function_declarations_part1.inc` | Signature and parameter emission |
| `decls/codegen_function_declarations_part2.inc` | Result type and attribute handling |
| `decls/codegen_subroutine_declarations.f90` | Subroutine declaration emission |

### Program & Module Structure (9 files)
Program and module structure generation with headers, bodies, and variables.

| File | Description |
|------|-------------|
| `programs/codegen_program_generation.f90` | Program structure generation |
| `programs/codegen_program_header.f90` | Program/module/function statements |
| `programs/codegen_program_body.f90` | Statement emission and internal procedures |
| `programs/codegen_program_variables.f90` | Include wrapper for variable declaration sections |
| `programs/codegen_program_variables_collect.inc` | Collect declarations and uses |
| `programs/codegen_program_variables_analysis.inc` | Infer used identifiers and types |
| `programs/codegen_program_variables_emit.inc` | Emit declaration blocks |
| `decls/codegen_program_decl_utils.f90` | Program declaration utilities |
| `decls/codegen_module_generation.f90` | Module structure generation |

### Monomorphization Support (6 files)
Parameter grouping, name mangling, and specialization support.

| File | Description |
|------|-------------|
| `programs/codegen_grouped_body.f90` | Group procedure bodies by specialization |
| `programs/codegen_grouped_body_params.f90` | Parameter grouping |
| `programs/codegen_grouped_body_params_helpers.f90` | Parameter grouping helpers |
| `decls/codegen_parameter_info.f90` | Parameter metadata extraction |
| `decls/codegen_parameter_mapping.f90` | Parameter name mapping |
| `utils/codegen_name_mangling.f90` | Name mangling for monomorphization |

### Type Utilities (4 files)
Type handling, character types, and type inference integration.

| File | Description |
|------|-------------|
| `decls/codegen_type_utils.f90` | Type utility functions |
| `decls/codegen_type_inference_utils.f90` | Type inference integration |
| `utils/codegen_character_types.f90` | Character type handling |
| `utils/codegen_character_normalization.f90` | Character length normalization |

### Specialized Utilities (4 files)

| File | Description |
|------|-------------|
| `utils/codegen_basic_utils.f90` | Basic string utilities and formatting helpers |
| `utils/codegen_import_reorder.f90` | Reorder use/import statements |
| `utils/codegen_entry_utils.f90` | Entry statement handling (legacy Fortran) |
| `statements/codegen_loop_vars_mod.f90` | Loop variable tracking and emission |

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
- See `docs/guides/CHARACTER_TYPE_GUIDE.md`

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
