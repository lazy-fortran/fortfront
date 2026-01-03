# Standardizers

## Purpose

The standardizers subsystem transforms lazy Fortran ASTs into standard Fortran ASTs by adding inferred declarations, fixing intents, inserting program structure, and handling monomorphization. This is the bridge between semantic analysis (which infers types) and code generation (which emits standard Fortran).

Standardizers modify the AST in-place to make it fully conformant with standard Fortran requirements.

## File Index

### Core Orchestration (3 files)

| File | Description |
|------|-------------|
| `standardizer.f90` | Public facade for standardization operations |
| `standardizer_core.f90` | Core standardization orchestration |
| `standardizer_driver.f90` | Main driver coordinating multi-pass execution |

### Type Standardization (3 files)
Transform type declarations to standard Fortran including derived types.

| File | Description |
|------|-------------|
| `standardizer_types.f90` | Type standardization facade |
| `standardizer_types_part1.inc` | Type declarations |
| `standardizer_types_part2.inc` | Derived types |

### Program & Module Structure (3 files)
Add program/module wrappers to bare statements and procedures.

| File | Description |
|------|-------------|
| `standardizer_program.f90` | Program structure (wrap bare statements) |
| `standardizer_module.f90` | Module structure standardization |
| `standardizer_wrapping.f90` | Wrap bare code in program structure |

### Function Standardization (5 files)
Add result types, infer intents, and standardize parameters.

| File | Description |
|------|-------------|
| `standardizer_function.f90` | Function standardization |
| `standardizer_function_parameters.f90` | Parameter standardization |
| `standardizer_function_parameter_builders.f90` | Build parameter declarations |
| `standardizer_function_param_scanner.f90` | Scan parameters for inference |
| `standardizer_function_result_utils.f90` | Result type utilities |

### Subroutine Standardization (2 files)
Infer and insert intent attributes for subroutine parameters.

| File | Description |
|------|-------------|
| `standardizer_subroutine.f90` | Subroutine standardization |
| `standardizer_subroutine_intent.f90` | Intent inference and insertion |

### Subprogram & Parameter Handling (2 files)

| File | Description |
|------|-------------|
| `standardizer_parameter.f90` | Parameter declaration standardization |
| `standardizer_subprograms.f90` | Subprogram standardization utilities |

### Declarations (9 files)
Infer, collect, and insert variable declarations into the AST.

| File | Description |
|------|-------------|
| `standardizer_declarations.f90` | Declaration standardization facade |
| `standardizer_declarations_core.f90` | Core declaration logic |
| `standardizer_declarations_collection.f90` | Collect declarations |
| `standardizer_declarations_insertion.f90` | Insert declarations into AST |
| `standardizer_declarations_inference.f90` | Inference from semantic context |
| `standardizer_declarations_parsing.f90` | Parse existing declarations |
| `standardizer_declarations_variables.f90` | Variable declarations |
| `standardizer_declarations_array.f90` | Array declarations |
| `standardizer_declarations_state.f90` | State management |

### Attribute Handling (3 files)

| File | Description |
|------|-------------|
| `standardizer_allocatable.f90` | Allocatable attribute handling |
| `standardizer_pointer_targets.f90` | Pointer and target attributes |
| `standardizer_interface_utils.f90` | Interface block utilities |

### Monomorphization (4 files)
Generate type-specialized procedure variants and rewrite call sites.

| File | Description |
|------|-------------|
| `ast_monomorphization.f90` | Monomorphization facade |
| `ast_monomorphization_part1.inc` | Signature analysis |
| `ast_monomorphization_part2.inc` | Specialization generation |
| `ast_monomorphization_part3.inc` | Call site rewriting |

## Key Concepts

**Program Structure Standardization**
- **Bare statements**: Wrap in `program main ... end program`
- **Procedures only**: Keep as standalone procedures (no wrapping)
- **Modules**: Preserve module structure
- **Complete programs**: Use existing structure

**Declaration Inference and Insertion**
- Query semantic context for inferred types
- Generate declaration nodes for untyped variables
- Insert declarations at procedure boundaries
- Group declarations by type for clean output

**Intent Inference**
- Analyze variable usage in procedure body
- **Read only** → `intent(in)`
- **Write only** → `intent(out)`
- **Read and write** → `intent(inout)`
- Insert intent attributes into parameter declarations

**Result Type Standardization**
- Functions must have explicit result type
- Implicit result: Use function name as result variable
- Explicit result: Separate result variable
- Type prefix: Move type to function signature

**Allocatable and Pointer Handling**
- Infer `allocatable` for dynamically sized arrays
- Infer `pointer` for pointer assignments (`x => target`)
- Infer `target` for pointer targets
- Validate attribute compatibility

**Monomorphization**
- Generate type-specialized versions of generic procedures
- Name mangling: `add__i32_i32`, `add__f64_f64`
- Rewrite call sites to use specialized names
- Use Fortran generic interfaces (standard Fortran)
- See `docs/architecture/MONOMORPHIZATION.md` for detailed strategy

**Multi-Pass Standardization**
1. **Structure pass**: Add program/module wrappers
2. **Declaration pass**: Insert inferred declarations
3. **Intent pass**: Add intent attributes
4. **Monomorphization pass**: Generate specializations
5. **Cleanup pass**: Remove temporary annotations

**AST Modification**
- Standardizers modify AST in-place
- Add new nodes for declarations
- Update existing nodes with attributes
- Maintain AST validity throughout

**Standard Fortran Compliance**
- `implicit none` at every procedure boundary
- All variables explicitly declared
- All parameters have intent (except functions)
- All functions have explicit result type
- No lazy Fortran constructs remain

## Dependencies

**Semantic Context**
- `semantic/` - Type information for declarations
- `semantic/types/` - Type system for inferred types

**AST**
- `ast/` - AST nodes and traversal
- `ast/factory/` - Create new declaration nodes

**Analysis**
- `analysis/variable_usage` - Intent inference
- `analysis/call_graph` - Monomorphization call site analysis

**Codegen**
- `codegen/codegen_name_mangling` - Name mangling for monomorphization

**Common Utilities**
- `common/identifier_table` - Identifier management
- `common/declaration_attribute_utils` - Attribute handling
