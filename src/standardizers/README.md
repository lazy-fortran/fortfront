# Standardizers

## Purpose

The standardizers subsystem transforms lazy Fortran ASTs into standard Fortran ASTs by adding inferred declarations, fixing intents, inserting program structure, and handling monomorphization. This is the bridge between semantic analysis (which infers types) and code generation (which emits standard Fortran).

Standardizers modify the AST in-place to make it fully conformant with standard Fortran requirements.

## File Index

| File | Description |
|------|-------------|
| standardizer.f90 | Public facade for standardization operations |
| standardizer_core.f90 | Core standardization orchestration |
| standardizer_driver.f90 | Main standardization driver, multi-pass coordination |
| standardizer_types.f90 | Type standardization facade (includes two parts) |
| standardizer_types_part1.inc | Type standardization part 1: type declarations |
| standardizer_types_part2.inc | Type standardization part 2: derived types |
| standardizer_program.f90 | Program structure standardization (wrap bare statements) |
| standardizer_module.f90 | Module structure standardization |
| standardizer_function.f90 | Function standardization (add result type, intents) |
| standardizer_function_parameters.f90 | Function parameter standardization |
| standardizer_function_parameter_builders.f90 | Build standardized parameter declarations |
| standardizer_function_param_scanner.f90 | Scan function parameters for inference |
| standardizer_function_result_utils.f90 | Function result type utilities |
| standardizer_subroutine.f90 | Subroutine standardization (add intents) |
| standardizer_subroutine_intent.f90 | Subroutine intent inference and insertion |
| standardizer_parameter.f90 | Parameter declaration standardization |
| standardizer_subprograms.f90 | Subprogram standardization utilities |
| standardizer_wrapping.f90 | Wrap bare code in program structure |
| standardizer_declarations.f90 | Declaration standardization facade |
| standardizer_declarations_core.f90 | Core declaration standardization logic |
| standardizer_declarations_collection.f90 | Collect declarations for insertion |
| standardizer_declarations_insertion.f90 | Insert inferred declarations into AST |
| standardizer_declarations_inference.f90 | Declaration inference from semantic context |
| standardizer_declarations_parsing.f90 | Parse existing declarations |
| standardizer_declarations_variables.f90 | Variable declaration standardization |
| standardizer_declarations_array.f90 | Array declaration standardization |
| standardizer_declarations_state.f90 | State management for declaration processing |
| standardizer_allocatable.f90 | Allocatable attribute handling |
| standardizer_pointer_targets.f90 | Pointer and target attribute handling |
| standardizer_interface_utils.f90 | Interface block utilities |
| ast_monomorphization.f90 | Monomorphization facade (includes three parts) |
| ast_monomorphization_part1.inc | Monomorphization part 1: signature analysis |
| ast_monomorphization_part2.inc | Monomorphization part 2: specialization generation |
| ast_monomorphization_part3.inc | Monomorphization part 3: call site rewriting |

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
- See `docs/MONOMORPHIZATION.md` for detailed strategy

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
