# Semantic Analysis

## Purpose

The semantic analysis subsystem performs type inference, scope resolution, and validation on the AST. For lazy Fortran, it infers missing type information from usage patterns. For standard Fortran, it validates declared types against usage. Semantic analysis is the core of fortfront's transformation capability, enabling the conversion of minimally-typed code to fully-typed standard Fortran.

## Directory Structure

- `analyzers/` - Semantic analyzers for different AST node types
- `types/` - Type system implementation (mono_type, poly_type, type unification)

## File Index

| File | Description |
|------|-------------|
| semantic_api.f90 | Public API facade for semantic analysis |
| scope_manager.f90 | Scope stack management, symbol table, variable declarations |
| semantic_inference_helpers.f90 | Type inference helper functions |
| semantic_input_mode.f90 | Input mode enum (lazy vs standard Fortran) |
| semantic_operating_mode.f90 | Operating mode selection (strict vs infer) |
| symbol_table_api.f90 | Public API for the symbol table |
| semantic_unsigned_integer_mix_diagnostics.f90 | Shared diagnostics for signed/unsigned integer mixing |
| type_hierarchy.f90 | Type hierarchy and subtype relationships |
| constant_transformation.f90 | Constant folding and compile-time evaluation |

## Key Concepts

**Type Inference (Lazy Fortran)**
- **From literals**: `x = 5` → `x` is `integer`
- **From call sites**: `add(5, 3)` → `add` parameters are `integer`
- **From expressions**: `y = x + 1` → `y` has same type as `x`
- **From intrinsics**: `sin(x)` → `x` must be `real`
- **Multi-pass**: Iterate until types converge (or max iterations reached)

**Type Validation (Standard Fortran)**
- Verify declared types match usage
- Check type compatibility in assignments
- Ensure procedure calls have correct argument types
- Validate array indexing and slicing
- Detect type errors and report clearly

**Scope Management**
- **Scope stack**: Track nested scopes (program → module → procedure → block)
- **Symbol table**: Map identifiers to declarations in each scope
- **Shadowing**: Inner scopes can shadow outer scopes
- **Host association**: Internal procedures access parent's variables

**Type System**
- **Mono types**: Concrete types (e.g., `integer(4)`, `real(8)`, `character(len=10)`)
- **Poly types**: Generic types with type variables (e.g., `forall a. a -> a`)
- **Type unification**: Solve type equations to infer concrete types
- **Type substitution**: Apply inferred types throughout AST

**Semantic Context**
- Holds scope stack, type environment, identifier table
- Threaded through all analysis passes
- Incremental type refinement (multiple passes)
- Maintains consistency across AST

**Convergence**
- Type inference may require multiple passes
- Each pass refines type information
- Converges when no new type information discovered
- Maximum iteration limit prevents infinite loops

**Array Type Inference**
- Infer array rank from indexing: `a(i, j)` → rank 2
- Infer element type from literals: `[1, 2, 3]` → `integer` array
- Propagate array types through expressions
- Handle array slicing: `a(1:10, :)` → rank 2 slice

**Procedure Type Inference**
- Build call graph to collect call sites
- Infer parameter types from arguments at each call
- Unify types across multiple call sites
- Infer result type from return expressions
- Infer intent from usage (read vs write vs both)

## Dependencies

**AST**
- `ast/` - AST nodes and traversal

**Analysis**
- `analysis/call_graph` - Call graph for procedure type inference
- `analysis/variable_usage` - Variable usage for intent inference

**Common Utilities**
- `common/identifier_table` - Identifier management
- `common/declaration_attribute_utils` - Attribute handling

**Error Reporting**
- `error_handling` - Semantic error reporting
- `error_reporting` - Structured error messages
