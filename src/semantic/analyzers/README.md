# Semantic Analyzers

## Purpose

This directory contains specialized semantic analyzers for different AST node types and analysis tasks. Each analyzer focuses on a specific aspect of semantic analysis: expressions, assignments, function calls, arrays, parameters, literals, etc.

For complete semantic analysis concepts including type inference, scope management, and convergence, see [Semantic README](../README.md#key-concepts).

## File Index

### Core Analyzer Infrastructure

| File | Description |
|------|-------------|
| semantic_analyzer.f90 | Main semantic analyzer orchestration |
| semantic_analyzer_base.f90 | Base analyzer interface and shared utilities |
| base_analyzer.f90 | Abstract base for all analyzers |
| analyzer_results.f90 | Result types for analyzer operations |
| semantic_analyzer_context_impl.f90 | Semantic context implementation |
| semantic_analyzer_with_checks.f90 | Analyzer with validation checks |

### Type Inference Implementation

| File | Description |
|------|-------------|
| semantic_analyzer_infer_impl.f90 | Type inference facade (includes three parts) |
| semantic_analyzer_infer_impl_part1.inc | Type inference part 1: expressions |
| semantic_analyzer_infer_impl_part2.inc | Type inference part 2: statements |
| semantic_analyzer_infer_impl_part3.inc | Type inference part 3: declarations |
| semantic_analyzer_infer_helpers.f90 | Type inference helper functions |
| semantic_analyzer_infer_type_locals_part1.inc | Local variable inference part 1 |
| semantic_analyzer_infer_type_locals_part2.inc | Local variable inference part 2 |
| semantic_analyzer_infer_type_locals_part3.inc | Local variable inference part 3 |

### Expression Analysis

| File | Description |
|------|-------------|
| semantic_expression_context.f90 | Expression context tracking |
| semantic_binary_operations.f90 | Binary operator type checking and inference |
| semantic_binary_ops_core.f90 | Core binary operation semantics |
| semantic_literal_identifier.f90 | Literal and identifier type inference |
| semantic_literal_type_helpers.f90 | Literal type helper functions |

### Function and Procedure Analysis

| File | Description |
|------|-------------|
| semantic_function_analysis.f90 | Function semantic analysis orchestration |
| semantic_function_call.f90 | Function call type checking |
| semantic_function_helpers.f90 | Function analysis helper utilities |
| semantic_function_inference.f90 | Function type inference from call sites |
| semantic_call_signature_collector.f90 | Collect function signatures from calls |
| semantic_subroutine_analysis.f90 | Subroutine semantic analysis |
| semantic_parameter_analysis.f90 | Parameter type and intent inference |
| semantic_procedure_utils.f90 | Procedure analysis utilities |

### Array Analysis

| File | Description |
|------|-------------|
| semantic_array_intrinsics.f90 | Array intrinsic function type checking (matmul, reshape, etc.) |
| semantic_array_literal.f90 | Array literal type inference `[1, 2, 3]` |
| semantic_array_slice.f90 | Array slicing type inference `a(1:10, :)` |
| semantic_array_type_builders.f90 | Build array types from element types and shapes |

### Assignment and Variable Analysis

| File | Description |
|------|-------------|
| semantic_assignment_inference.f90 | Assignment statement type inference |
| semantic_identifier_context.f90 | Identifier usage context tracking |
| semantic_undefined_variable_checker.f90 | Detect usage of undefined variables |
| semantic_walrus_checker.f90 | Detect same-scope redeclaration via walrus `:=` |
| semantic_pure_validation.f90 | Enforce PURE/ELEMENTAL body restrictions (no I/O, STOP, PAUSE) |
| semantic_bind_c_validation.f90 | Enforce BIND(C) interoperability constraints on derived-type components and procedure dummies (F2003 15.3.2) |
| semantic_elemental_validation.f90 | Enforce ELEMENTAL scalar-dummy restriction (no array dummies) |
| semantic_implied_do_validation.f90 | Enforce implied-DO index locality in array constructors (no self-reference in bounds, no shadowing nested index) |
| semantic_declaration_utils.f90 | Declaration processing utilities |

### Type Operations

| File | Description |
|------|-------------|
| semantic_type_operations.f90 | Type system operations (unify, substitute, generalize) |
| semantic_analyzer_type_ops_impl.f90 | Type operation implementations |
| semantic_type_context.f90 | Type context management |
| semantic_type_lookup_wrapper.f90 | Type lookup utilities |

### Scope and Validation

| File | Description |
|------|-------------|
| semantic_scope_creation.f90 | Create and manage scopes |
| semantic_explicit_interface_checker.f90 | Strict-mode explicit interface validation for procedure calls |
| semantic_strict_argument_type_checker.f90 | Strict-mode validation for actual vs dummy argument type matching |
| semantic_strict_argument_type_checker_resolution.f90 | Strict-mode procedure interface resolution helpers |
| semantic_strict_argument_type_checker_types.f90 | Strict-mode actual/dummy argument type extraction and comparison |
| semantic_strict_argument_type_checker_validation.f90 | Strict-mode call validation, argument mapping, and type checks |
| semantic_strict_argument_type_checker_scope_utils.f90 | Helpers for strict-mode scoped procedure interface lookup |
| semantic_validation_utils.f90 | Validation helper utilities |
| semantic_annotation_utils.f90 | AST annotation with type information |
| semantic_constant_values.f90 | Constant value analysis |

## Key Concepts

**Analyzer Dispatch**
- Main analyzer dispatches to specialized analyzers based on node type
- Each specialized analyzer handles specific syntactic category
- Results aggregated and combined in semantic context

**Multi-Pass Analysis**
- First pass: Gather declarations, build symbol tables
- Second pass: Infer types from usage
- Third pass: Propagate types through call graph
- Iterate until convergence or max iterations

**Expression Type Inference**
- Literals: Immediate type known (`5` → `integer`, `3.14` → `real`)
- Identifiers: Look up in scope stack
- Binary operations: Type promotion rules (e.g., `integer + real` → `real`)
- Function calls: Query function signature or infer from call

**Assignment Type Inference**
- Forward inference: `x = 5` → `x` is `integer`
- Backward inference: If `x` declared as `real`, `5` coerced to `real`
- Unification: Both sides must have compatible types

**Function Type Inference**
- Collect all call sites from call graph
- Extract argument types at each call
- Unify types across all calls
- Infer return type from result expressions
- Handle polymorphism via specialization

**Array Type Inference**
- Rank inference: Count indices in array access
- Shape inference: From array constructors and allocations
- Element type inference: From literals or usage
- Intrinsic functions: Type rules for matmul, transpose, etc.

**Parameter Intent Inference**
- **intent(in)**: Only read in procedure body
- **intent(out)**: Only written in procedure body
- **intent(inout)**: Both read and written
- Analyze variable usage to determine intent

## Dependencies

**Semantic Core**
- `semantic/scope_manager` - Scope and symbol management
- `semantic/types/` - Type system

**AST**
- `ast/` - AST nodes and traversal

**Analysis**
- `analysis/call_graph` - Call site information
- `analysis/variable_usage` - Variable read/write patterns

**Common Utilities**
- `common/identifier_table` - Identifier management
