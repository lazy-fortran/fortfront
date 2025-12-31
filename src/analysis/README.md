# Analysis

## Purpose

The analysis subsystem provides program-wide structural analysis capabilities that operate on complete, type-checked ASTs. This includes call graph construction, variable usage tracking, and procedure signature analysis. These analyses form the foundation for type inference in lazy Fortran and provide essential information for tools like linters, compilers, and language servers.

The analysis phase operates **after** semantic analysis and works with fully type-checked code, distinguishing it from semantic analysis which focuses on type inference and validation.

## File Index

| File | Description |
|------|-------------|
| call_graph.f90 | Public API facade for call graph functionality |
| call_graph_core_mod.f90 | Core call graph data structures and types |
| call_graph_builder_mod.f90 | Primary call graph construction from AST |
| call_graph_builder_state_mod.f90 | State management during call graph construction |
| call_graph_builder_postprocess_mod.f90 | Post-processing passes (unused procedure detection, cycle detection) |
| call_graph_constants_mod.f90 | Constants for call graph operations |
| call_graph_queries_mod.f90 | Query interface for call graph information |
| call_graph_signatures_mod.f90 | Procedure signature extraction and comparison |
| variable_usage_core.f90 | Core types for variable usage tracking |
| variable_usage_tracker.f90 | Variable usage tracking implementation |
| variable_usage_dispatcher.f90 | Facade module for variable usage dispatching |
| variable_usage_control_handlers.f90 | Handlers for control flow nodes (if, do, select, where, forall) |
| variable_usage_stmt_handlers.f90 | Handlers for statement nodes (call, I/O, allocate, procedure defs) |
| variable_usage_expr_handlers.f90 | Handlers for expression nodes (binary_op, call_or_subscript, assignment) |

## Key Concepts

**Call Graph Construction**
- Tracks all function and subroutine calls throughout the program
- Detects unused procedures and call cycles
- Extracts procedure signatures for type inference
- Handles internal procedures and nested scopes

**Variable Usage Analysis**
- Tracks which variables are referenced where
- Distinguishes between definitions and uses
- Supports scope-aware variable tracking
- Provides foundation for dataflow analysis

**Signature-Based Analysis**
- Extracts type signatures from procedure calls
- Supports type inference by analyzing call patterns
- Enables cross-procedure type propagation
- Handles polymorphic function specialization

**Separation from Semantic Analysis**
- Semantic: Type inference, scope resolution, validation
- Analysis: Program structure, call patterns, usage tracking
- Analysis operates on **complete** AST after semantic passes
- Provides program-wide structural information

## Dependencies

**AST Infrastructure**
- `ast/` - AST node types, traversal, arena allocation
- `ast/traversal/` - Visitor pattern for AST traversal

**Semantic Context**
- `semantic/` - Type information, scope manager
- `semantic/types/` - Type system for signature analysis

**Common Utilities**
- `common/identifier_table` - Identifier management
- `common/uid_generator` - Unique ID generation
