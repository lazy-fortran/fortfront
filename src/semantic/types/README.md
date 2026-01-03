# Semantic Types

## Purpose

This directory implements the type system for semantic analysis: type representations (mono types, poly types), type operations (unification, substitution), type checking, and type arena allocation. The type system supports both concrete types (for standard Fortran) and polymorphic types (for generic lazy Fortran constructs).

## File Index

| File | Description |
|------|-------------|
| type_system_unified.f90 | Unified type system: mono types, poly types, type variables |
| type_system_arena.f90 | Arena-based type allocation for efficient memory management |
| type_checker.f90 | Type compatibility checking and validation |
| type_constants.f90 | Constants for built-in types (integer, real, character, logical) |
| type_array_safe.f90 | Safe array type operations with bounds checking |
| type_utils_safe.f90 | Safe type utility functions |
| semantic_context_types.f90 | Types for semantic context (scope, type environment) |
| semantic_result_types.f90 | Result types for semantic operations |

## Key Concepts

For complete type system design and type inference concepts, see [Semantic README](../README.md#key-concepts) and [docs/guides/TYPE_SAFETY_GUIDE.md](../../../docs/guides/TYPE_SAFETY_GUIDE.md).

**This Directory's Specifics**:
- **Type representations**: Mono types (concrete), poly types (generic), type variables
- **Type operations**: Unification, substitution, compatibility checking
- **Type arena**: Efficient type allocation and management
- **Type constants**: Built-in types (integer, real, character, logical)

**Key Files**:
- `type_system_unified.f90` - Core type system
- `type_checker.f90` - Type compatibility and validation
- `type_system_arena.f90` - Arena-based type allocation
- Separate environment for each scope
- Nested environments for nested scopes
- Supports shadowing and lookup

**Type Arena Allocation**
- Types allocated in arena (like AST nodes)
- No manual deallocation required
- Efficient allocation for temporary types during inference
- Automatic cleanup on scope exit

**Array Types**
- Element type + rank (dimensionality)
- Example: `integer(4) array(rank=2)` - 2D integer array
- Shape information optional (can be inferred)
- Support for assumed-shape, assumed-size, explicit-shape

**Function Types**
- Parameter types + result type
- Example: `(integer, integer) -> integer` (binary function)
- Support for optional parameters
- Intent information attached to parameters

**Type Checking**
- Verify type compatibility in assignments
- Check procedure call argument types
- Validate array operations
- Report type mismatches with context

**Type Promotion**
- Automatic promotion for mixed-type expressions
- `integer + real` → `real` (integer promoted to real)
- `integer(4) + integer(8)` → `integer(8)` (promote to larger kind)
- Character concatenation preserves kind

## Dependencies

**Memory Management**
- `memory/arena_memory` - Arena allocation for types

**Common Utilities**
- `common/identifier_table` - Identifier management for type names

**Error Reporting**
- `error_handling` - Type error reporting
