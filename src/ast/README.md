# AST (Abstract Syntax Tree)

## Purpose

The AST subsystem provides the core data structures and operations for representing parsed Fortran programs (both standard and lazy Fortran). It implements arena-based memory management for safe, efficient AST node allocation without manual deallocation. The AST is the central data structure used by all compiler phases: parsing, semantic analysis, code generation, and tool integration.

**Critical Design Principle**: AST nodes MUST NOT be copied. All operations use the visitor pattern with node indices and arena references.

## Directory Structure

- `nodes/` - AST node type definitions
- `traversal/` - Visitor pattern implementation and traversal utilities
- `arena/` - Arena-based memory management
- `factory/` - Factory methods for creating AST nodes

## File Index

| File | Description |
|------|-------------|
| ast_base.f90 | Base types and constants for AST nodes |
| ast_error_nodes.f90 | Error node types for parse error recovery |
| ast_introspection.f90 | Runtime introspection of AST node properties |
| ast_traversal_utils.f90 | Utilities for AST traversal patterns |
| ast_types.f90 | Core AST type definitions |

## Key Concepts

**Arena-Based Allocation**
- All AST nodes allocated in contiguous arena memory
- No manual `deallocate` required - arena cleanup handles everything
- Stack-like allocation pattern with automatic scope cleanup
- Reduces memory fragmentation and improves cache locality

**Visitor Pattern**
- Safe traversal via `visit_node_at(arena, index, visitor_callback)`
- Visitor receives node reference, never copies
- Supports pre-order and post-order traversal
- Type-safe dispatch to node-specific handlers

**Node Type Organization**
- Core nodes: Programs, modules, procedures, expressions
- Control flow: If/select/where constructs
- Loops: Do, do-while, implied-do
- Data: Arrays, derived types, parameters
- I/O: Read, write, print, format
- Bounds: Array bounds and specifications

**Safe Access Patterns**
- Node indices (not pointers) for referencing nodes
- Arena reference required for all node access
- Prevents dangling pointer issues
- Enables safe concurrent traversal

**Memory Safety**
- Arena prevents use-after-free
- Index-based access prevents dangling pointers
- Automatic lifetime management
- See `src/memory/README.md` for current arena allocation details

## Dependencies

**Memory Management**
- `memory/arena_memory` - General arena allocator
- `memory/compiler_arena` - Compiler-wide allocation context

**Common Utilities**
- `common/identifier_table` - String interning for identifiers
- `common/uid_generator` - Unique node IDs

**External**
- `stdlib` - Fortran standard library
