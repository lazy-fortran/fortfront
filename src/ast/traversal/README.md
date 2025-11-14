# AST Traversal

## Purpose

This directory provides the visitor pattern implementation for safe AST traversal. Instead of copying or directly accessing nodes, all traversal uses visitor callbacks with node references. This ensures memory safety, prevents node duplication, and provides a consistent traversal interface for all compiler phases.

Supports multiple traversal orders (pre-order, post-order, level-order) and type-safe dispatch to node-specific visitor methods.

## File Index

| File | Description |
|------|-------------|
| ast_traversal.f90 | High-level traversal orchestration, traversal order control, node enumeration |
| ast_visitor.f90 | Visitor pattern implementation, type-safe dispatch, visitor callbacks |

## Key Concepts

For complete visitor pattern design, see [AST README](../README.md#key-concepts).

**This Directory's Specifics**:
- **ast_traversal.f90**: High-level orchestration, traversal order control (pre/post/level-order)
- **ast_visitor.f90**: Visitor pattern implementation, type-safe dispatch, callbacks

**Traversal Orders**:
- **Pre-order**: Parent before children (scope setup)
- **Post-order**: Children before parent (type inference)
- **Level-order**: Level-by-level (dependency analysis)

**Usage**: `call visit_node_at(arena, index, visitor)` - safe, index-based, no pointer management

**Traversal Utilities**
- Child node enumeration
- Sibling traversal
- Subtree extraction
- Path tracking (for error reporting)

**Use Cases**
- **Semantic Analysis**: Post-order for type inference (children before parents)
- **Code Generation**: Pre-order for declaration emission (parents before children)
- **Linting**: Any order for pattern matching
- **Optimization**: Multiple passes with different orders

## Dependencies

**AST Core**
- `ast/ast_base` - Base node types
- `ast/ast_types` - AST type definitions
- `ast/arena/` - Arena memory management
- `ast/nodes/` - All node type definitions

**Common Utilities**
- `common/identifier_table` - Identifier lookups during traversal
