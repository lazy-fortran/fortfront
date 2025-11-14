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

**Visitor Pattern**
- Callbacks receive node reference, never copy nodes
- Type-safe dispatch based on node kind
- Visitor state maintained across traversal
- Supports early termination and conditional traversal

**Traversal Orders**
- **Pre-order**: Visit parent before children (useful for scope setup)
- **Post-order**: Visit children before parent (useful for type inference)
- **Level-order**: Visit nodes level-by-level (useful for dependency analysis)

**Safe Node Access**
- All access via `visit_node_at(arena, index, visitor)`
- Arena reference ensures valid memory access
- Index-based references prevent dangling pointers
- No manual pointer management required

**Visitor Interface**
```fortran
type :: visitor_t
    procedure(visit_node), pointer :: visit => null()
end type

interface
    subroutine visit_node(visitor, arena, node_index)
        import :: visitor_t, ast_arena_t
        type(visitor_t), intent(inout) :: visitor
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
    end subroutine
end interface
```

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
