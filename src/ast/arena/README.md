# AST Arena

## Purpose

This directory provides arena-based memory management for AST nodes. Arena allocation eliminates manual deallocation, prevents memory fragmentation, and enables safe concurrent traversal. All AST nodes are allocated in contiguous memory blocks with automatic cleanup on scope exit.

The arena implementation provides multiple interfaces for compatibility with different allocation patterns while maintaining memory safety guarantees.

## File Index

| File | Description |
|------|-------------|
| ast_arena_core.f90 | Core arena allocator, memory block management, allocation tracking |
| ast_arena_compat.f90 | Compatibility layer for legacy allocation patterns |
| ast_arena_modern.f90 | Modern type-safe allocation interface |
| ast_arena_safe.f90 | Safety-checked allocation with bounds verification |

## Key Concepts

**Arena Allocation Model**
- Stack-like allocation in contiguous memory blocks
- No individual deallocation - entire arena freed at once
- Automatic lifetime management tied to scope
- Predictable memory layout for cache efficiency

**Memory Safety**
- Bounds checking in debug builds
- Index-based node references (not pointers)
- Prevents use-after-free errors
- Prevents memory leaks (automatic cleanup)

**Allocation Strategies**
- **Block-based**: Allocate large blocks, serve allocations from current block
- **Growth**: Allocate larger blocks when current block exhausted
- **Alignment**: Ensure proper alignment for performance
- **Tracking**: Record allocation metadata for debugging

**Performance Characteristics**
- O(1) allocation (bump pointer)
- O(1) bulk deallocation (free entire arena)
- No fragmentation (contiguous allocation)
- Cache-friendly (predictable memory layout)

**Stack Size Considerations**
- Large programs may exceed default stack limits
- Windows: 1-2 MB default stack
- Linux: 8 MB default stack
- Test target: `make test-small-stack` simulates Windows limits
- See `docs/MEMORY_SAFETY_ANALYSIS.md` for mitigation strategies

**Interface Variants**
- **Core**: Low-level allocation primitives
- **Compat**: Legacy interface for existing code
- **Modern**: Type-safe allocation with generics
- **Safe**: Bounds-checked allocation for debugging

## Dependencies

**Memory Infrastructure**
- `memory/arena_memory` - General-purpose arena allocator
- `memory/compiler_arena` - Compiler-wide allocation context

**AST Types**
- `ast/ast_base` - Base node types for allocation
- `ast/ast_types` - Type metadata for size calculations
