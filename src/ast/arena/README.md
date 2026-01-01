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
| ast_arena_source_text.f90 | Source text storage and retrieval utilities for arenas |

## Key Concepts

For complete arena allocation design principles, see [AST README](../README.md#key-concepts) and [docs/MEMORY_SAFETY_ANALYSIS.md](../../../docs/MEMORY_SAFETY_ANALYSIS.md).

**This Directory's Specifics**:
- **Core allocator**: `ast_arena_core.f90` - block management, allocation tracking
- **Compatibility layer**: `ast_arena_compat.f90` - legacy patterns
- **Modern interface**: `ast_arena_modern.f90` - type-safe allocation
- **Safety checks**: `ast_arena_safe.f90` - bounds verification
- **Source text**: `ast_arena_source_text.f90` - source retrieval for tooling

**Performance**: O(1) allocation, O(1) bulk deallocation, cache-friendly layout

**Stack limits**: Test with `make test-small-stack` to simulate Windows 1-2 MB limits

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
