# Memory

## Purpose

The memory subsystem provides arena-based allocators for compiler-wide memory management. Arena allocation eliminates manual deallocation, prevents memory fragmentation, and provides predictable performance. All major compiler data structures (AST, CST, semantic context) use arena allocation.

## File Index

| File | Description |
|------|-------------|
| arena_memory.f90 | General-purpose arena allocator, block management, growth strategy |
| compiler_arena.f90 | Compiler-wide allocation context, phase-specific arenas, lifetime management |

## Key Concepts

**Arena Allocation**
- **Stack-like allocation**: Allocate forward, deallocate entire arena at once
- **No fragmentation**: Contiguous allocation eliminates memory holes
- **No manual deallocation**: Arena cleanup frees all allocations
- **Predictable performance**: O(1) allocation, O(1) bulk deallocation

**Allocation Strategy**
1. Allocate large memory block (e.g., 1 MB)
2. Serve allocations via bump pointer
3. When block full, allocate larger block
4. Link blocks in chain for bulk deallocation

**Growth Policy**
- Initial block: 1 MB
- Growth factor: 2x (each block twice the size of previous)
- Maximum block: Platform-dependent (32 MB typical)
- Exponential growth amortizes allocation cost

**Compiler Arena**
- Phase-specific arenas: Lexer, parser, semantic, codegen
- Independent lifetimes: Free phases independently
- Hierarchical structure: Parent arena owns child arenas
- Global arena: Compiler-wide persistent allocations

**Memory Safety**
- Index-based references (not pointers)
- Bounds checking in debug builds
- Use-after-free prevention
- Leak prevention (automatic cleanup)

**Performance Characteristics**
- **Allocation**: O(1) bump pointer increment
- **Deallocation**: O(1) entire arena freed
- **Memory locality**: Sequential allocations are contiguous
- **Cache efficiency**: Predictable access patterns

**Stack vs Heap**
- Arena blocks allocated on heap
- Managed objects (AST nodes) in arena
- Avoids stack overflow for large programs
- See `make test-small-stack` for validation

## Dependencies

**Standard Library**
- `iso_fortran_env` - `int64`, memory constants

**Utilities**
- `error_handling` - Memory allocation error reporting
