# Performance

## Purpose

This directory provides performance profiling and metrics for the AST subsystem. It tracks allocation counts, memory usage, traversal times, and other performance-critical operations. Performance monitoring helps identify bottlenecks and validate optimization efforts.

## File Index

| File | Description |
|------|-------------|
| ast_performance.f90 | AST performance metrics: allocation counts, memory usage, traversal times |

## Key Concepts

**Performance Metrics**
- **Allocation count**: Number of AST nodes allocated
- **Memory usage**: Total bytes allocated in arena
- **Traversal time**: Time to traverse entire AST
- **Parse time**: Time from tokens to AST
- **Semantic analysis time**: Time for type inference
- **Code generation time**: Time to emit Fortran

**Profiling Infrastructure**
- Timing instrumentation around critical sections
- Counter increments for allocation tracking
- Memory usage queries from arena allocator
- Low overhead when profiling disabled

**Performance Testing**
- Benchmark suite for common operations
- Regression testing for performance
- Comparison with baseline performance
- Identify performance regressions in CI

**Optimization Targets**
- Arena allocation efficiency
- AST traversal performance
- Type inference convergence
- Code generation speed

**Stack Usage Monitoring**
- Track stack depth during recursion
- Detect stack overflow risk
- Validate against small stack limits (Windows)
- See `make test-small-stack` for validation

**Memory Profiling**
- Track peak memory usage
- Identify memory leaks (should be none with arena)
- Measure arena growth patterns
- Optimize block sizes

**Typical Metrics Output**
```
=== Fortfront Performance Metrics ===
Parse time: 123 ms
Semantic analysis time: 456 ms
Code generation time: 78 ms
Total time: 657 ms

AST nodes allocated: 12,345
Memory used: 2.4 MB
Peak memory: 2.8 MB

Traversals: 15
Average traversal time: 3 ms
```

## Dependencies

**AST**
- `ast/` - AST operations to instrument

**Memory**
- `memory/arena_memory` - Arena memory statistics

**Utilities**
- `utilities/debug_trace` - Logging infrastructure
