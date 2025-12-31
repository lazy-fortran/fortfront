# CST (Concrete Syntax Tree)

## Purpose

The CST subsystem provides a concrete syntax tree representation that preserves all source-level details including whitespace, comments, and exact formatting. Unlike the AST which abstracts away syntactic details, the CST maintains a complete, lossless representation of the source code.

CSTs are primarily used for tools that need to preserve formatting (formatters, refactoring tools) or analyze source-level patterns (style checkers, comment extractors).

## File Index

| File | Description |
|------|-------------|
| cst_core.f90 | Core CST data structures, node types, tree construction |
| cst_nodes.f90 | CST node type definitions, token-level representation |
| cst_arena.f90 | Arena-based allocation for CST nodes (similar to AST arena) |
| cst_trivia_query.f90 | Public trivia query helpers for CST and AST integration |

## Key Concepts

**CST vs AST**
- **CST**: Preserves all source details (whitespace, comments, exact token positions)
- **AST**: Abstracts syntax to semantic meaning (removes formatting, comments)
- **Use CST for**: Formatters, refactoring, source-level analysis
- **Use AST for**: Type checking, compilation, semantic analysis

**Lossless Representation**
- Every character from source preserved
- Exact token positions tracked
- Comments associated with nodes
- Whitespace patterns maintained

**CST Structure**
- Token-based nodes (not abstract constructs)
- Parent-child relationships explicit
- Sibling links for sequential access
- Source span for every node

**Arena Allocation**
- Similar to AST arena (no manual deallocation)
- CST nodes allocated in contiguous blocks
- Automatic cleanup on scope exit
- Index-based node references

**Typical Use Case: Formatter**
1. Parse source → CST
2. Traverse CST preserving comments
3. Apply formatting rules to whitespace
4. Emit formatted source from CST

## Dependencies

**Memory Management**
- `memory/arena_memory` - Arena allocator
- `memory/compiler_arena` - Compiler context

**Lexer**
- `lexer/` - Tokenization (CST built from token stream)

**Common Utilities**
- `common/identifier_table` - Identifier interning
