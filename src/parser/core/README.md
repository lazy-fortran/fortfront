# Parser Core

## Purpose

The parser core provides the fundamental infrastructure for parsing operations: state management, token access, parse dispatching, and shared utilities. This directory contains the foundational components used by all specialized parsers (declarations, expressions, statements).

## File Index

| File | Description |
|------|-------------|
| parser_api.f90 | Public API facade for parser operations |
| parser_state.f90 | Parser state management: token position, scope depth, error accumulation |
| parser_dispatcher.f90 | Main parse dispatcher facade (includes two parts below) |
| parser_dispatcher_part1.inc | Parse dispatcher part 1: statement type detection and routing |
| parser_dispatcher_part2.inc | Parse dispatcher part 2: declaration and expression routing |
| parser_utilities.f90 | General parsing utilities: lookahead, backtracking, synchronization |
| parser_utils.f90 | Low-level parser utilities: token matching, consumption |
| parser_token_views.f90 | Token stream view abstraction for lookahead |
| parser_inline_instantiation.f90 | Shared helpers for consuming inline instantiation braces |
| parser_implicit_shared.f90 | Shared implicit typing utilities |
| parser_import_resolution.f90 | Import statement resolution and dependency tracking |
| mixed_construct_detector.f90 | Detect mixed lazy/standard Fortran constructs |
| ensure_if_do_registration_bridge.f90 | Bridge for if/do control flow registration |
| ensure_forall_array_registration_bridge.f90 | Bridge for forall/array construct registration |
| url_utilities.f90 | URL handling utilities (for error reporting with links) |

## Key Concepts

**Parser State**
- **Current position**: Index into token stream
- **Scope depth**: Track nesting level (for indentation, error recovery)
- **Error list**: Accumulate parse errors
- **Parse mode**: Expression vs statement vs declaration
- **Lookahead buffer**: Peek ahead without consuming tokens

**Parse Dispatcher**
- Central routing based on current token
- Dispatches to specialized parsers:
  - `program`/`module`/`function` → declaration parser
  - `if`/`do`/`select` → control flow parser
  - `print`/`read`/`write` → I/O statement parser
  - Identifier → assignment or call statement parser
  - Expression start → expression parser
- Handles ambiguous cases with lookahead

**Token Views**
- Abstraction over token array
- Provides lookahead: `peek(1)`, `peek(2)`, etc.
- Supports backtracking: save position, restore on failure
- Efficient sliding window over token stream

**Error Recovery**
- **Panic mode**: Skip tokens until synchronization point
- **Synchronization points**: Statement boundaries (`;`, newline, `end`)
- **Error nodes**: Preserve partial parse information
- **Continue parsing**: Report multiple errors in one pass

**Mixed Construct Detection**
- Identify `!fortfront:standard_begin` / `!fortfront:standard_end` markers
- Route standard blocks to pass-through handler
- Route lazy blocks to full transformation pipeline
- See `docs/guides/MIXED_CONSTRUCTS_GUIDE.md`

**Import Resolution**
- Track `use module_name` statements
- Build module dependency graph
- Resolve imported identifiers
- Support `only:` and rename clauses

## Dependencies

**Lexer**
- `lexer/lexer_token_types` - Token type definitions

**AST**
- `ast/factory/` - Create AST nodes
- `ast/arena/` - Allocate nodes in arena

**Common Utilities**
- `common/identifier_table` - Identifier management

**Error Handling**
- `error_handling` - Error reporting infrastructure
