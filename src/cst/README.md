# Lexical trivia

## Purpose

This directory holds the lexical trivia surface: the comments, whitespace, and
newlines the lexer records alongside tokens, exposed so tools can ask what
trivia surrounds a given AST node.

It does **not** contain a concrete syntax tree. Earlier revisions carried a full
CST implementation with its own arena and node type, and this README described
it as the basis for formatters and refactoring tools. Nothing ever built one:
the parser goes from tokens to AST directly, and the arena was reachable only
from tests that populated it by hand. That machinery has been removed.

The `cst_` module names and `CST_*` constant names are retained because they are
part of the published `fortfront` interface and fluff compares against those
constant values.

## File Index

| File | Description |
|------|-------------|
| cst_nodes.f90 | `trivia_t` plus the `CST_COMMENT`, `CST_WHITESPACE`, and `CST_NEWLINE` kind constants |
| cst_trivia_query.f90 | Trivia lookup over the token stream, keyed to AST node source positions |

## Key Concepts

**Trivia is derived, not stored.** `get_trivia_for_ast_node` retokenizes the
source with `tokenize_core_with_trivia` and matches trivia runs against the
node's recorded line and column. There is no persistent side structure to keep
synchronized with the AST, which is why there is no arena here.

**Two distinct trivia types exist in fortfront.** `trivia_token_t`, from
`lexer_core`, is what the lexer emits per token. `trivia_t`, defined here, is
the shaped result handed to API consumers. They are not interchangeable: fluff's
F004 and F005 rules use the lexer type directly, while its AST wrapper uses this
one.

## Consumers

fluff's `fluff_ast.f90` calls `get_trivia_for_ast_node` through the `fortfront`
facade and converts `trivia_t` into its own `fluff_trivia_t`. Changing the shape
of `trivia_t` or the values of the kind constants is a breaking change for
fluff.

## Dependencies

`lexer_core` for tokenization; `ast_arena_modern` and `ast_introspection` for
resolving a node's source location.
