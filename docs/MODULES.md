# Public modules and source map

Choose a public module according to the result the caller needs. An abstract
syntax tree (AST) stores parsed nodes in an arena; node indices refer to that
owning arena and must not be reused after it is reset or destroyed.

| Public module | Purpose |
| --- | --- |
| `fortfront_lexer` | Tokens and tokenization, including source trivia. |
| `fortfront_ast` | Arena and node types, traversal, locations, trivia and JSON inspection. |
| `fortfront_semantic` | Semantic contexts, inference and validation. |
| `fortfront_transform` | Source-to-source transformation and options. |
| `fortfront_tooling` | Load files or strings into an arena with tooling parse options. |
| `fortfront_compiler` | Owned frontend results and typed source, type, binding and ownership queries for compiler consumers. |

Compiler drivers should start with `compile_frontend_from_string` or
`compile_frontend_from_file` from `fortfront_compiler`. Tools that need a parsed
arena can use `tooling_load_ast_from_string` or
`tooling_load_ast_from_file`. Source transformers can use
`transform_lazy_fortran_string`. See the [library guide](guides/LIBRARY_USAGE.md)
for examples, diagnostics and query boundaries.

## Implementation directories

| Location | Responsibility |
| --- | --- |
| `src/lexer/` | Tokenization and lexical input handling. |
| `src/parser/` | Statement and expression parsing. |
| `src/ast/` | Node definitions, arena storage, traversal and introspection. |
| `src/semantic/` | Type inference, scopes, validation and semantic facts. |
| `src/standardizers/` | Standardization and source transformations. |
| `src/codegen/` | Standard Fortran source emission. |
| `src/interfaces/` | C interoperability bridge. |
| `app/` | Command-line applications. |
| `test/` | Native unit, integration, API and system regressions. |
| `examples/` | Canonical standard and Lazy Fortran input programs. |

The modules at the top of `src/` expose the public entry points and assemble
frontend stages. Detailed guides cover the
[parser pipeline](architecture/PRATT_PIPELINE_ARCHITECTURE.md) and
[semantic pipeline](architecture/SEMANTIC_PIPELINE_ARCHITECTURE.md).
