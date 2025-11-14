# Lexer

## Purpose

The lexer (lexical analyzer) tokenizes Fortran source text into a stream of tokens for parsing. It handles both standard Fortran and lazy Fortran syntax, recognizes keywords, identifiers, literals, operators, and delimiters, and provides location tracking for error reporting.

The lexer is the first phase of compilation, converting raw text into structured tokens that the parser can process.

## File Index

| File | Description |
|------|-------------|
| lexer_api.f90 | Public API facade for lexer functionality |
| lexer_core.f90 | Core lexing logic, token stream generation, character classification |
| lexer_scanners.f90 | Specialized scanners for literals (numbers, strings, identifiers) |
| lexer_token_types.f90 | Token type definitions, keyword table, operator recognition |

## Key Concepts

**Token Types**
- **Keywords**: `program`, `function`, `if`, `do`, `end`, etc.
- **Identifiers**: Variable names, function names, module names
- **Literals**: Integer, real, complex, string, logical (`.true.`, `.false.`)
- **Operators**: Arithmetic (`+`, `-`, `*`, `/`, `**`), relational (`==`, `/=`, `<`, `>`), logical (`.and.`, `.or.`, `.not.`)
- **Delimiters**: Parentheses, commas, colons, semicolons
- **Special**: Newline, EOF, continuation (`&`)

**Keyword Recognition**
- Case-insensitive matching (Fortran is case-insensitive)
- Context-free recognition (parser handles context)
- Distinguish keywords from identifiers via lookup table
- Handle Fortran 77/90/95/2003/2008/2018 keywords

**Literal Scanning**
- **Integer**: `42`, `123_8`, `-5` (with kind parameters)
- **Real**: `3.14`, `1.0d0`, `2.5e-3` (scientific notation, double precision)
- **String**: `'hello'`, `"world"` (single or double quotes)
- **Complex**: `(1.0, 2.0)` (real and imaginary parts)
- **Logical**: `.true.`, `.false.`

**Operator Variants**
- **Fortran 77 style**: `.eq.`, `.ne.`, `.lt.`, `.le.`, `.gt.`, `.ge.`
- **Fortran 90 style**: `==`, `/=`, `<`, `<=`, `>`, `>=`
- Both styles supported for compatibility

**Location Tracking**
- Every token records source file, line, column
- Enables precise error messages
- Supports source-to-source transformations
- Used by IDE integrations for navigation

**Whitespace Handling**
- Free-form: Whitespace is insignificant (except in strings)
- Fixed-form: Column-based layout (optional, legacy support)
- Comments: `!` to end of line
- Continuation: `&` at end of line (free-form)

## Dependencies

**Common Utilities**
- `common/identifier_table` - Identifier interning
- `utilities/string_utils` - String manipulation

**Standard Library**
- `stdlib` - Character classification, string utilities
