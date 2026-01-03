# Parser

## Purpose

The parser transforms a token stream from the lexer into an Abstract Syntax Tree (AST). It implements a Pratt parser for expressions and a recursive descent parser for statements and declarations. The parser handles both standard Fortran and lazy Fortran syntax, performing error recovery to continue parsing after syntax errors.

The parser is the second phase of compilation, bridging the lexer and semantic analysis.

## Directory Structure

- `core/` - Core parser infrastructure (state, dispatcher, utilities)
- `declarations/` - Declaration parsing (types, variables, interfaces, modules)
- `expressions/` - Expression parsing (Pratt parser, operators, arrays)
- `statements/` - Statement parsing (assignments, I/O, control flow)
- `procedures/` - Procedure parsing (functions, subroutines, parameters)
- `control_flow/` - Control flow parsing (if, select, where, do)

## Key Concepts

**Pratt Parsing for Expressions**
- Operator precedence climbing algorithm
- Left-associative and right-associative operators
- Handles complex expressions: `a + b * c ** d`
- See `docs/architecture/PRATT_PIPELINE_ARCHITECTURE.md` for details

**Recursive Descent for Statements**
- Top-down parsing with lookahead
- Predictive parsing based on first token
- Backtracking for ambiguous cases
- Context-sensitive parsing (e.g., `data` as keyword vs identifier)

**Error Recovery**
- Continue parsing after syntax errors
- Create error nodes to preserve partial information
- Synchronize at statement boundaries
- Report multiple errors per parse

**Two-Stage Parsing: CST → AST**
- **Stage 1**: Build Concrete Syntax Tree (preserves all details)
- **Stage 2**: Transform CST to Abstract Syntax Tree (semantic structure)
- Optional: Skip CST for faster parsing (direct to AST)

**Context-Sensitive Parsing**
- Keywords can be identifiers in some contexts: `data = 5` (assignment) vs `data x /5/` (data statement)
- Lookahead to disambiguate: `if (cond) x = 5` (statement if) vs `if (cond) then` (block if)
- See `parser/statements/parser_keyword_disambiguation.f90`

**Parser State Management**
- Tracks current token position
- Maintains scope depth for nesting
- Records parse mode (expression vs statement)
- Accumulates error messages

## Dependencies

**Lexer**
- `lexer/` - Token stream input

**AST**
- `ast/` - AST node creation and storage
- `ast/factory/` - Factory methods for node construction
- `ast/arena/` - Arena allocation for nodes

**Common Utilities**
- `common/identifier_table` - Identifier management
- `common/uid_generator` - Unique node IDs

**Error Handling**
- `error_handling` - Parse error reporting
- `error_reporting` - Structured error messages

**Location Tracking**
- All parser functions must populate `line` and `column` fields on AST nodes
- Source locations are validated by `frontend_location_validation` module
- Missing locations degrade diagnostic quality (appear as "line 1")
- Enable validation: `export FORTFRONT_VALIDATE_LOCATIONS=1`
- See issue #2383 for location validation architecture
