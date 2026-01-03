# Parser Expressions

## Purpose

This directory implements expression parsing using a Pratt parser (precedence climbing algorithm). It handles arithmetic, relational, and logical expressions, function calls, array operations, and complex nested expressions. The Pratt parser elegantly handles operator precedence and associativity.

## File Index

| File | Description |
|------|-------------|
| parser_expressions.f90 | Main Pratt parser implementation, operator precedence table |
| parser_expression_helpers.f90 | Expression parsing helper functions |
| parser_expression_operator_utils.f90 | Operator utilities: precedence, associativity, type |
| parser_expression_stacks.f90 | Expression parsing stack management |
| parser_expression_tokens.f90 | Expression token classification |
| parser_expression_arrays.f90 | Array expression parsing: indexing, slicing, constructors |
| parser_array_constructs.f90 | Array constructor parsing: `[1, 2, 3]`, implied-do |
| parser_assignment.f90 | Assignment statement parsing |
| parser_assignment_shared.f90 | Shared assignment utilities |
| parser_namelist_shared.f90 | Namelist expression utilities |

## Key Concepts

**Pratt Parsing (Precedence Climbing)**
- For complete Pratt parser design, see [Parser README](../README.md#key-concepts) and [docs/architecture/PRATT_PIPELINE_ARCHITECTURE.md](../../../docs/architecture/PRATT_PIPELINE_ARCHITECTURE.md)
- Elegant operator precedence via left/right binding power
- Naturally handles associativity and prefix/infix/postfix operators

**Operator Precedence (Highest to Lowest)**
1. **Primary**: Literals, identifiers, parentheses
2. **Postfix**: Array indexing `a(i)`, function calls `f(x)`, member access `obj%field`
3. **Prefix**: Unary plus/minus `+x`, `-x`, logical not `.not. x`
4. **Exponentiation**: `**` (right-associative)
5. **Multiplication/Division**: `*`, `/`
6. **Addition/Subtraction**: `+`, `-`
7. **Concatenation**: `//` (string concatenation)
8. **Relational**: `==`, `/=`, `<`, `<=`, `>`, `>=`, `.eq.`, `.ne.`, etc.
9. **Logical NOT**: `.not.`
10. **Logical AND**: `.and.`
11. **Logical OR**: `.or.`
12. **Logical equivalence**: `.eqv.`, `.neqv.`

**Array Expressions**
- **Indexing**: `a(i, j, k)` - element access
- **Slicing**: `a(1:10, :, 5)` - section access
- **Constructors**: `[1, 2, 3]` - array literals
- **Implied-do**: `[(i*2, i=1,10)]` - generated arrays
- **Reshape**: Multi-dimensional constructors

**Function Call Syntax**
- **Positional arguments**: `f(1, 2, 3)`
- **Keyword arguments**: `f(x=1, y=2, z=3)`
- **Mixed arguments**: `f(1, y=2, z=3)` (positional before keywords)
- **Array element access**: Disambiguate from function calls via context

**Assignment vs Expression**
- Parse assignment: `x = expr`
- Distinguish from equality test: `x == y`
- Handle pointer assignment: `x => target`
- Support array section assignment: `a(1:10) = 5`

**Expression Stacks**
- Operator stack for Pratt algorithm
- Operand stack for subexpression results
- Enables natural precedence handling
- Supports complex nested expressions

## Dependencies

**Parser Core**
- `parser/core/parser_state` - State management
- `parser/core/parser_utilities` - Parsing utilities

**AST Factory**
- `ast/factory/ast_factory_expressions` - Expression node creation
- `ast/factory/ast_factory_arrays` - Array node creation

**Lexer**
- `lexer/lexer_token_types` - Token types for operators
