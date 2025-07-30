# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Test Commands

### Build
```bash
fpm build
```

### Run all tests
```bash
fpm test
```

### Run tests with coverage (Linux only)
```bash
fpm clean --all
fpm test --profile debug --flag '-cpp -fprofile-arcs -ftest-coverage -g'
lcov --capture --directory build/ --output-file coverage.info \
  --rc branch_coverage=1 \
  --ignore-errors inconsistent
lcov --remove coverage.info \
  'build/dependencies/*' \
  'test/*' \
  '/usr/*' \
  --output-file coverage_filtered.info
genhtml coverage_filtered.info --output-directory coverage_html \
  --branch-coverage \
  --legend
```

### Run a specific test
```bash
fpm test <test_name>
# Example: fpm test test_frontend_lexer_api
```

### Important build flags
- Always use `-cpp` flag when building/testing (required for preprocessing)
- Use `--profile debug` for debugging and coverage

## Development Tips

- Do not always use `fpm clean all`. Only use clean in cases where all other fixes fail

## Architecture Overview

fortfront is a Fortran frontend that processes code through four distinct phases:

1. **Lexer** (`src/lexer/`) - Tokenizes Fortran source code
   - Core module: `lexer_core.f90`
   - Produces `token_t` array

2. **Parser** (`src/parser/`) - Builds Abstract Syntax Tree (AST)
   - Core modules: `parser_core.f90`, `parser_dispatcher.f90`
   - Specialized parsers: `parser_expressions.f90`, `parser_declarations.f90`, `parser_control_flow.f90`
   - Uses `parser_state_t` to track parsing state
   - Produces AST nodes in a global arena

3. **Semantic Analysis** (`src/semantic/`) - Type checking and inference
   - Core module: `semantic_analyzer.f90`
   - Type system: `type_system_hm.f90` (Hindley-Milner type inference)
   - Scope management: `scope_manager.f90`
   - Produces typed AST with `semantic_context_t`

4. **Code Generation** (`src/codegen/`) - Emits standard Fortran
   - Core module: `codegen_core.f90`
   - Handles indentation: `codegen_indent.f90`
   - Generates clean, standard-compliant Fortran code

### Key Design Patterns

- **AST Arena**: All AST nodes are stored in a global arena (`ast_core.f90`)
- **Visitor Pattern**: AST transformations use visitors (`ast_visitor.f90`)
- **Typed AST**: Extended AST nodes with type information (`ast_typed.f90`)
- **JSON Serialization**: Debug/inspection via JSON (`ast_json.f90`, `json_writer.f90`)

### Main Entry Points

- `frontend.f90`: Main module with `compile_source` subroutine
- `frontend_integration.f90`: Higher-level compilation interface

### Test Organization

Tests are organized by compilation phase:
- `test/lexer/` - Lexical analysis tests
- `test/parser/` - Parser tests
- `test/semantic/` - Semantic analysis and type inference tests
- `test/codegen/` - Code generation tests
- `test/standardizer/` - AST standardization tests

Total: 72 test files, with 69 passing as of last update.

### Common Development Patterns

When adding new features:
1. Start with lexer if new tokens are needed
2. Update parser to handle new syntax
3. Add semantic analysis for type checking
4. Implement code generation
5. Write comprehensive tests for each phase