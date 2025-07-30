# Test Coverage Improvement Plan

## Current Status
- **Current Coverage**: 41.9% (3378 out of 8064 lines)
- **Target Coverage**: 80%
- **Gap**: Need to cover an additional ~3073 lines

## Coverage Analysis by Module

### 1. Lexer Module (`src/lexer/`)
**Priority: HIGH** - Core functionality that all other modules depend on

- [ ] `lexer_core.f90` - Test all token types, error conditions, edge cases
- [ ] `lexer_diagnostics.f90` - Test error reporting and diagnostic messages
- [ ] `lexer_position.f90` - Test position tracking across different input scenarios

### 2. Parser Module (`src/parser/`)
**Priority: HIGH** - Critical for AST generation

- [ ] `parser_core.f90` - Test basic parsing infrastructure
- [ ] `parser_expressions.f90` - Test all expression types:
  - Binary operators (arithmetic, logical, comparison)
  - Unary operators
  - Function calls
  - Array indexing
  - Type conversions
- [ ] `parser_declarations.f90` - Test:
  - Variable declarations with different types
  - Function/subroutine declarations
  - Module declarations
  - Interface blocks
- [ ] `parser_control_flow.f90` - Test:
  - IF/THEN/ELSE constructs
  - DO loops (all variants)
  - SELECT CASE
  - WHERE constructs
- [ ] `parser_dispatcher.f90` - Test dispatch logic for different statement types
- [ ] `parser_state.f90` - Test parser state management
- [ ] `parser_utils.f90` - Test utility functions

### 3. AST Module (`src/ast/`)
**Priority: MEDIUM** - Important for AST manipulation

- [ ] `ast_core.f90` - Test arena allocation and node management
- [ ] `ast_visitor.f90` - Test visitor pattern implementation
- [ ] `ast_builder.f90` - Test AST construction helpers
- [ ] `ast_utils.f90` - Test AST utility functions
- [ ] `ast_json.f90` - Test JSON serialization/deserialization
- [ ] `ast_typed.f90` - Test typed AST nodes

### 4. Semantic Analysis Module (`src/semantic/`)
**Priority: HIGH** - Type checking is critical

- [x] `type_system_hm.f90` - Already improved, but needs more:
  - [ ] Test complex type inference scenarios
  - [ ] Test generic types
  - [ ] Test type constraints
- [x] `json_reader.f90` - Basic coverage added
- [ ] `semantic_analyzer.f90` - Test:
  - Type checking for all expression types
  - Scope resolution
  - Function overloading resolution
- [ ] `scope_manager.f90` - Test:
  - Nested scopes
  - Module scopes
  - Interface scopes
- [ ] `semantic_checks.f90` - Test semantic validation rules

### 5. Code Generation Module (`src/codegen/`)
**Priority: MEDIUM** - Important for output correctness

- [ ] `codegen_core.f90` - Test code generation for all AST node types
- [ ] `codegen_indent.f90` - Test indentation logic
- [ ] `codegen_utils.f90` - Test code generation utilities

### 6. Utility Modules (`src/utils/`)
**Priority: LOW** - Support functionality

- [ ] `json_writer.f90` - Test JSON output formatting
- [ ] `string_utils.f90` - Test string manipulation functions
- [ ] `error_handler.f90` - Test error handling infrastructure

## Test Implementation Strategy

### Phase 1: Critical Path Coverage (Target: 60%)
Focus on the main compilation pipeline:
1. Lexer core functionality
2. Parser for basic constructs
3. Semantic analyzer for basic types
4. Code generator for basic output

### Phase 2: Comprehensive Coverage (Target: 80%)
1. Edge cases and error paths
2. Complex language features
3. Advanced type inference
4. Optimization paths

### Phase 3: Robustness (Target: 90%+)
1. Stress tests
2. Performance tests
3. Integration tests
4. Fuzz testing

## Test File Naming Convention
- Unit tests: `test/[module]/test_[component].f90`
- Integration tests: `test/integration/test_[feature].f90`
- Coverage tests: `test/[module]/test_[component]_coverage.f90`

## Test Implementation Guidelines

1. **Test Driven Development**: Write tests before implementing features
2. **Edge Cases**: Always test boundary conditions
3. **Error Paths**: Test all error handling code
4. **Real-World Scenarios**: Include tests based on actual Fortran code patterns
5. **Performance**: Include benchmarks for critical paths

## Immediate Actions

1. Create comprehensive lexer tests - this will likely add 500+ lines of coverage
2. Add parser tests for all statement types - another 1000+ lines
3. Expand semantic analysis tests - 500+ lines
4. Add code generation tests - 500+ lines

## Notes
- Focus on testing error paths as they are often uncovered
- Use parameterized tests where possible to test multiple scenarios
- Ensure each test is independent and can run in isolation
- Add comments explaining what each test validates