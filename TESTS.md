# Coverage Test Strategy

## Current Coverage Analysis

**Coverage Status**: 91.2% (31/34 measurable lines)
**Problem**: Only 34 lines across entire codebase are measurable for coverage
**Root Cause**: Most modules contain only interfaces/types, or functions not called by tests

## Complete Coverage Breakdown

### ✅ Fully Covered Modules (100%)
```
src/ast/ast_factory.f90                    1/1    lines
src/codegen/codegen_core.f90               1/1    lines  
src/codegen/codegen_indent.f90             1/1    lines
src/parser/parser_declarations.f90         1/1    lines
src/parser/parser_statements.f90           1/1    lines
src/semantic/semantic_analyzer_with_checks.f90  1/1 lines
src/semantic/semantic_checks.f90           1/1    lines
src/standardizer.f90                       1/1    lines
```

### ⚠️ Partially Covered Modules
```
src/semantic/type_system_hm.f90           23/25  lines (92%) - missing 699-700
src/json_reader.f90                       0/1    lines (0%)  - missing 709
```

### ❌ Zero Measurable Lines (Need Function Calls)
```
src/frontend.f90                          0      lines
src/lexer/lexer_core.f90                  0      lines  
src/parser/parser_core.f90                0      lines
src/parser/parser_control_flow.f90        0      lines
src/parser/parser_dispatcher.f90          0      lines
src/parser/parser_expressions.f90         0      lines
src/parser/parser_state.f90               0      lines
src/ast/ast_core.f90                      0      lines
src/json_writer.f90                       0      lines
src/semantic/parameter_tracker.f90        0      lines
src/semantic/scope_manager.f90            0      lines
src/semantic/semantic_analyzer.f90        0      lines
src/semantic/type_checker.f90             0      lines
src/string_types.f90                      0      lines
```

## Test Strategy: Direct Function Calls

**Goal**: Call functions from modules showing 0 lines to generate coverage data

### Test 1: Frontend Pipeline Test
**File**: `test_frontend_core.f90`
**Target**: `frontend.f90`, `lexer_core.f90`, `parser_core.f90`

```fortran
program test_frontend_core
    use frontend, only: compile_source, lex_file, parse_tokens, emit_fortran
    use lexer_core, only: tokenize_core, token_type_name
    use parser_core, only: parse_expression, parse_primary
    
    ! Test frontend.f90 functions
    call compile_source("integer :: x = 42", result, error)
    call lex_file("real :: y", tokens, error)
    call parse_tokens(tokens, arena, root, error)
    call emit_fortran(arena, root, code)
    
    ! Test lexer_core.f90 functions directly
    call tokenize_core("logical :: flag = .true.", core_tokens)
    type_name = token_type_name(TK_KEYWORD)
    
    ! Test parser_core.f90 functions directly
    state = create_parser_state(tokens)
    expr_node = parse_expression(state, arena)
    prim_node = parse_primary(state, arena)
end program
```

### Test 2: Parser Modules Test  
**File**: `test_parser_modules.f90`
**Target**: `parser_expressions.f90`, `parser_control_flow.f90`, `parser_dispatcher.f90`

```fortran
program test_parser_modules
    use parser_expressions, only: parse_logical_or, parse_comparison, parse_term
    use parser_control_flow, only: parse_if, parse_do_loop, parse_select_case
    use parser_dispatcher, only: parse_statement_dispatcher
    use parser_state, only: parser_peek, parser_consume
    
    ! Test parser_expressions.f90
    lor_node = parse_logical_or(state, arena)
    cmp_node = parse_comparison(state, arena)  
    term_node = parse_term(state, arena)
    
    ! Test parser_control_flow.f90
    if_node = parse_if(state, arena)
    do_node = parse_do_loop(state, arena)
    case_node = parse_select_case(state, arena)
    
    ! Test parser_dispatcher.f90
    stmt_node = parse_statement_dispatcher(state, arena)
    
    ! Test parser_state.f90
    token = parser_peek(state)
    call parser_consume(state)
end program
```

### Test 3: AST Operations Test
**File**: `test_ast_core.f90`  
**Target**: `ast_core.f90`, `json_writer.f90`

```fortran
program test_ast_core
    use ast_core, only: create_program, create_assignment, create_binary_op,
                        create_identifier, create_literal, ast_arena_push
    use json_writer, only: write_ast_to_json, write_token_to_json
    
    ! Test ast_core.f90 functions
    prog_idx = create_program(arena, "test_prog")
    id_idx = create_identifier(arena, "variable")
    lit_idx = create_literal(arena, "42", LITERAL_INTEGER)
    assign_idx = create_assignment(arena, id_idx, lit_idx)
    binop_idx = create_binary_op(arena, "+", lit_idx, lit_idx)
    
    call ast_arena_push(arena, prog_idx)
    
    ! Test json_writer.f90 functions
    json_str = write_ast_to_json(arena, prog_idx)
    token_json = write_token_to_json(token)
end program
```

### Test 4: Semantic Analysis Test
**File**: `test_semantic_core.f90`
**Target**: `semantic_analyzer.f90`, `scope_manager.f90`, `type_checker.f90`

```fortran
program test_semantic_core
    use semantic_analyzer, only: analyze_semantics, create_semantic_context
    use scope_manager, only: push_scope, pop_scope, add_symbol
    use type_checker, only: check_type_compatibility, infer_type
    use parameter_tracker, only: track_parameter, get_parameter_info
    
    ! Test semantic_analyzer.f90
    ctx = create_semantic_context()
    call analyze_semantics(arena, root_idx, ctx)
    
    ! Test scope_manager.f90  
    call push_scope(ctx%scopes, "function")
    call add_symbol(ctx%scopes, "x", type_info)
    call pop_scope(ctx%scopes)
    
    ! Test type_checker.f90
    compatible = check_type_compatibility(type1, type2)
    inferred = infer_type(expr_node, ctx)
    
    ! Test parameter_tracker.f90
    call track_parameter(tracker, "param", param_info)
    info = get_parameter_info(tracker, "param")
end program
```

### Test 5: Utility Modules Test
**File**: `test_utilities.f90`
**Target**: `string_types.f90`, remaining json_reader.f90 line

```fortran
program test_utilities
    use string_types, only: string_t, string_length, string_concat
    use json_reader, only: json_to_semantic
    
    ! Test string_types.f90
    str1 = string_t("hello")
    str2 = string_t("world") 
    len = string_length(str1)
    result = string_concat(str1, str2)
    
    ! Test json_reader.f90 missing line 709
    call json_to_semantic(json_file, arena, root, ctx) ! Exercise error path
end program
```

### Test 6: Complete Missing Lines
**File**: `test_missing_lines.f90`
**Target**: Specific uncovered lines in type_system_hm.f90

```fortran
program test_missing_lines
    use type_system_hm, only: free_type_vars
    
    ! Create scenario that exercises lines 699-700 in type_system_hm.f90
    ! These are likely error handling or edge case paths
    
    ! Create type with specific structure to hit missing lines
    type_with_edge_case = create_complex_type()
    call free_type_vars(type_with_edge_case, vars) ! Hit lines 699-700
end program
```

## Implementation Plan

1. **Create Test 1** - Frontend pipeline (expect large coverage boost)
2. **Measure coverage** - Should see many more measurable lines
3. **Create Test 2** - Parser modules  
4. **Create Test 3** - AST operations
5. **Create Test 4** - Semantic analysis
6. **Create Test 5** - Utilities
7. **Create Test 6** - Specific missing lines

## Success Metrics

- **Immediate Goal**: >1000 measurable lines (from current 34)
- **Coverage Target**: >80% of measurable lines
- **Test Quality**: Fast (<2s each), meaningful assertions, real function calls
- **Non-shallow**: Tests exercise actual module logic, not just imports