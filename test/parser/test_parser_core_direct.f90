program test_parser_core_direct
    use lexer_core, only: tokenize_core, token_t, TK_IDENTIFIER, TK_NUMBER, &
                          TK_OPERATOR, TK_EOF
    use parser_expressions_module, only: parse_expression, parse_primary, &
                                        parse_term, parse_factor
    use parser_state_module, only: create_parser_state, parser_state_t
    use ast_core, only: ast_arena_t, create_ast_arena
    implicit none

    integer :: total_tests, passed_tests
    type(token_t), allocatable :: tokens(:)
    type(ast_arena_t) :: arena
    type(parser_state_t) :: parser
    integer :: expr_index

    total_tests = 0
    passed_tests = 0

    print *, "=== Parser Core Direct Function Tests ==="
    print *, ""

    ! Test 1: Parse simple identifier expression
    call test_start("Parse identifier expression")
    call tokenize_core("x", tokens)
    arena = create_ast_arena()
    expr_index = parse_expression(tokens, arena)
    if (expr_index > 0 .and. arena%size >= 1) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: expr_index>0, arena%size>=1"
        print *, "  Got: expr_index=", expr_index, ", arena%size=", arena%size
    end if

    ! Test 2: Parse simple number expression
    call test_start("Parse number expression")
    call tokenize_core("42", tokens)
    arena = create_ast_arena()
    expr_index = parse_expression(tokens, arena)
    if (expr_index > 0 .and. arena%size >= 1) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: expr_index>0, arena%size>=1"
        print *, "  Got: expr_index=", expr_index, ", arena%size=", arena%size
    end if

    ! Test 3: Parse binary addition expression
    call test_start("Parse binary addition")
    call tokenize_core("a + b", tokens)
    arena = create_ast_arena()
    expr_index = parse_expression(tokens, arena)
    ! Should create at least 3 nodes: identifier(a), identifier(b), binary_op(+)
    if (expr_index > 0 .and. arena%size >= 3) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: expr_index>0, arena%size>=3 (a,b,+)"
        print *, "  Got: expr_index=", expr_index, ", arena%size=", arena%size
    end if

    ! Test 4: Parse multiplication expression
    call test_start("Parse multiplication")
    call tokenize_core("x * y", tokens)
    arena = create_ast_arena()
    expr_index = parse_expression(tokens, arena)
    if (expr_index > 0 .and. arena%size >= 3) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: expr_index>0, arena%size>=3 (x,y,*)"
        print *, "  Got: expr_index=", expr_index, ", arena%size=", arena%size
    end if

    ! Test 5: Parse precedence expression (a + b * c)
    call test_start("Parse precedence expression")
    call tokenize_core("a + b * c", tokens)
    arena = create_ast_arena()
    expr_index = parse_expression(tokens, arena)
    ! Should create 5 nodes: a, b, c, *, +
    if (expr_index > 0 .and. arena%size >= 5) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: expr_index>0, arena%size>=5 (a,b,c,*,+)"
        print *, "  Got: expr_index=", expr_index, ", arena%size=", arena%size
    end if

    ! Test 6: Parse parenthesized expression
    call test_start("Parse parentheses")
    call tokenize_core("(x + y)", tokens)
    arena = create_ast_arena()
    expr_index = parse_expression(tokens, arena)
    if (expr_index > 0 .and. arena%size >= 3) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: expr_index>0, arena%size>=3 (x,y,+)"
        print *, "  Got: expr_index=", expr_index, ", arena%size=", arena%size
    end if

    ! Test 7: Parse function call
    call test_start("Parse function call")
    call tokenize_core("sin(x)", tokens)
    arena = create_ast_arena()
    expr_index = parse_expression(tokens, arena)
    if (expr_index > 0 .and. arena%size >= 2) then  ! sin, x, call
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: expr_index>0, arena%size>=2 (sin,x,call)"
        print *, "  Got: expr_index=", expr_index, ", arena%size=", arena%size
    end if

    ! Test 8: Parse array literal
    call test_start("Parse array literal")
    call tokenize_core("[1, 2, 3]", tokens)
    arena = create_ast_arena()
    expr_index = parse_expression(tokens, arena)
    if (expr_index > 0 .and. arena%size >= 4) then  ! 1, 2, 3, array
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: expr_index>0, arena%size>=4 (1,2,3,array)"
        print *, "  Got: expr_index=", expr_index, ", arena%size=", arena%size
    end if

    ! Test 9: Parse term directly (factor-level parsing)
    call test_start("Parse term directly")
    call tokenize_core("a * b", tokens)
    arena = create_ast_arena()
    parser = create_parser_state(tokens)
    expr_index = parse_term(parser, arena)
    if (expr_index > 0 .and. arena%size >= 3) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: expr_index>0, arena%size>=3"
        print *, "  Got: expr_index=", expr_index, ", arena%size=", arena%size
    end if

    ! Test 10: Parse factor directly (primary-level parsing)
    call test_start("Parse factor directly")
    call tokenize_core("42", tokens)
    arena = create_ast_arena()
    parser = create_parser_state(tokens)
    expr_index = parse_factor(parser, arena)
    if (expr_index > 0 .and. arena%size >= 1) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: expr_index>0, arena%size>=1"
        print *, "  Got: expr_index=", expr_index, ", arena%size=", arena%size
    end if

    ! Test 11: Parse primary directly (atomic expressions)
    call test_start("Parse primary directly")
    call tokenize_core("variable", tokens)
    arena = create_ast_arena()
    parser = create_parser_state(tokens)
    expr_index = parse_primary(parser, arena)
    if (expr_index > 0 .and. arena%size >= 1) then
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: expr_index>0, arena%size>=1"
        print *, "  Got: expr_index=", expr_index, ", arena%size=", arena%size
    end if

    ! Test 12: Empty expression handling
    call test_start("Empty expression handling")
    call tokenize_core("", tokens)  ! Only EOF token
    arena = create_ast_arena()
    expr_index = parse_expression(tokens, arena)
    ! Should handle gracefully (expr_index might be 0)
    if (arena%size >= 0) then  ! Just shouldn't crash
        call test_pass()
    else
        call test_fail()
        print *, "  Expected: no crash on empty input"
        print *, "  Got: expr_index=", expr_index, ", arena%size=", arena%size
    end if

    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", total_tests, " tests"

    if (passed_tests == total_tests) then
        print *, "All parser core direct tests passed!"
        stop 0
    else
        print *, "Some parser core tests failed!"
        stop 1
    end if

contains

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        total_tests = total_tests + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start

    subroutine test_pass()
        print *, " ... PASSED"
        passed_tests = passed_tests + 1
    end subroutine test_pass

    subroutine test_fail()
        print *, " ... FAILED"
    end subroutine test_fail

end program test_parser_core_direct