program test_parser_expressions_direct
    use parser_expressions_module
    use parser_state_module
    use lexer_core
    use ast_core
    use ast_factory
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== Parser Expressions Direct Tests ==="
    
    ! Test expression parsing hierarchy
    call test_simple_literal()
    call test_binary_operation()
    call test_range_expression()
    call test_logical_operations()
    call test_function_call()
    call test_array_subscript()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All parser expressions tests passed!"
        stop 0
    else
        print *, "Some parser expressions tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_simple_literal()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: expr_idx
        
        call test_start("Parse simple literal")
        
        allocate(tokens(1))
        tokens(1) = token_t(kind=TK_NUMBER, text="42", line=1, column=1)
        
        arena = create_ast_stack()
        expr_idx = parse_expression(tokens, arena)
        
        if (expr_idx > 0 .and. arena%size > 0) then
            call test_pass()
        else
            call test_fail("Failed to parse literal")
        end if
    end subroutine test_simple_literal
    
    subroutine test_binary_operation()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: expr_idx
        
        call test_start("Parse binary operation")
        
        allocate(tokens(3))
        tokens(1) = token_t(kind=TK_NUMBER, text="2", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text="+", line=1, column=3)
        tokens(3) = token_t(kind=TK_NUMBER, text="3", line=1, column=5)
        
        arena = create_ast_stack()
        expr_idx = parse_expression(tokens, arena)
        
        if (expr_idx > 0 .and. arena%size >= 3) then
            call test_pass()
        else
            call test_fail("Failed to parse binary operation")
        end if
    end subroutine test_binary_operation
    
    subroutine test_range_expression()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: expr_idx
        
        call test_start("Parse range expression")
        
        allocate(tokens(3))
        tokens(1) = token_t(kind=TK_NUMBER, text="1", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text=":", line=1, column=2)
        tokens(3) = token_t(kind=TK_NUMBER, text="10", line=1, column=3)
        
        arena = create_ast_stack()
        parser = create_parser_state(tokens)
        expr_idx = parse_range(parser, arena)
        
        if (expr_idx > 0) then
            call test_pass()
        else
            call test_fail("Failed to parse range")
        end if
    end subroutine test_range_expression
    
    subroutine test_logical_operations()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: expr_idx
        
        call test_start("Parse logical operation")
        
        allocate(tokens(3))
        tokens(1) = token_t(kind=TK_IDENTIFIER, text="x", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text=">", line=1, column=3)
        tokens(3) = token_t(kind=TK_NUMBER, text="0", line=1, column=5)
        
        arena = create_ast_stack()
        parser = create_parser_state(tokens)
        expr_idx = parse_comparison(parser, arena)
        
        if (expr_idx > 0) then
            call test_pass()
        else
            call test_fail("Failed to parse comparison")
        end if
    end subroutine test_logical_operations
    
    subroutine test_function_call()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: expr_idx
        
        call test_start("Parse function call")
        
        allocate(tokens(4))
        tokens(1) = token_t(kind=TK_IDENTIFIER, text="sqrt", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text="(", line=1, column=5)
        tokens(3) = token_t(kind=TK_NUMBER, text="4", line=1, column=6)
        tokens(4) = token_t(kind=TK_OPERATOR, text=")", line=1, column=7)
        
        arena = create_ast_stack()
        parser = create_parser_state(tokens)
        expr_idx = parse_primary(parser, arena)
        
        if (expr_idx > 0) then
            call test_pass()
        else
            call test_fail("Failed to parse function call")
        end if
    end subroutine test_function_call
    
    subroutine test_array_subscript()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: expr_idx
        
        call test_start("Parse array subscript")
        
        allocate(tokens(4))
        tokens(1) = token_t(kind=TK_IDENTIFIER, text="arr", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text="(", line=1, column=4)
        tokens(3) = token_t(kind=TK_NUMBER, text="5", line=1, column=5)
        tokens(4) = token_t(kind=TK_OPERATOR, text=")", line=1, column=6)
        
        arena = create_ast_stack()
        parser = create_parser_state(tokens)
        expr_idx = parse_primary(parser, arena)
        
        if (expr_idx > 0) then
            call test_pass()
        else
            call test_fail("Failed to parse array subscript")
        end if
    end subroutine test_array_subscript
    
    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start
    
    subroutine test_pass()
        print *, " ... PASSED"
        pass_count = pass_count + 1
    end subroutine test_pass
    
    subroutine test_fail(reason)
        character(len=*), intent(in) :: reason
        print *, " ... FAILED"
        print *, "  Reason: ", reason
    end subroutine test_fail
    
end program test_parser_expressions_direct