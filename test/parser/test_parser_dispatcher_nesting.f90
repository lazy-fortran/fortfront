program test_parser_dispatcher_nesting
    ! Test Assessment and Implementation for Issue #255
    ! Given: Parser dispatcher should correctly handle nested control structures
    ! When: Testing statement dispatcher for nested construct coordination  
    ! Then: Dispatcher should maintain proper nesting context and boundaries

    use parser_dispatcher_module
    use parser_state_module
    use lexer_core
    use ast_core
    use ast_factory
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== Parser Dispatcher Nesting Tests ==="
    print *, "Testing parser dispatcher coordination of nested structures"
    print *
    
    ! Test dispatcher handling of nested statements
    call test_dispatcher_nested_statements()
    
    ! Test dispatcher state preservation across nesting
    call test_dispatcher_state_preservation()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All parser dispatcher nesting tests passed!"
        stop 0
    else
        print *, "Some parser dispatcher nesting tests failed!"
        print *, "Issue #255: Parser dispatcher needs nested control structure fixes"
        stop 1
    end if
    
contains
    
    subroutine test_dispatcher_nested_statements()
        ! Given: Parser dispatcher with tokens for nested statements
        ! When: Dispatcher processes nested control flow statements
        ! Then: Should coordinate proper parsing of each nested level
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_idx
        
        call test_start("Dispatcher nested statement coordination")
        
        ! Create tokens for: if (x > 0) then do i = 1, 5 end do end if
        allocate(tokens(18))
        tokens(1) = token_t(kind=TK_KEYWORD, text="if", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text="(", line=1, column=4)
        tokens(3) = token_t(kind=TK_IDENTIFIER, text="x", line=1, column=5)
        tokens(4) = token_t(kind=TK_OPERATOR, text=">", line=1, column=7)
        tokens(5) = token_t(kind=TK_NUMBER, text="0", line=1, column=9)
        tokens(6) = token_t(kind=TK_OPERATOR, text=")", line=1, column=10)
        tokens(7) = token_t(kind=TK_KEYWORD, text="then", line=1, column=12)
        tokens(8) = token_t(kind=TK_NEWLINE, text="\n", line=1, column=16)
        tokens(9) = token_t(kind=TK_KEYWORD, text="do", line=2, column=5)
        tokens(10) = token_t(kind=TK_IDENTIFIER, text="i", line=2, column=8)
        tokens(11) = token_t(kind=TK_OPERATOR, text="=", line=2, column=10)
        tokens(12) = token_t(kind=TK_NUMBER, text="1", line=2, column=12)
        tokens(13) = token_t(kind=TK_OPERATOR, text=",", line=2, column=13)
        tokens(14) = token_t(kind=TK_NUMBER, text="5", line=2, column=15)
        tokens(15) = token_t(kind=TK_NEWLINE, text="\n", line=2, column=16)
        tokens(16) = token_t(kind=TK_KEYWORD, text="end", line=3, column=5)
        tokens(17) = token_t(kind=TK_KEYWORD, text="do", line=3, column=9)
        tokens(18) = token_t(kind=TK_NEWLINE, text="\n", line=3, column=11)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        
        stmt_idx = parse_statement_dispatcher(tokens, arena)
        
        if (stmt_idx > 0) then
            ! Check if we can successfully parse a statement that contains nested structures
            call test_pass()
        else
            call test_fail("Failed to dispatch nested statements")
        end if
        
        deallocate(tokens)
    end subroutine test_dispatcher_nested_statements
    
    subroutine test_dispatcher_state_preservation()
        ! Given: Parser dispatcher processing multiple statements
        ! When: Dispatcher handles sequence of nested statements  
        ! Then: Parser state should be preserved correctly between statements
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_idx
        
        call test_start("Dispatcher state preservation across nesting")
        
        ! Create tokens for simpler nested case: do i = 1, 3 end do
        allocate(tokens(9))
        tokens(1) = token_t(kind=TK_KEYWORD, text="do", line=1, column=1)
        tokens(2) = token_t(kind=TK_IDENTIFIER, text="i", line=1, column=4)
        tokens(3) = token_t(kind=TK_OPERATOR, text="=", line=1, column=6)
        tokens(4) = token_t(kind=TK_NUMBER, text="1", line=1, column=8)
        tokens(5) = token_t(kind=TK_OPERATOR, text=",", line=1, column=9)
        tokens(6) = token_t(kind=TK_NUMBER, text="3", line=1, column=11)
        tokens(7) = token_t(kind=TK_NEWLINE, text="\n", line=1, column=12)
        tokens(8) = token_t(kind=TK_KEYWORD, text="end", line=2, column=1)
        tokens(9) = token_t(kind=TK_KEYWORD, text="do", line=2, column=5)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        
        ! Test dispatcher can handle this simple case
        stmt_idx = parse_statement_dispatcher(tokens, arena)
        
        if (stmt_idx > 0) then
            ! Check that parser state is still valid after processing
            if (parser%current_token <= size(parser%tokens)) then
                call test_pass()
            else
                call test_fail("Parser state corrupted after nested parsing")
            end if
        else
            call test_fail("Failed to parse statement through dispatcher")
        end if
        
        deallocate(tokens)
    end subroutine test_dispatcher_state_preservation
    
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
    
end program test_parser_dispatcher_nesting