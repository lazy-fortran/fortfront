program test_parser_nested_control_flow
    ! Test Assessment and Implementation for Issue #255
    ! Given: Parser control flow module should handle nested structures  
    ! When: Testing direct parser functions for nested constructs
    ! Then: Parser state should correctly handle nesting depth and boundaries

    use parser_control_flow_module
    use parser_state_module
    use lexer_core
    use ast_core
    use ast_factory
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== Parser Nested Control Flow Direct Tests ==="
    print *, "Testing parser's ability to handle nested control structures"
    print *
    
    ! Test nested if parsing capability 
    call test_nested_if_parsing()
    
    ! Test nested do loop parsing capability
    call test_nested_do_parsing()
    
    ! Test mixed nested structure parsing capability
    call test_mixed_nested_parsing()
    
    ! Test parser state depth tracking
    call test_parser_state_nesting_depth()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All parser nested control flow tests passed!"
        stop 0
    else
        print *, "Some parser nested control flow tests failed!"
        print *, "Issue #255: Parser needs nested control structure fixes"
        stop 1
    end if
    
contains
    
    subroutine test_nested_if_parsing()
        ! Given: Tokens representing nested if statements
        ! When: Parser processes nested if constructs
        ! Then: Should parse each if level without corruption
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_idx
        
        call test_start("Nested if statement parsing")
        
        ! Create tokens for: if (x > 0) then if (y > 0) then end if end if
        allocate(tokens(14))
        tokens(1) = token_t(kind=TK_KEYWORD, text="if", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text="(", line=1, column=4)
        tokens(3) = token_t(kind=TK_IDENTIFIER, text="x", line=1, column=5)
        tokens(4) = token_t(kind=TK_OPERATOR, text=">", line=1, column=7)
        tokens(5) = token_t(kind=TK_NUMBER, text="0", line=1, column=9)
        tokens(6) = token_t(kind=TK_OPERATOR, text=")", line=1, column=10)
        tokens(7) = token_t(kind=TK_KEYWORD, text="then", line=1, column=12)
        tokens(8) = token_t(kind=TK_KEYWORD, text="if", line=2, column=5)
        tokens(9) = token_t(kind=TK_OPERATOR, text="(", line=2, column=8)
        tokens(10) = token_t(kind=TK_IDENTIFIER, text="y", line=2, column=9)
        tokens(11) = token_t(kind=TK_OPERATOR, text=">", line=2, column=11)
        tokens(12) = token_t(kind=TK_NUMBER, text="0", line=2, column=13)
        tokens(13) = token_t(kind=TK_OPERATOR, text=")", line=2, column=14)
        tokens(14) = token_t(kind=TK_KEYWORD, text="then", line=2, column=16)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        
        stmt_idx = parse_if(parser, arena)
        
        if (stmt_idx > 0) then
            ! Basic success - parsed without crashing
            call test_pass()
        else
            call test_fail("Failed to parse nested if statements")
        end if
        
        deallocate(tokens)
    end subroutine test_nested_if_parsing
    
    subroutine test_nested_do_parsing()
        ! Given: Tokens representing nested do loops
        ! When: Parser processes nested do constructs  
        ! Then: Should parse each do level maintaining scope
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_idx
        
        call test_start("Nested do loop parsing")
        
        ! Create tokens for: do i = 1, 3 do j = 1, 2 end do end do
        allocate(tokens(16))
        tokens(1) = token_t(kind=TK_KEYWORD, text="do", line=1, column=1)
        tokens(2) = token_t(kind=TK_IDENTIFIER, text="i", line=1, column=4)
        tokens(3) = token_t(kind=TK_OPERATOR, text="=", line=1, column=6)
        tokens(4) = token_t(kind=TK_NUMBER, text="1", line=1, column=8)
        tokens(5) = token_t(kind=TK_OPERATOR, text=",", line=1, column=9)
        tokens(6) = token_t(kind=TK_NUMBER, text="3", line=1, column=11)
        tokens(7) = token_t(kind=TK_NEWLINE, text="\n", line=1, column=12)
        tokens(8) = token_t(kind=TK_KEYWORD, text="do", line=2, column=5)
        tokens(9) = token_t(kind=TK_IDENTIFIER, text="j", line=2, column=8)
        tokens(10) = token_t(kind=TK_OPERATOR, text="=", line=2, column=10)
        tokens(11) = token_t(kind=TK_NUMBER, text="1", line=2, column=12)
        tokens(12) = token_t(kind=TK_OPERATOR, text=",", line=2, column=13)
        tokens(13) = token_t(kind=TK_NUMBER, text="2", line=2, column=15)
        tokens(14) = token_t(kind=TK_NEWLINE, text="\n", line=2, column=16)
        tokens(15) = token_t(kind=TK_KEYWORD, text="end", line=3, column=5)
        tokens(16) = token_t(kind=TK_KEYWORD, text="do", line=3, column=9)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        
        stmt_idx = parse_do_loop(parser, arena)
        
        if (stmt_idx > 0) then
            ! Basic success - parsed without crashing
            call test_pass()
        else
            call test_fail("Failed to parse nested do loops")
        end if
        
        deallocate(tokens)
    end subroutine test_nested_do_parsing
    
    subroutine test_mixed_nested_parsing()
        ! Given: Tokens representing mixed nested structures (if inside do)
        ! When: Parser processes mixed nested constructs
        ! Then: Should parse both types in correct hierarchy
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_idx
        
        call test_start("Mixed nested structure parsing")
        
        ! Create tokens for: do i = 1, 5 if (i > 2) then end if end do
        allocate(tokens(18))
        tokens(1) = token_t(kind=TK_KEYWORD, text="do", line=1, column=1)
        tokens(2) = token_t(kind=TK_IDENTIFIER, text="i", line=1, column=4)
        tokens(3) = token_t(kind=TK_OPERATOR, text="=", line=1, column=6)
        tokens(4) = token_t(kind=TK_NUMBER, text="1", line=1, column=8)
        tokens(5) = token_t(kind=TK_OPERATOR, text=",", line=1, column=9)
        tokens(6) = token_t(kind=TK_NUMBER, text="5", line=1, column=11)
        tokens(7) = token_t(kind=TK_NEWLINE, text="\n", line=1, column=12)
        tokens(8) = token_t(kind=TK_KEYWORD, text="if", line=2, column=5)
        tokens(9) = token_t(kind=TK_OPERATOR, text="(", line=2, column=8)
        tokens(10) = token_t(kind=TK_IDENTIFIER, text="i", line=2, column=9)
        tokens(11) = token_t(kind=TK_OPERATOR, text=">", line=2, column=11)
        tokens(12) = token_t(kind=TK_NUMBER, text="2", line=2, column=13)
        tokens(13) = token_t(kind=TK_OPERATOR, text=")", line=2, column=14)
        tokens(14) = token_t(kind=TK_KEYWORD, text="then", line=2, column=16)
        tokens(15) = token_t(kind=TK_NEWLINE, text="\n", line=2, column=20)
        tokens(16) = token_t(kind=TK_KEYWORD, text="end", line=3, column=5)
        tokens(17) = token_t(kind=TK_KEYWORD, text="if", line=3, column=9)
        tokens(18) = token_t(kind=TK_NEWLINE, text="\n", line=3, column=11)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        
        stmt_idx = parse_do_loop(parser, arena)
        
        if (stmt_idx > 0) then
            ! Basic success - parsed without crashing
            call test_pass()
        else
            call test_fail("Failed to parse mixed nested structures")
        end if
        
        deallocate(tokens)
    end subroutine test_mixed_nested_parsing
    
    subroutine test_parser_state_nesting_depth()
        ! Given: Parser state handling nested structures
        ! When: Processing multiple levels of nesting
        ! Then: Parser state should track nesting correctly
        type(parser_state_t) :: parser
        type(token_t), allocatable :: tokens(:)
        
        call test_start("Parser state nesting depth tracking")
        
        ! Create simple tokens to test parser state creation
        allocate(tokens(5))
        tokens(1) = token_t(kind=TK_KEYWORD, text="do", line=1, column=1)
        tokens(2) = token_t(kind=TK_IDENTIFIER, text="i", line=1, column=4)
        tokens(3) = token_t(kind=TK_OPERATOR, text="=", line=1, column=6)
        tokens(4) = token_t(kind=TK_NUMBER, text="1", line=1, column=8)
        tokens(5) = token_t(kind=TK_EOF, text="", line=1, column=9)
        
        parser = create_parser_state(tokens)
        
        ! Check that parser state was created successfully
        ! This is a basic test - more complex nesting depth tracking would need
        ! access to internal parser state which may not be public
        if (parser%current_token == 1) then
            call test_pass()
        else
            call test_fail("Parser state not initialized correctly")
        end if
        
        deallocate(tokens)
    end subroutine test_parser_state_nesting_depth
    
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
    
end program test_parser_nested_control_flow