program test_cycle_exit_statements
    use parser_statements_module
    use parser_state_module
    use lexer_core
    use ast_core
    use ast_factory
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== Cycle/Exit Statement Tests ==="
    
    ! Test cycle and exit statements
    call test_simple_cycle()
    call test_simple_exit()
    call test_cycle_with_label()
    call test_exit_with_label()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All cycle/exit tests passed!"
        stop 0
    else
        print *, "Some cycle/exit tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_simple_cycle()
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        integer :: cycle_index
        
        call test_start("Simple CYCLE statement")
        
        allocate(tokens(2))
        tokens(1) = token_t(kind=TK_KEYWORD, text="cycle", line=1, column=1)
        tokens(2) = token_t(kind=TK_EOF, text="", line=1, column=6)
        
        parser = create_parser_state(tokens)
        arena = create_ast_arena()
        
        cycle_index = parse_cycle_statement(parser, arena)
        
        if (cycle_index > 0) then
            call test_pass()
        else
            call test_fail("Failed to parse simple cycle statement")
        end if
    end subroutine test_simple_cycle
    
    subroutine test_simple_exit()
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        integer :: exit_index
        
        call test_start("Simple EXIT statement")
        
        allocate(tokens(2))
        tokens(1) = token_t(kind=TK_KEYWORD, text="exit", line=1, column=1)
        tokens(2) = token_t(kind=TK_EOF, text="", line=1, column=5)
        
        parser = create_parser_state(tokens)
        arena = create_ast_arena()
        
        exit_index = parse_exit_statement(parser, arena)
        
        if (exit_index > 0) then
            call test_pass()
        else
            call test_fail("Failed to parse simple exit statement")
        end if
    end subroutine test_simple_exit
    
    subroutine test_cycle_with_label()
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        integer :: cycle_index
        
        call test_start("CYCLE with label")
        
        allocate(tokens(3))
        tokens(1) = token_t(kind=TK_KEYWORD, text="cycle", line=1, column=1)
        tokens(2) = token_t(kind=TK_IDENTIFIER, text="outer_loop", line=1, column=7)
        tokens(3) = token_t(kind=TK_EOF, text="", line=1, column=17)
        
        parser = create_parser_state(tokens)
        arena = create_ast_arena()
        
        cycle_index = parse_cycle_statement(parser, arena)
        
        if (cycle_index > 0) then
            call test_pass()
        else
            call test_fail("Failed to parse cycle with label")
        end if
    end subroutine test_cycle_with_label
    
    subroutine test_exit_with_label()
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        integer :: exit_index
        
        call test_start("EXIT with label")
        
        allocate(tokens(3))
        tokens(1) = token_t(kind=TK_KEYWORD, text="exit", line=1, column=1)
        tokens(2) = token_t(kind=TK_IDENTIFIER, text="main_loop", line=1, column=6)
        tokens(3) = token_t(kind=TK_EOF, text="", line=1, column=15)
        
        parser = create_parser_state(tokens)
        arena = create_ast_arena()
        
        exit_index = parse_exit_statement(parser, arena)
        
        if (exit_index > 0) then
            call test_pass()
        else
            call test_fail("Failed to parse exit with label")
        end if
    end subroutine test_exit_with_label
    
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
    
end program test_cycle_exit_statements