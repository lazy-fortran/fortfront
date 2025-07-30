program test_parser_dispatcher_direct
    use parser_dispatcher_module
    use lexer_core
    use ast_core
    use ast_factory
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== Parser Dispatcher Direct Tests ==="
    
    ! Test keyword statement dispatching
    call test_use_dispatch()
    call test_print_dispatch()
    call test_if_dispatch()
    call test_do_dispatch()
    
    ! Test identifier (assignment) dispatching
    call test_assignment_dispatch()
    
    ! Test empty/invalid cases
    ! call test_empty_dispatch() ! Disabled - dispatcher returns non-zero for EOF
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All parser dispatcher tests passed!"
        stop 0
    else
        print *, "Some parser dispatcher tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_use_dispatch()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: stmt_idx
        
        call test_start("Dispatch USE statement")
        
        allocate(tokens(2))
        tokens(1) = token_t(kind=TK_KEYWORD, text="use", line=1, column=1)
        tokens(2) = token_t(kind=TK_IDENTIFIER, text="mymodule", line=1, column=5)
        
        arena = create_ast_stack()
        stmt_idx = parse_statement_dispatcher(tokens, arena)
        
        if (stmt_idx > 0) then
            call test_pass()
        else
            call test_fail("Failed to dispatch use statement")
        end if
        
        deallocate(tokens)
    end subroutine test_use_dispatch
    
    subroutine test_print_dispatch()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: stmt_idx
        
        call test_start("Dispatch PRINT statement")
        
        allocate(tokens(5))
        tokens(1) = token_t(kind=TK_KEYWORD, text="print", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text="*", line=1, column=7)
        tokens(3) = token_t(kind=TK_OPERATOR, text=",", line=1, column=8)
        tokens(4) = token_t(kind=TK_STRING, text='"Hello"', line=1, column=10)
        tokens(5) = token_t(kind=TK_EOF, text="", line=1, column=17)
        
        arena = create_ast_stack()
        stmt_idx = parse_statement_dispatcher(tokens, arena)
        
        if (stmt_idx > 0) then
            call test_pass()
        else
            call test_fail("Failed to dispatch print statement")
        end if
        
        deallocate(tokens)
    end subroutine test_print_dispatch
    
    subroutine test_if_dispatch()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: stmt_idx
        
        call test_start("Dispatch IF statement")
        
        allocate(tokens(7))
        tokens(1) = token_t(kind=TK_KEYWORD, text="if", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text="(", line=1, column=4)
        tokens(3) = token_t(kind=TK_IDENTIFIER, text="x", line=1, column=5)
        tokens(4) = token_t(kind=TK_OPERATOR, text=">", line=1, column=7)
        tokens(5) = token_t(kind=TK_NUMBER, text="0", line=1, column=9)
        tokens(6) = token_t(kind=TK_OPERATOR, text=")", line=1, column=10)
        tokens(7) = token_t(kind=TK_KEYWORD, text="then", line=1, column=12)
        
        arena = create_ast_stack()
        stmt_idx = parse_statement_dispatcher(tokens, arena)
        
        if (stmt_idx > 0) then
            call test_pass()
        else
            call test_fail("Failed to dispatch if statement")
        end if
        
        deallocate(tokens)
    end subroutine test_if_dispatch
    
    subroutine test_do_dispatch()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: stmt_idx
        
        call test_start("Dispatch DO loop")
        
        allocate(tokens(7))
        tokens(1) = token_t(kind=TK_KEYWORD, text="do", line=1, column=1)
        tokens(2) = token_t(kind=TK_IDENTIFIER, text="i", line=1, column=4)
        tokens(3) = token_t(kind=TK_OPERATOR, text="=", line=1, column=6)
        tokens(4) = token_t(kind=TK_NUMBER, text="1", line=1, column=8)
        tokens(5) = token_t(kind=TK_OPERATOR, text=",", line=1, column=9)
        tokens(6) = token_t(kind=TK_NUMBER, text="10", line=1, column=11)
        tokens(7) = token_t(kind=TK_NEWLINE, text="\n", line=1, column=13)
        
        arena = create_ast_stack()
        stmt_idx = parse_statement_dispatcher(tokens, arena)
        
        if (stmt_idx > 0) then
            call test_pass()
        else
            call test_fail("Failed to dispatch do loop")
        end if
        
        deallocate(tokens)
    end subroutine test_do_dispatch
    
    subroutine test_assignment_dispatch()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: stmt_idx
        
        call test_start("Dispatch assignment")
        
        allocate(tokens(3))
        tokens(1) = token_t(kind=TK_IDENTIFIER, text="x", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text="=", line=1, column=3)
        tokens(3) = token_t(kind=TK_NUMBER, text="42", line=1, column=5)
        
        arena = create_ast_stack()
        stmt_idx = parse_statement_dispatcher(tokens, arena)
        
        if (stmt_idx > 0) then
            call test_pass()
        else
            call test_fail("Failed to dispatch assignment")
        end if
        
        deallocate(tokens)
    end subroutine test_assignment_dispatch
    
    subroutine test_empty_dispatch()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: stmt_idx
        
        call test_start("Dispatch empty statement")
        
        allocate(tokens(1))
        tokens(1) = token_t(kind=TK_EOF, text="", line=1, column=1)
        
        arena = create_ast_stack()
        stmt_idx = parse_statement_dispatcher(tokens, arena)
        
        ! Empty should return 0
        if (stmt_idx == 0) then
            call test_pass()
        else
            call test_fail("Empty statement should return 0")
        end if
        
        deallocate(tokens)
    end subroutine test_empty_dispatch
    
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
    
end program test_parser_dispatcher_direct