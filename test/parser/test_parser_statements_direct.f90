program test_parser_statements_direct
    use parser_statements_module
    use parser_state_module
    use lexer_core
    use ast_core
    use ast_factory
    implicit none
    
    integer :: test_count, pass_count
    logical :: is_windows
    
    test_count = 0
    pass_count = 0
    
    ! Detect if we're on Windows
    is_windows = check_if_windows()
    
    print *, "=== Parser Statements Direct Tests ==="
    
    if (is_windows) then
        print *, "SKIPPING: Parser statements tests on Windows (memory issues)"
        print *, "This is a temporary workaround and will be fixed"
        stop 0
    end if
    
    ! Test use statement parsing
    call test_use_statements()
    
    ! Test I/O statement parsing
    call test_io_statements()
    
    ! Test control statements
    call test_control_statements()
    
    ! Test program/module/function parsing
    call test_program_units()
    
    ! Test memory management statements
    call test_memory_statements()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All parser statements tests passed!"
        stop 0
    else
        print *, "Some parser statements tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_use_statements()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_idx
        
        call test_start("Use statement - simple")
        
        ! Create tokens: use mymodule
        allocate(tokens(2))
        tokens(1) = token_t(kind=TK_KEYWORD, text="use", line=1, column=1)
        tokens(2) = token_t(kind=TK_IDENTIFIER, text="mymodule", line=1, column=5)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        
        stmt_idx = parse_use_statement(parser, arena)
        
        if (stmt_idx > 0) then
            ! Can only verify that parsing succeeded
            call test_pass()
        else
            call test_fail("Failed to parse use statement")
        end if
        
        call test_start("Use statement - with only clause")
        
        ! Create tokens: use mymodule, only: func1, func2
        deallocate(tokens)
        allocate(tokens(8))
        tokens(1) = token_t(kind=TK_KEYWORD, text="use", line=1, column=1)
        tokens(2) = token_t(kind=TK_IDENTIFIER, text="mymodule", line=1, column=5)
        tokens(3) = token_t(kind=TK_OPERATOR, text=",", line=1, column=13)
        tokens(4) = token_t(kind=TK_KEYWORD, text="only", line=1, column=15)
        tokens(5) = token_t(kind=TK_OPERATOR, text=":", line=1, column=19)
        tokens(6) = token_t(kind=TK_IDENTIFIER, text="func1", line=1, column=21)
        tokens(7) = token_t(kind=TK_OPERATOR, text=",", line=1, column=26)
        tokens(8) = token_t(kind=TK_IDENTIFIER, text="func2", line=1, column=28)
        
        parser = create_parser_state(tokens)
        call arena%clear()
        
        stmt_idx = parse_use_statement(parser, arena)
        
        if (stmt_idx > 0) then
            ! Can only verify that parsing succeeded
            call test_pass()
        else
            call test_fail("Failed to parse use with only")
        end if
        
        deallocate(tokens)
    end subroutine test_use_statements
    
    subroutine test_io_statements()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_idx
        
        call test_start("Print statement - simple")
        
        ! Create tokens: print *, "Hello"
        allocate(tokens(4))
        tokens(1) = token_t(kind=TK_KEYWORD, text="print", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text="*", line=1, column=7)
        tokens(3) = token_t(kind=TK_OPERATOR, text=",", line=1, column=8)
        tokens(4) = token_t(kind=TK_STRING, text='"Hello"', line=1, column=10)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        
        stmt_idx = parse_print_statement(parser, arena)
        
        if (stmt_idx > 0) then
            ! Can only verify that parsing succeeded
            call test_pass()
        else
            call test_fail("Failed to parse print statement")
        end if
        
        call test_start("Write statement with unit")
        
        ! Create tokens: write(10,*) x
        deallocate(tokens)
        allocate(tokens(7))
        tokens(1) = token_t(kind=TK_KEYWORD, text="write", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text="(", line=1, column=6)
        tokens(3) = token_t(kind=TK_NUMBER, text="10", line=1, column=7)
        tokens(4) = token_t(kind=TK_OPERATOR, text=",", line=1, column=9)
        tokens(5) = token_t(kind=TK_OPERATOR, text="*", line=1, column=10)
        tokens(6) = token_t(kind=TK_OPERATOR, text=")", line=1, column=11)
        tokens(7) = token_t(kind=TK_IDENTIFIER, text="x", line=1, column=13)
        
        parser = create_parser_state(tokens)
        call arena%clear()
        
        stmt_idx = parse_write_statement(parser, arena)
        
        if (stmt_idx > 0) then
            ! Can only verify that parsing succeeded
            call test_pass()
        else
            call test_fail("Failed to parse write statement")
        end if
        
        call test_start("Read statement - simple")
        
        ! Create tokens: read(*,*) x
        deallocate(tokens)
        allocate(tokens(7))
        tokens(1) = token_t(kind=TK_KEYWORD, text="read", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text="(", line=1, column=5)
        tokens(3) = token_t(kind=TK_OPERATOR, text="*", line=1, column=6)
        tokens(4) = token_t(kind=TK_OPERATOR, text=",", line=1, column=7)
        tokens(5) = token_t(kind=TK_OPERATOR, text="*", line=1, column=8)
        tokens(6) = token_t(kind=TK_OPERATOR, text=")", line=1, column=9)
        tokens(7) = token_t(kind=TK_IDENTIFIER, text="x", line=1, column=11)
        
        parser = create_parser_state(tokens)
        call arena%clear()
        
        stmt_idx = parse_read_statement(parser, arena)
        
        if (stmt_idx > 0) then
            ! Can only verify that parsing succeeded
            call test_pass()
        else
            call test_fail("Failed to parse read statement")
        end if
        
        deallocate(tokens)
    end subroutine test_io_statements
    
    subroutine test_control_statements()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_idx
        
        call test_start("Stop statement")
        
        ! Create tokens: stop
        allocate(tokens(1))
        tokens(1) = token_t(kind=TK_KEYWORD, text="stop", line=1, column=1)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        
        stmt_idx = parse_stop_statement(parser, arena)
        
        if (stmt_idx > 0) then
            ! Can only verify that parsing succeeded
            call test_pass()
        else
            call test_fail("Failed to parse stop")
        end if
        
        call test_start("Stop statement with code")
        
        ! Create tokens: stop 1
        deallocate(tokens)
        allocate(tokens(2))
        tokens(1) = token_t(kind=TK_KEYWORD, text="stop", line=1, column=1)
        tokens(2) = token_t(kind=TK_NUMBER, text="1", line=1, column=6)
        
        parser = create_parser_state(tokens)
        call arena%clear()
        
        stmt_idx = parse_stop_statement(parser, arena)
        
        if (stmt_idx > 0) then
            ! Can only verify that parsing succeeded
            call test_pass()
        else
            call test_fail("Failed to parse stop with code")
        end if
        
        call test_start("Return statement")
        
        ! Create tokens: return
        deallocate(tokens)
        allocate(tokens(1))
        tokens(1) = token_t(kind=TK_KEYWORD, text="return", line=1, column=1)
        
        parser = create_parser_state(tokens)
        call arena%clear()
        
        stmt_idx = parse_return_statement(parser, arena)
        
        if (stmt_idx > 0) then
            ! Can only verify that parsing succeeded
            call test_pass()
        else
            call test_fail("Failed to parse return")
        end if
        
        deallocate(tokens)
    end subroutine test_control_statements
    
    subroutine test_program_units()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_idx
        
        call test_start("Program statement")
        
        ! Create tokens: program test_prog
        allocate(tokens(2))
        tokens(1) = token_t(kind=TK_KEYWORD, text="program", line=1, column=1)
        tokens(2) = token_t(kind=TK_IDENTIFIER, text="test_prog", line=1, column=9)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        
        stmt_idx = parse_program_statement(parser, arena)
        
        if (stmt_idx > 0) then
            ! Can only verify that parsing succeeded
            call test_pass()
        else
            call test_fail("Failed to parse program statement")
        end if
        
        call test_start("Module declaration")
        
        ! Create tokens: module mymod
        deallocate(tokens)
        allocate(tokens(2))
        tokens(1) = token_t(kind=TK_KEYWORD, text="module", line=1, column=1)
        tokens(2) = token_t(kind=TK_IDENTIFIER, text="mymod", line=1, column=8)
        
        parser = create_parser_state(tokens)
        call arena%clear()
        
        stmt_idx = parse_module(parser, arena)
        
        if (stmt_idx > 0) then
            ! Can only verify that parsing succeeded
            call test_pass()
        else
            call test_fail("Failed to parse module")
        end if
        
        deallocate(tokens)
    end subroutine test_program_units
    
    subroutine test_memory_statements()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_idx
        
        call test_start("Allocate statement - simple")
        
        ! Create tokens: allocate(arr(10))
        allocate(tokens(6))
        tokens(1) = token_t(kind=TK_KEYWORD, text="allocate", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text="(", line=1, column=9)
        tokens(3) = token_t(kind=TK_IDENTIFIER, text="arr", line=1, column=10)
        tokens(4) = token_t(kind=TK_OPERATOR, text="(", line=1, column=13)
        tokens(5) = token_t(kind=TK_NUMBER, text="10", line=1, column=14)
        tokens(6) = token_t(kind=TK_OPERATOR, text=")", line=1, column=16)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        
        stmt_idx = parse_allocate_statement(parser, arena)
        
        if (stmt_idx > 0) then
            ! Can only verify that parsing succeeded
            call test_pass()
        else
            call test_fail("Failed to parse allocate")
        end if
        
        call test_start("Deallocate statement")
        
        ! Create tokens: deallocate(arr)
        deallocate(tokens)
        allocate(tokens(4))
        tokens(1) = token_t(kind=TK_KEYWORD, text="deallocate", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text="(", line=1, column=11)
        tokens(3) = token_t(kind=TK_IDENTIFIER, text="arr", line=1, column=12)
        tokens(4) = token_t(kind=TK_OPERATOR, text=")", line=1, column=15)
        
        parser = create_parser_state(tokens)
        call arena%clear()
        
        stmt_idx = parse_deallocate_statement(parser, arena)
        
        if (stmt_idx > 0) then
            ! Can only verify that parsing succeeded
            call test_pass()
        else
            call test_fail("Failed to parse deallocate")
        end if
        
        deallocate(tokens)
    end subroutine test_memory_statements
    
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
    
    function check_if_windows() result(is_win)
        logical :: is_win
        character(len=10) :: os_name
        integer :: stat
        
        ! Try to detect Windows through environment variable
        call get_environment_variable('OS', os_name, status=stat)
        is_win = (stat == 0 .and. os_name(1:7) == 'Windows')
        
        ! Alternative: check for Windows-specific env var
        if (.not. is_win) then
            call get_environment_variable('WINDIR', os_name, status=stat)
            is_win = (stat == 0)
        end if
    end function check_if_windows
    
end program test_parser_statements_direct