program test_parser_control_flow_direct
    use parser_control_flow_module
    use parser_state_module
    use lexer_core
    use ast_core
    use ast_factory
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== Parser Control Flow Direct Tests ==="
    
    ! Test if statement parsing
    call test_if_statements()
    
    ! Test do loop parsing
    call test_do_loops()
    
    ! Test select case parsing
    call test_select_case()
    
    ! Test where constructs
    call test_where_constructs()
    
    ! Test forall constructs - not implemented yet
    ! call test_forall_constructs()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All parser control flow tests passed!"
        stop 0
    else
        print *, "Some parser control flow tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_if_statements()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_idx
        
        call test_start("If-then statement")
        
        ! Create tokens: if (x > 0) then
        allocate(tokens(6))
        tokens(1) = token_t(kind=TK_KEYWORD, text="if", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text="(", line=1, column=4)
        tokens(3) = token_t(kind=TK_IDENTIFIER, text="x", line=1, column=5)
        tokens(4) = token_t(kind=TK_OPERATOR, text=">", line=1, column=7)
        tokens(5) = token_t(kind=TK_NUMBER, text="0", line=1, column=9)
        tokens(6) = token_t(kind=TK_OPERATOR, text=")", line=1, column=10)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        
        stmt_idx = parse_if(parser, arena)
        
        if (stmt_idx > 0) then
            ! Can only verify that parsing succeeded
            call test_pass()
        else
            call test_fail("Failed to parse if statement")
        end if
        
        call test_start("If-then-else statement")
        
        ! Create tokens: if (x > 0) then ... else ... end if
        deallocate(tokens)
        allocate(tokens(10))
        tokens(1) = token_t(kind=TK_KEYWORD, text="if", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text="(", line=1, column=4)
        tokens(3) = token_t(kind=TK_IDENTIFIER, text="x", line=1, column=5)
        tokens(4) = token_t(kind=TK_OPERATOR, text=">", line=1, column=7)
        tokens(5) = token_t(kind=TK_NUMBER, text="0", line=1, column=9)
        tokens(6) = token_t(kind=TK_OPERATOR, text=")", line=1, column=10)
        tokens(7) = token_t(kind=TK_KEYWORD, text="then", line=1, column=12)
        tokens(8) = token_t(kind=TK_KEYWORD, text="else", line=2, column=1)
        tokens(9) = token_t(kind=TK_KEYWORD, text="end", line=3, column=1)
        tokens(10) = token_t(kind=TK_KEYWORD, text="if", line=3, column=5)
        
        parser = create_parser_state(tokens)
        call arena%clear()
        
        stmt_idx = parse_if(parser, arena)
        
        if (stmt_idx > 0) then
            ! Can only verify that parsing succeeded
            call test_pass()
        else
            call test_fail("Failed to parse if-then-else")
        end if
        
        deallocate(tokens)
    end subroutine test_if_statements
    
    subroutine test_do_loops()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_idx
        
        call test_start("Do loop with bounds")
        
        ! Create tokens: do i = 1, 10
        allocate(tokens(7))
        tokens(1) = token_t(kind=TK_KEYWORD, text="do", line=1, column=1)
        tokens(2) = token_t(kind=TK_IDENTIFIER, text="i", line=1, column=4)
        tokens(3) = token_t(kind=TK_OPERATOR, text="=", line=1, column=6)
        tokens(4) = token_t(kind=TK_NUMBER, text="1", line=1, column=8)
        tokens(5) = token_t(kind=TK_OPERATOR, text=",", line=1, column=9)
        tokens(6) = token_t(kind=TK_NUMBER, text="10", line=1, column=11)
        tokens(7) = token_t(kind=TK_NEWLINE, text="\n", line=1, column=13)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        
        stmt_idx = parse_do_loop(parser, arena)
        
        if (stmt_idx > 0) then
            ! Can only verify that parsing succeeded
            call test_pass()
        else
            call test_fail("Failed to parse do loop")
        end if
        
        call test_start("Do loop with stride")
        
        ! Create tokens: do i = 1, 10, 2
        deallocate(tokens)
        allocate(tokens(9))
        tokens(1) = token_t(kind=TK_KEYWORD, text="do", line=1, column=1)
        tokens(2) = token_t(kind=TK_IDENTIFIER, text="i", line=1, column=4)
        tokens(3) = token_t(kind=TK_OPERATOR, text="=", line=1, column=6)
        tokens(4) = token_t(kind=TK_NUMBER, text="1", line=1, column=8)
        tokens(5) = token_t(kind=TK_OPERATOR, text=",", line=1, column=9)
        tokens(6) = token_t(kind=TK_NUMBER, text="10", line=1, column=11)
        tokens(7) = token_t(kind=TK_OPERATOR, text=",", line=1, column=13)
        tokens(8) = token_t(kind=TK_NUMBER, text="2", line=1, column=15)
        tokens(9) = token_t(kind=TK_NEWLINE, text="\n", line=1, column=16)
        
        parser = create_parser_state(tokens)
        call arena%clear()
        
        stmt_idx = parse_do_loop(parser, arena)
        
        if (stmt_idx > 0) then
            ! Can only verify that parsing succeeded
            call test_pass()
        else
            call test_fail("Failed to parse do loop with stride")
        end if
        
        call test_start("Do while loop")
        
        ! Create tokens: do while (x > 0)
        deallocate(tokens)
        allocate(tokens(7))
        tokens(1) = token_t(kind=TK_KEYWORD, text="do", line=1, column=1)
        tokens(2) = token_t(kind=TK_KEYWORD, text="while", line=1, column=4)
        tokens(3) = token_t(kind=TK_OPERATOR, text="(", line=1, column=10)
        tokens(4) = token_t(kind=TK_IDENTIFIER, text="x", line=1, column=11)
        tokens(5) = token_t(kind=TK_OPERATOR, text=">", line=1, column=13)
        tokens(6) = token_t(kind=TK_NUMBER, text="0", line=1, column=15)
        tokens(7) = token_t(kind=TK_OPERATOR, text=")", line=1, column=16)
        
        parser = create_parser_state(tokens)
        call arena%clear()
        
        stmt_idx = parse_do_while(parser, arena)
        
        if (stmt_idx > 0) then
            ! Can only verify that parsing succeeded
            call test_pass()
        else
            call test_fail("Failed to parse do while")
        end if
        
        deallocate(tokens)
    end subroutine test_do_loops
    
    subroutine test_select_case()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_idx
        
        call test_start("Select case statement")
        
        ! Create tokens: select case (x)
        allocate(tokens(5))
        tokens(1) = token_t(kind=TK_KEYWORD, text="select", line=1, column=1)
        tokens(2) = token_t(kind=TK_KEYWORD, text="case", line=1, column=8)
        tokens(3) = token_t(kind=TK_OPERATOR, text="(", line=1, column=13)
        tokens(4) = token_t(kind=TK_IDENTIFIER, text="x", line=1, column=14)
        tokens(5) = token_t(kind=TK_OPERATOR, text=")", line=1, column=15)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        
        stmt_idx = parse_select_case(parser, arena)
        
        if (stmt_idx > 0) then
            ! Can only verify that parsing succeeded
            call test_pass()
        else
            call test_fail("Failed to parse select case")
        end if
        
        ! Case block test would need different structure
        ! Just skip this test for now
        
        deallocate(tokens)
    end subroutine test_select_case
    
    subroutine test_where_constructs()
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        type(token_t), allocatable :: tokens(:)
        integer :: stmt_idx
        
        call test_start("Where statement")
        
        ! Create tokens: where (arr > 0)
        allocate(tokens(6))
        tokens(1) = token_t(kind=TK_KEYWORD, text="where", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text="(", line=1, column=7)
        tokens(3) = token_t(kind=TK_IDENTIFIER, text="arr", line=1, column=8)
        tokens(4) = token_t(kind=TK_OPERATOR, text=">", line=1, column=12)
        tokens(5) = token_t(kind=TK_NUMBER, text="0", line=1, column=14)
        tokens(6) = token_t(kind=TK_OPERATOR, text=")", line=1, column=15)
        
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        
        stmt_idx = parse_where_construct(parser, arena)
        
        if (stmt_idx > 0) then
            ! Can only verify that parsing succeeded
            call test_pass()
        else
            call test_fail("Failed to parse where statement")
        end if
        
        call test_start("Where construct with elsewhere")
        
        ! Create tokens: where (mask) ... elsewhere ... end where
        deallocate(tokens)
        allocate(tokens(9))
        tokens(1) = token_t(kind=TK_KEYWORD, text="where", line=1, column=1)
        tokens(2) = token_t(kind=TK_OPERATOR, text="(", line=1, column=7)
        tokens(3) = token_t(kind=TK_IDENTIFIER, text="mask", line=1, column=8)
        tokens(4) = token_t(kind=TK_OPERATOR, text=")", line=1, column=12)
        tokens(5) = token_t(kind=TK_NEWLINE, text="\n", line=1, column=13)
        tokens(6) = token_t(kind=TK_KEYWORD, text="elsewhere", line=2, column=1)
        tokens(7) = token_t(kind=TK_NEWLINE, text="\n", line=2, column=9)
        tokens(8) = token_t(kind=TK_KEYWORD, text="end", line=3, column=1)
        tokens(9) = token_t(kind=TK_KEYWORD, text="where", line=3, column=5)
        
        parser = create_parser_state(tokens)
        call arena%clear()
        
        stmt_idx = parse_where_construct(parser, arena)
        
        if (stmt_idx > 0) then
            ! Can only verify that parsing succeeded
            call test_pass()
        else
            call test_fail("Failed to parse where construct")
        end if
        
        deallocate(tokens)
    end subroutine test_where_constructs
!    
!    subroutine test_forall_constructs()
!        type(parser_state_t) :: parser
!        type(ast_arena_t) :: arena
!        type(token_t), allocatable :: tokens(:)
!        integer :: stmt_idx
!        
!        call test_start("Forall statement")
!        
!        ! Create tokens: forall (i=1:n)
!        allocate(tokens(9))
!        tokens(1) = token_t(kind=TK_KEYWORD, text="forall", line=1, column=1)
!        tokens(2) = token_t(kind=TK_OPERATOR, text="(", line=1, column=8)
!        tokens(3) = token_t(kind=TK_IDENTIFIER, text="i", line=1, column=9)
!        tokens(4) = token_t(kind=TK_OPERATOR, text="=", line=1, column=10)
!        tokens(5) = token_t(kind=TK_NUMBER, text="1", line=1, column=11)
!        tokens(6) = token_t(kind=TK_OPERATOR, text=":", line=1, column=12)
!        tokens(7) = token_t(kind=TK_IDENTIFIER, text="n", line=1, column=13)
!        tokens(8) = token_t(kind=TK_OPERATOR, text=")", line=1, column=14)
!        tokens(9) = token_t(kind=TK_NEWLINE, text="\n", line=1, column=15)
!        
!        arena = create_ast_arena()
!        parser = create_parser_state(tokens)
!        
!        stmt_idx = parse_forall_statement(parser, arena)
!        
!        if (stmt_idx > 0) then
!            ! Can only verify that parsing succeeded
!            call test_pass()
!        else
!            call test_fail("Failed to parse forall")
!        end if
!        
!        call test_start("Forall with mask")
!        
!        ! Create tokens: forall (i=1:n, j=1:m, i<j)
!        deallocate(tokens)
!        allocate(tokens(18))
!        tokens(1) = token_t(kind=TK_KEYWORD, text="forall", line=1, column=1)
!        tokens(2) = token_t(kind=TK_OPERATOR, text="(", line=1, column=8)
!        tokens(3) = token_t(kind=TK_IDENTIFIER, text="i", line=1, column=9)
!        tokens(4) = token_t(kind=TK_OPERATOR, text="=", line=1, column=10)
!        tokens(5) = token_t(kind=TK_NUMBER, text="1", line=1, column=11)
!        tokens(6) = token_t(kind=TK_OPERATOR, text=":", line=1, column=12)
!        tokens(7) = token_t(kind=TK_IDENTIFIER, text="n", line=1, column=13)
!        tokens(8) = token_t(kind=TK_OPERATOR, text=",", line=1, column=14)
!        tokens(9) = token_t(kind=TK_IDENTIFIER, text="j", line=1, column=16)
!        tokens(10) = token_t(kind=TK_OPERATOR, text="=", line=1, column=17)
!        tokens(11) = token_t(kind=TK_NUMBER, text="1", line=1, column=18)
!        tokens(12) = token_t(kind=TK_OPERATOR, text=":", line=1, column=19)
!        tokens(13) = token_t(kind=TK_IDENTIFIER, text="m", line=1, column=20)
!        tokens(14) = token_t(kind=TK_OPERATOR, text=",", line=1, column=21)
!        tokens(15) = token_t(kind=TK_IDENTIFIER, text="i", line=1, column=23)
!        tokens(16) = token_t(kind=TK_OPERATOR, text="<", line=1, column=24)
!        tokens(17) = token_t(kind=TK_IDENTIFIER, text="j", line=1, column=25)
!        tokens(18) = token_t(kind=TK_OPERATOR, text=")", line=1, column=26)
!        
!        parser = create_parser_state(tokens)
!        call arena%clear()
!        
!        stmt_idx = parse_forall_statement(parser, arena)
!        
!        if (stmt_idx > 0) then
!            ! Can only verify that parsing succeeded
!            call test_pass()
!        else
!            call test_fail("Failed to parse forall with mask")
!        end if
!        
!        deallocate(tokens)
!    end subroutine test_forall_constructs
    
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
    
end program test_parser_control_flow_direct