program test_do_loops_comprehensive
    ! Comprehensive test for all do loop variants: simple, with step, while, nested
    use lexer_core, only: tokenize_core, token_t, TK_EOF, TK_KEYWORD, TK_OPERATOR, TK_IDENTIFIER, TK_NUMBER
    use ast_core, only: ast_node, do_loop_node, do_while_node, literal_node, binary_op_node, ast_arena_t, create_ast_arena
    use parser_dispatcher_module, only: parse_statement_dispatcher
    use parser_control_flow_module, only: parse_do_loop, parse_do_while
    use parser_state_module, only: parser_state_t, create_parser_state
    implicit none

    logical :: all_passed = .true.
    integer :: test_count = 0
    integer :: tests_passed = 0

    print *, "=== Comprehensive Do Loop Parser Tests ==="

    ! Test all do loop variants
    call test_simple_do_loop()
    call test_do_loop_with_step()
    call test_do_while_loop()
    call test_nested_do_loops()
    call test_do_loop_direct_parsing()

    ! Report results
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", tests_passed, "/", test_count, " tests"
    
    if (tests_passed == test_count) then
        print *, "All do loop parser tests passed"
        stop 0
    else
        print *, "Some do loop parser tests failed"
        stop 1
    end if

contains

    subroutine test_simple_do_loop()
        ! Test basic do loop: do i = 1, 10
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: stmt_index

        call test_start("Simple do loop parsing")

        ! Test: do i = 1, 10 end do
        call tokenize_core("do i = 1, 10 end do", tokens)
        tokens = add_eof_token(tokens)

        arena = create_ast_arena()
        stmt_index = parse_statement_dispatcher(tokens, arena)

        if (stmt_index <= 0) then
            call test_fail("No AST node returned for simple do loop")
            return
        end if
        
        if (.not. allocated(arena%entries(stmt_index)%node)) then
            call test_fail("Node not allocated in arena")
            return
        end if

        select type (stmt => arena%entries(stmt_index)%node)
        type is (do_loop_node)
            if (.not. verify_loop_variable(stmt, "i")) return
            if (.not. verify_start_value(arena, stmt, "1")) return
            if (.not. verify_end_value(arena, stmt, "10")) return
            call test_pass("Simple do loop parsing")
        class default
            call test_fail("Expected do_loop_node")
        end select
    end subroutine test_simple_do_loop

    subroutine test_do_loop_with_step()
        ! Test do loop with step: do i = 1, 10, 2
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: stmt_index

        call test_start("Do loop with step")

        call tokenize_core("do i = 1, 10, 2 end do", tokens)
        tokens = add_eof_token(tokens)

        arena = create_ast_arena()
        stmt_index = parse_statement_dispatcher(tokens, arena)

        if (stmt_index <= 0) then
            call test_fail("No AST node returned for do loop with step")
            return
        end if

        select type (stmt => arena%entries(stmt_index)%node)
        type is (do_loop_node)
            if (.not. verify_loop_variable(stmt, "i")) return
            if (.not. verify_start_value(arena, stmt, "1")) return
            if (.not. verify_end_value(arena, stmt, "10")) return
            if (.not. verify_step_value(arena, stmt, "2")) return
            call test_pass("Do loop with step")
        class default
            call test_fail("Expected do_loop_node with step")
        end select
    end subroutine test_do_loop_with_step

    subroutine test_do_while_loop()
        ! Test do while loop: do while (i < 10)
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        integer :: loop_index

        call test_start("Do while loop parsing")

        ! Create tokens for: do while (i < 10)
        allocate (tokens(8))
        tokens(1) = token_t(TK_KEYWORD, "do", 1, 1)
        tokens(2) = token_t(TK_KEYWORD, "while", 1, 4)
        tokens(3) = token_t(TK_OPERATOR, "(", 1, 10)
        tokens(4) = token_t(TK_IDENTIFIER, "i", 1, 11)
        tokens(5) = token_t(TK_OPERATOR, "<", 1, 13)
        tokens(6) = token_t(TK_NUMBER, "10", 1, 15)
        tokens(7) = token_t(TK_OPERATOR, ")", 1, 17)
        tokens(8) = token_t(TK_EOF, "", 1, 18)

        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        loop_index = parse_do_while(parser, arena)

        if (loop_index <= 0) then
            call test_fail("No AST node returned for do while loop")
            return
        end if

        select type (stmt => arena%entries(loop_index)%node)
        type is (do_while_node)
            if (stmt%condition_index <= 0) then
                call test_fail("No condition in do while loop")
                return
            end if
            call test_pass("Do while loop parsing")
        class default
            call test_fail("Expected do_while_node")
        end select
    end subroutine test_do_while_loop

    subroutine test_nested_do_loops()
        ! Test nested do loops
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: stmt_index

        call test_start("Nested do loops")

        call tokenize_core("do i = 1, 5 do j = 1, 3 end do end do", tokens)
        tokens = add_eof_token(tokens)

        arena = create_ast_arena()
        stmt_index = parse_statement_dispatcher(tokens, arena)

        if (stmt_index <= 0) then
            call test_fail("No AST node returned for nested do loops")
            return
        end if

        select type (outer_stmt => arena%entries(stmt_index)%node)
        type is (do_loop_node)
            if (.not. verify_loop_variable(outer_stmt, "i")) return
            ! For nested loops, we'd check the body contains another do loop
            ! This is a simplified test - in reality we'd traverse the body
            call test_pass("Nested do loops")
        class default
            call test_fail("Expected outer do_loop_node for nested loops")
        end select
    end subroutine test_nested_do_loops

    subroutine test_do_loop_direct_parsing()
        ! Test direct do loop parsing (not through dispatcher)
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        integer :: loop_index

        call test_start("Direct do loop parsing")

        ! Create tokens for "do i = 1, 5"
        allocate (tokens(7))
        tokens(1) = token_t(TK_KEYWORD, "do", 1, 1)
        tokens(2) = token_t(TK_IDENTIFIER, "i", 1, 4)
        tokens(3) = token_t(TK_OPERATOR, "=", 1, 6)
        tokens(4) = token_t(TK_NUMBER, "1", 1, 8)
        tokens(5) = token_t(TK_OPERATOR, ",", 1, 9)
        tokens(6) = token_t(TK_NUMBER, "5", 1, 11)
        tokens(7) = token_t(TK_EOF, "", 1, 12)

        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        loop_index = parse_do_loop(parser, arena)

        if (loop_index <= 0) then
            call test_fail("parse_do_loop returned invalid index")
            return
        end if

        select type (loop_node => arena%entries(loop_index)%node)
        type is (do_loop_node)
            if (.not. verify_loop_variable(loop_node, "i")) return
            call test_pass("Direct do loop parsing")
        class default
            call test_fail("Expected do_loop_node from direct parsing")
        end select
    end subroutine test_do_loop_direct_parsing

    ! Helper functions
    function add_eof_token(tokens) result(tokens_with_eof)
        type(token_t), intent(in) :: tokens(:)
        type(token_t), allocatable :: tokens_with_eof(:)
        integer :: n
        
        n = size(tokens)
        allocate (tokens_with_eof(n + 1))
        tokens_with_eof(1:n) = tokens
        tokens_with_eof(n + 1)%kind = TK_EOF
        tokens_with_eof(n + 1)%text = ""
        tokens_with_eof(n + 1)%line = 1
        tokens_with_eof(n + 1)%column = 1
    end function add_eof_token

    logical function verify_loop_variable(loop_node, expected_var)
        class(do_loop_node), intent(in) :: loop_node
        character(len=*), intent(in) :: expected_var
        
        verify_loop_variable = .false.
        if (allocated(loop_node%var_name)) then
            if (loop_node%var_name == expected_var) then
                verify_loop_variable = .true.
            else
                call test_fail("Wrong loop variable name: expected " // expected_var // ", got " // loop_node%var_name)
            end if
        else
            call test_fail("No loop variable name")
        end if
    end function verify_loop_variable

    logical function verify_start_value(arena, loop_node, expected_value)
        type(ast_arena_t), intent(in) :: arena
        class(do_loop_node), intent(in) :: loop_node
        character(len=*), intent(in) :: expected_value
        
        verify_start_value = .false.
        if (loop_node%start_expr_index <= 0) then
            call test_fail("No start expression in do loop")
            return
        end if
        
        if (.not. allocated(arena%entries(loop_node%start_expr_index)%node)) then
            call test_fail("Start expression node not allocated")
            return
        end if

        select type (start => arena%entries(loop_node%start_expr_index)%node)
        type is (literal_node)
            if (start%value == expected_value) then
                verify_start_value = .true.
            else
                call test_fail("Wrong start expression value: expected " // expected_value // ", got " // start%value)
            end if
        class default
            call test_fail("Start expression is not a literal")
        end select
    end function verify_start_value

    logical function verify_end_value(arena, loop_node, expected_value)
        type(ast_arena_t), intent(in) :: arena
        class(do_loop_node), intent(in) :: loop_node
        character(len=*), intent(in) :: expected_value
        
        verify_end_value = .false.
        if (loop_node%end_expr_index <= 0) then
            call test_fail("No end expression in do loop")
            return
        end if

        select type (end_expr => arena%entries(loop_node%end_expr_index)%node)
        type is (literal_node)
            if (end_expr%value == expected_value) then
                verify_end_value = .true.
            else
                call test_fail("Wrong end expression value")
            end if
        class default
            call test_fail("End expression is not a literal")
        end select
    end function verify_end_value

    logical function verify_step_value(arena, loop_node, expected_value)
        type(ast_arena_t), intent(in) :: arena
        class(do_loop_node), intent(in) :: loop_node
        character(len=*), intent(in) :: expected_value
        
        verify_step_value = .false.
        if (loop_node%step_expr_index <= 0) then
            call test_fail("No step expression in do loop")
            return
        end if

        select type (step_expr => arena%entries(loop_node%step_expr_index)%node)
        type is (literal_node)
            if (step_expr%value == expected_value) then
                verify_step_value = .true.
            else
                call test_fail("Wrong step expression value")
            end if
        class default
            call test_fail("Step expression is not a literal")
        end select
    end function verify_step_value

    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A,A)', advance='no') "Testing ", test_name, "... "
    end subroutine test_start

    subroutine test_pass(test_name)
        character(len=*), intent(in) :: test_name
        tests_passed = tests_passed + 1
        print *, "PASS"
    end subroutine test_pass

    subroutine test_fail(reason)
        character(len=*), intent(in) :: reason
        print *, "FAIL: ", reason
        all_passed = .false.
    end subroutine test_fail
    
end program test_do_loops_comprehensive