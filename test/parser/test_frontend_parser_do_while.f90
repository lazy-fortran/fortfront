! Test do while loop parsing
program test_frontend_parser_do_while
    use parser_core
    use parser_control_flow_module, only: parse_do_while
    use parser_state_module, only: parser_state_t, create_parser_state
    use lexer_core, only: token_t, TK_KEYWORD, TK_OPERATOR, TK_IDENTIFIER, TK_NUMBER, TK_EOF
    use ast_core, only: ast_arena_t, create_ast_arena, do_while_node
    use iso_fortran_env, only: real64
    implicit none

    integer :: tests_passed = 0
    integer :: tests_failed = 0

    ! Test basic do while loop
    call test_basic_do_while()

    ! Print test summary
    print *, "Do while parser tests:"
    print *, "  Passed:", tests_passed
    print *, "  Failed:", tests_failed
    if (tests_failed == 0) then
        print *, "All tests passed!"
    else
        print *, "Some tests failed"
        stop 1
    end if

contains

    subroutine test_basic_do_while()
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        integer :: loop_index

        print *, "Testing basic do while loop parsing..."

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

        ! Create arena
        arena = create_ast_arena()

        parser = create_parser_state(tokens)
        loop_index = parse_do_while(parser, arena)

        if (loop_index > 0) then
            if (allocated(arena%entries(loop_index)%node)) then
                select type (loop_node => arena%entries(loop_index)%node)
                type is (do_while_node)
                    print *, "  ✓ Parsed as do_while_node"
                    tests_passed = tests_passed + 1

                    ! Check condition
                    if (loop_node%condition_index > 0) then
                        print *, "  ✓ Has condition"
                        tests_passed = tests_passed + 1
                    else
                        print *, "  ✗ Missing condition"
                        tests_failed = tests_failed + 1
                    end if
                class default
                    print *, "  ✗ Wrong node type"
                    tests_failed = tests_failed + 1
                end select
            else
                print *, "  ✗ Node not allocated in arena"
                tests_failed = tests_failed + 1
            end if
        else
            print *, "  ✗ Failed to parse do while loop"
            tests_failed = tests_failed + 1
        end if

        ! Clean up arena
        call arena%clear()
        deallocate (tokens)
    end subroutine test_basic_do_while

end program test_frontend_parser_do_while
