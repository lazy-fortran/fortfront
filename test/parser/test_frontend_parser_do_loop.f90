program test_frontend_parser_do_loop
    use lexer_core
    use parser_dispatcher_module, only: parse_statement_dispatcher
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_control_flow_module, only: parse_do_loop
    use ast_core, only: ast_arena_t, create_ast_arena, do_loop_node
    use frontend, only: find_program_unit_boundary
    implicit none

    integer :: test_count = 0
    integer :: tests_passed = 0

    call test_do_loop_parsing()
    call test_do_loop_boundary_detection()

    print *, "DO LOOP PARSER TESTS: ", tests_passed, " of ", test_count, " passed"

    if (tests_passed /= test_count) then
        stop 1
    end if

contains

    subroutine test_do_loop_parsing()
        ! Test parsing of simple do loop
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        integer :: loop_index

        test_count = test_count + 1
        print *, "Test 1: Simple do loop parsing"

        ! Create tokens for "do i = 1, 5"
        allocate (tokens(7))
        tokens(1) = token_t(TK_KEYWORD, "do", 1, 1)
        tokens(2) = token_t(TK_IDENTIFIER, "i", 1, 4)
        tokens(3) = token_t(TK_OPERATOR, "=", 1, 6)
        tokens(4) = token_t(TK_NUMBER, "1", 1, 8)
        tokens(5) = token_t(TK_OPERATOR, ",", 1, 9)
        tokens(6) = token_t(TK_NUMBER, "5", 1, 11)
        tokens(7) = token_t(TK_EOF, "", 1, 12)

        ! Create arena
        arena = create_ast_arena()
        
        ! Parse do loop directly
        parser = create_parser_state(tokens)
        loop_index = parse_do_loop(parser, arena)

        if (loop_index > 0) then
            if (allocated(arena%entries(loop_index)%node)) then
                select type (loop_node => arena%entries(loop_index)%node)
                type is (do_loop_node)
                    if (loop_node%var_name == "i") then
                        print *, "  ✓ PASS: Do loop variable parsed correctly"
                        tests_passed = tests_passed + 1
                    else
                       print *, "  ✗ FAIL: Expected var_name='i', got: ", loop_node%var_name
                    end if
                class default
                    print *, "  ✗ FAIL: Expected do_loop_node, got different type"
                end select
            else
                print *, "  ✗ FAIL: Node not allocated in arena"
            end if
        else
            print *, "  ✗ FAIL: parse_do_loop returned invalid index"
        end if
        
        ! Clean up arena
        call arena%clear()

    end subroutine test_do_loop_parsing

    subroutine test_do_loop_boundary_detection()
        ! Test boundary detection for do loops
        type(token_t), allocatable :: tokens(:)
        integer :: unit_start, unit_end

        test_count = test_count + 1
        print *, "Test 2: Do loop boundary detection"

        ! Create tokens for complete do loop
        allocate (tokens(13))
        tokens(1) = token_t(TK_KEYWORD, "do", 1, 1)
        tokens(2) = token_t(TK_IDENTIFIER, "i", 1, 4)
        tokens(3) = token_t(TK_OPERATOR, "=", 1, 6)
        tokens(4) = token_t(TK_NUMBER, "1", 1, 8)
        tokens(5) = token_t(TK_OPERATOR, ",", 1, 9)
        tokens(6) = token_t(TK_NUMBER, "5", 1, 11)
        tokens(7) = token_t(TK_NEWLINE, "", 1, 12)
        tokens(8) = token_t(TK_KEYWORD, "print", 2, 1)
        tokens(9) = token_t(TK_OPERATOR, "*", 2, 7)
        tokens(10) = token_t(TK_IDENTIFIER, "i", 2, 10)
        tokens(11) = token_t(TK_KEYWORD, "end", 3, 1)
        tokens(12) = token_t(TK_KEYWORD, "do", 3, 5)
        tokens(13) = token_t(TK_EOF, "", 3, 7)

        call find_program_unit_boundary(tokens, 1, unit_start, unit_end, .false.)

        if (unit_start == 1 .and. unit_end == 12) then
            print *, "  ✓ PASS: Do loop boundary detected correctly"
            tests_passed = tests_passed + 1
        else
           print *, "  ✗ FAIL: Expected boundary 1-12, got: ", unit_start, "-", unit_end
        end if

    end subroutine test_do_loop_boundary_detection

end program test_frontend_parser_do_loop
