program test_multi_variable_declarations
    use lexer_core, only: token_t, TK_KEYWORD, TK_IDENTIFIER, TK_OPERATOR, TK_EOF
    use parser_state_module
    use parser_declarations, only: parse_declaration
    use ast_core
    implicit none

    logical :: all_tests_passed

    print *, "=== Multi-Variable Declaration Parser Tests ==="
    print *

    all_tests_passed = .true.

    ! Test multi-variable declaration parsing
    if (.not. test_multi_variable_parsing()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "All multi-variable declaration tests passed!"
        stop 0
    else
        print *, "Some multi-variable declaration tests failed"
        stop 1
    end if

contains

    function test_multi_variable_parsing() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        integer :: decl_index, initial_size, final_size
        integer :: i
        logical :: found_x, found_y
        character(len=:), allocatable :: error_msg

        passed = .false.
        found_x = .false.
        found_y = .false.

        print *, "Testing multi-variable declaration 'integer :: x, y'..."

        ! Create test tokens for "integer :: x, y"
        allocate (tokens(6))
        tokens(1) = token_t(TK_KEYWORD, "integer", 1, 1)
        tokens(2) = token_t(TK_OPERATOR, "::", 1, 9)
        tokens(3) = token_t(TK_IDENTIFIER, "x", 1, 12)
        tokens(4) = token_t(TK_OPERATOR, ",", 1, 13)
        tokens(5) = token_t(TK_IDENTIFIER, "y", 1, 15)
        tokens(6) = token_t(TK_EOF, "", 1, 16)

        ! Create parser and arena
        parser = create_parser_state(tokens)
        arena = create_ast_arena()
        
        ! Record initial arena size
        initial_size = arena%size

        ! Parse declaration
        decl_index = parse_declaration(parser, arena)
        
        ! Record final arena size
        final_size = arena%size

        print *, "Declaration index returned:", decl_index
        print *, "Arena size before parsing:", initial_size
        print *, "Arena size after parsing:", final_size
        print *, "Number of nodes created:", final_size - initial_size

        if (decl_index > 0) then
            ! Check all declaration nodes in the arena
            do i = initial_size + 1, final_size
                select type (node => arena%entries(i)%node)
                type is (declaration_node)
                    print *, "  Found declaration for variable:", trim(node%var_name)
                    if (node%var_name == "x") found_x = .true.
                    if (node%var_name == "y") found_y = .true.
                end select
            end do
            
            if (found_x .and. found_y) then
                print *, "PASS: Both variables 'x' and 'y' were parsed correctly"
                print *, "      The parser creates declaration nodes for all variables"
                passed = .true.
            else if (found_x .and. .not. found_y) then
                print *, "FAIL: Only variable 'x' was parsed, 'y' was missed"
            else
                print *, "FAIL: Unexpected parsing result"
            end if
        else
            print *, "ERROR: No declaration created"
        end if

        ! Clean up
        call arena%clear()

    end function test_multi_variable_parsing

end program test_multi_variable_declarations