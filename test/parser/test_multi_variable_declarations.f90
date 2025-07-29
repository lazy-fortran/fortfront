program test_multi_variable_declarations
    use lexer_core, only: token_t, TK_KEYWORD, TK_IDENTIFIER, TK_OPERATOR, TK_EOF
    use parser_state_module
    use parser_declarations_module, only: parse_declaration
    use ast_core
    implicit none

    logical :: all_tests_passed

    print *, "=== Multi-Variable Declaration Parser Tests ==="
    print *

    all_tests_passed = .true.

    ! Test 1: Current behavior - parse_declaration only gets first variable
    if (.not. test_current_parse_declaration_behavior()) all_tests_passed = .false.

    print *
    if (all_tests_passed) then
        print *, "Multi-variable declaration test completed"
        print *, "NOTE: This test shows current parser limitation"
        stop 0
    else
        print *, "Multi-variable declaration test failed"
        stop 1
    end if

contains

    function test_current_parse_declaration_behavior() result(passed)
        logical :: passed
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        type(ast_arena_t) :: arena
        integer :: decl_index
        character(len=:), allocatable :: error_msg

        passed = .false.

        print *, "Testing current parse_declaration behavior with 'integer :: x, y'..."

        ! Create test tokens for "integer :: x, y"
        allocate (tokens(6))
        tokens(1) = token_t(TK_KEYWORD, "integer", 1, 1)
        tokens(2) = token_t(TK_OPERATOR, "::", 1, 9)
        tokens(3) = token_t(TK_IDENTIFIER, "x", 1, 12)
        tokens(4) = token_t(TK_OPERATOR, ",", 1, 13)
        tokens(5) = token_t(TK_IDENTIFIER, "y", 1, 15)
        tokens(6) = token_t(TK_EOF, "", 1, 16)

        ! Create parser
        parser = create_parser_state(tokens)

        ! Parse declaration - this should only create declaration for 'x'
        decl_index = parse_declaration(parser, arena)

        print *, "Declaration index returned:", decl_index

        if (decl_index > 0) then
            ! Check if only one declaration was created for 'x'
            select type (node => arena%entries(decl_index)%node)
            type is (declaration_node)
                if (node%var_name == "x") then
                    print *, "CURRENT BEHAVIOR: Only first variable 'x' parsed"
                 print *, "PROBLEM: Variable 'y' was ignored (consumed by TODO comment)"
                    passed = .true.  ! Test passes because it shows the bug
                else
                    print *, "UNEXPECTED: Variable name is", node%var_name
                end if
            class default
                print *, "ERROR: Node is not a declaration_node"
            end select
        else
            print *, "ERROR: No declaration created"
        end if

        if (passed) then
            print *, "FAIL: Current behavior demonstrates the bug"
            print *, "  - Parser creates declaration only for 'x'"
            print *, "  - Variable 'y' is silently ignored"
            print *, "  - This confirms the bug described in BACKLOG.md"
            passed = .false.  ! Actually this should fail to demonstrate the bug
        end if
    end function test_current_parse_declaration_behavior

end program test_multi_variable_declarations
