program test_frontend_parser_param_nodes
    use lexer_core
    use ast_core
    use ast_factory
    use ast_nodes_data, only: intent_type_to_string
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_statements_module, only: parse_function_definition
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Testing Parser: Parameter Declaration Nodes ==='
    print *

    ! Test that parameter_declaration nodes are created
    if (.not. test_param_nodes_created()) all_passed = .false.

    ! Report results
    print *
    if (all_passed) then
        print *, 'All parameter declaration node tests passed!'
        stop 0
    else
        print *, 'Some parameter declaration node tests failed!'
        stop 1
    end if

contains

    logical function test_param_nodes_created()
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: func_index, i
        logical :: found_param_decl

        test_param_nodes_created = .false.
        print *, 'Testing parameter declaration node creation...'

        ! Create tokens for: function add(real(8), intent(in) :: x, y)
        allocate (tokens(20))
        tokens(1) = token_t(TK_KEYWORD, "function", 1, 1)
        tokens(2) = token_t(TK_IDENTIFIER, "add", 1, 10)
        tokens(3) = token_t(TK_OPERATOR, "(", 1, 13)
        tokens(4) = token_t(TK_KEYWORD, "real", 1, 14)
        tokens(5) = token_t(TK_OPERATOR, "(", 1, 18)
        tokens(6) = token_t(TK_NUMBER, "8", 1, 19)
        tokens(7) = token_t(TK_OPERATOR, ")", 1, 20)
        tokens(8) = token_t(TK_OPERATOR, ",", 1, 21)
        tokens(9) = token_t(TK_KEYWORD, "intent", 1, 23)
        tokens(10) = token_t(TK_OPERATOR, "(", 1, 29)
        tokens(11) = token_t(TK_KEYWORD, "in", 1, 30)
        tokens(12) = token_t(TK_OPERATOR, ")", 1, 32)
        tokens(13) = token_t(TK_OPERATOR, "::", 1, 34)
        tokens(14) = token_t(TK_IDENTIFIER, "x", 1, 37)
        tokens(15) = token_t(TK_OPERATOR, ",", 1, 38)
        tokens(16) = token_t(TK_IDENTIFIER, "y", 1, 40)
        tokens(17) = token_t(TK_OPERATOR, ")", 1, 41)
        tokens(18) = token_t(TK_KEYWORD, "end", 2, 1)
        tokens(19) = token_t(TK_KEYWORD, "function", 2, 5)
        tokens(20) = token_t(TK_EOF, "", 2, 13)

        ! Parse
        parser = create_parser_state(tokens)
        func_index = parse_function_definition(parser, arena)

        if (func_index > 0) then
            print *, '  Function parsed successfully'

            ! Check if parameter_declaration nodes were created
            found_param_decl = .false.
            do i = 1, arena%size
                if (allocated(arena%entries(i)%node)) then
                    select type (node => arena%entries(i)%node)
                    type is (parameter_declaration_node)
                        found_param_decl = .true.
                    print *, '  Found parameter_declaration node for: ', trim(node%name)
                        print *, '    Type: ', trim(node%type_name)
                        print *, '    Kind: ', node%kind_value
                        print *, '    Intent: ', trim(intent_type_to_string(node%intent_type))
                    end select
                end if
            end do

            if (found_param_decl) then
                print *, '  PASS: Parameter declaration nodes created'
                test_param_nodes_created = .true.
            else
                print *, '  FAIL: No parameter declaration nodes found'
            end if
        else
            print *, '  FAIL: Could not parse function'
        end if

    end function test_param_nodes_created

end program test_frontend_parser_param_nodes
