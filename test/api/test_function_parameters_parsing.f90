program test_function_parameters_parsing
    ! Verify function parameter parsing via parser definition API
    use lexer_core, only: tokenize_core
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_definition_statements_module, only: parse_function_definition
    use parser_prefix_buffer_module, only: parser_prefix_buffer_t
    use fortfront, only: ast_arena_t, create_ast_arena, token_t, ast_node, function_def_node
    use ast_nodes_data, only: parameter_declaration_node
    implicit none

    logical :: all_passed
    all_passed = .true.

    print *, '=== fortfront API Function Parameter Parsing Tests ==='

    if (.not. test_typed_parameters()) all_passed = .false.
    if (.not. test_keyword_parameters()) all_passed = .false.

    print *
    if (all_passed) then
        print *, 'All function parameter parsing tests passed!'
        stop 0
    else
        print *, 'Some function parameter parsing tests failed!'
        stop 1
    end if

contains

    logical function test_typed_parameters()
        test_typed_parameters = .true.
        print *, 'Testing typed parameter parsing in function definition...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            character(len=:), allocatable :: source
            integer :: idx
            type(parser_state_t) :: parser
            class(ast_node), allocatable :: node
            type(parser_prefix_buffer_t) :: prefix_buffer

            source = 'real function foo(x, n)' // new_line('A') // &
                '  real, intent(in) :: x' // new_line('A') // &
                '  integer :: n' // new_line('A') // &
                '  foo = x * n' // new_line('A') // &
                'end function foo'

            call tokenize_core(source, tokens)
            arena = create_ast_arena()
            parser = create_parser_state(tokens)
            idx = parse_function_definition(parser, arena, prefix_buffer)

            if (idx <= 0 .or. idx > arena%size) then
                print *, '  FAIL: Invalid node index returned'
                test_typed_parameters = .false.
                return
            end if

            if (.not. allocated(arena%entries(idx)%node)) then
                print *, '  FAIL: Node not allocated'
                test_typed_parameters = .false.
                return
            end if

            select type (node => arena%entries(idx)%node)
                type is (function_def_node)
                if (.not. allocated(node%body_indices)) then
                    print *, '  INFO: body_indices not allocated'
                else
                    print *, '  INFO: body_indices size = ', size(node%body_indices)
                end if
                if (.not. allocated(node%param_indices)) then
                    print *, '  FAIL: Expected allocated param_indices'
                    test_typed_parameters = .false.
                    return
                end if
                if (size(node%param_indices) /= 2) then
                    print *, '  FAIL: Expected 2 parameters, got ', size(node%param_indices)
                    test_typed_parameters = .false.
                    return
                end if

                ! Minimal validation: indices refer to parameter nodes
                if (node%param_indices(1) <= 0 .or. node%param_indices(1) > arena%size) then
                    print *, '  FAIL: Invalid first parameter index'
                    test_typed_parameters = .false.
                    return
                end if
                if (.not. allocated(arena%entries(node%param_indices(1))%node)) then
                    print *, '  FAIL: First parameter node not allocated'
                    test_typed_parameters = .false.
                    return
                end if
                select type (p1 => arena%entries(node%param_indices(1))%node)
                    type is (parameter_declaration_node)
                    continue
                class default
                    print *, '  FAIL: First parameter is not a parameter_declaration_node'
                    test_typed_parameters = .false.
                    return
                end select

                if (node%param_indices(2) <= 0 .or. node%param_indices(2) > arena%size) then
                    print *, '  FAIL: Invalid second parameter index'
                    test_typed_parameters = .false.
                    return
                end if
                if (.not. allocated(arena%entries(node%param_indices(2))%node)) then
                    print *, '  FAIL: Second parameter node not allocated'
                    test_typed_parameters = .false.
                    return
                end if
                select type (p2 => arena%entries(node%param_indices(2))%node)
                    type is (parameter_declaration_node)
                    continue
                class default
                    print *, '  FAIL: Second parameter is not a parameter_declaration_node'
                    test_typed_parameters = .false.
                    return
                end select

            class default
                print *, '  FAIL: Expected function_def_node at root'
                test_typed_parameters = .false.
                return
            end select

            print *, '  PASS: Typed parameter parsing in function definition'
        end block
    end function test_typed_parameters

    logical function test_keyword_parameters()
        test_keyword_parameters = .true.
        print *, 'Testing keyword parameter names in function definition...'

        block
            type(token_t), allocatable :: tokens(:)
            type(ast_arena_t) :: arena
            character(len=:), allocatable :: source
            integer :: idx
            type(parser_state_t) :: parser
            class(ast_node), allocatable :: node
            type(parser_prefix_buffer_t) :: prefix_buffer

            source = 'integer function make_range(start, stop, step)' // new_line('A') // &
                '  integer :: start' // new_line('A') // &
                '  integer :: stop' // new_line('A') // &
                '  integer :: step' // new_line('A') // &
                '  make_range = stop - start + step' // new_line('A') // &
                'end function make_range'

            call tokenize_core(source, tokens)
            arena = create_ast_arena()
            parser = create_parser_state(tokens)
            idx = parse_function_definition(parser, arena, prefix_buffer)

            if (idx <= 0 .or. idx > arena%size) then
                print *, '  FAIL: Invalid node index returned'
                test_keyword_parameters = .false.
                return
            end if

            if (.not. allocated(arena%entries(idx)%node)) then
                print *, '  FAIL: Node not allocated'
                test_keyword_parameters = .false.
                return
            end if

            select type (node => arena%entries(idx)%node)
                type is (function_def_node)
                if (.not. allocated(node%param_indices)) then
                    print *, '  FAIL: Expected allocated param_indices'
                    test_keyword_parameters = .false.
                    return
                end if
                if (size(node%param_indices) /= 3) then
                    print *, '  FAIL: Expected 3 parameters, got ', size(node%param_indices)
                    test_keyword_parameters = .false.
                    return
                end if

                if (.not. verify_parameter_name(arena, node%param_indices(1), 'start')) then
                    test_keyword_parameters = .false.
                    return
                end if
                if (.not. verify_parameter_name(arena, node%param_indices(2), 'stop')) then
                    test_keyword_parameters = .false.
                    return
                end if
                if (.not. verify_parameter_name(arena, node%param_indices(3), 'step')) then
                    test_keyword_parameters = .false.
                    return
                end if
            class default
                print *, '  FAIL: Expected function_def_node at root'
                test_keyword_parameters = .false.
                return
            end select

            print *, '  PASS: Keyword parameter names parsed correctly'
        end block
    end function test_keyword_parameters

    logical function verify_parameter_name(arena, param_index, expected_name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_index
        character(len=*), intent(in) :: expected_name

        if (param_index <= 0 .or. param_index > arena%size) then
            print *, '  FAIL: Invalid parameter index'
            verify_parameter_name = .false.
            return
        end if
        if (.not. allocated(arena%entries(param_index)%node)) then
            print *, '  FAIL: Parameter node not allocated'
            verify_parameter_name = .false.
            return
        end if

        select type (param_node => arena%entries(param_index)%node)
            type is (parameter_declaration_node)
            if (.not. allocated(param_node%name)) then
                print *, '  FAIL: Parameter name not allocated'
                verify_parameter_name = .false.
                return
            end if
            if (trim(param_node%name) /= expected_name) then
                print *, '  FAIL: Expected parameter ', expected_name, ' got ', &
                    trim(param_node%name)
                verify_parameter_name = .false.
                return
            end if
        class default
            print *, '  FAIL: Node is not a parameter_declaration_node'
            verify_parameter_name = .false.
            return
        end select

        verify_parameter_name = .true.
    end function verify_parameter_name

end program test_function_parameters_parsing
