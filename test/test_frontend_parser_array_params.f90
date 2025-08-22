program test_frontend_parser_array_params
    use lexer_core
    use ast_core
    use ast_factory
    use ast_nodes_data, only: intent_type_to_string
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_definition_statements_module, only: parse_typed_parameters
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Testing Parser: Array Parameter Declarations ==='
    print *

    ! Test parsing array parameter declarations
    if (.not. test_simple_array_params()) all_passed = .false.

    print *
    if (all_passed) then
        print *, "All array parameter tests passed!"
        stop 0
    else
        print *, "Some array parameter tests failed!"
        stop 1
    end if

contains

    function test_simple_array_params() result(passed)
        logical :: passed
        type(ast_arena_t) :: arena
        character(len=*), parameter :: source = "real(8) :: x(:), y(:)"
        character(len=*), parameter :: test_name = "simple_array_params"
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        integer, allocatable :: param_indices(:)

        passed = .false.

        ! Tokenize
        call tokenize_core(source, tokens)
        if (.not. allocated(tokens) .or. size(tokens) == 0) then
            print *, "ERROR in ", test_name, ": Tokenization failed"
            return
        end if

        ! Create parser and arena
        parser = create_parser_state(tokens)
        arena = create_ast_arena()

        ! Parse typed parameters
        call parse_typed_parameters(parser, arena, param_indices)

        if (.not. allocated(param_indices) .or. size(param_indices) == 0) then
            print *, "ERROR in ", test_name, ": No parameters parsed"
            return
        end if

        ! Should have at least 2 parameter_declaration nodes
        ! (plus dimension nodes for the array specifications)
        if (size(param_indices) < 2) then
            print *, "ERROR in ", test_name, ": Expected at least 2 params, got ", size(param_indices)
            return
        end if

        ! Check first parameter (x)
        if (param_indices(1) > 0 .and. param_indices(1) <= arena%size) then
            select type (node1 => arena%entries(param_indices(1))%node)
            type is (parameter_declaration_node)
                if (node1%name /= "x") then
                    print *, "ERROR in ", test_name, ": First param name should be 'x', got '", node1%name, "'"
                    return
                end if
                if (node1%type_name /= "real") then
                    print *, "ERROR in ", test_name, ": First param type should be 'real', got '", node1%type_name, "'"
                    return
                end if
                if (node1%kind_value /= 8) then
                    print *, "ERROR in ", test_name, ": First param kind should be 8, got ", node1%kind_value
                    return
                end if
                if (intent_type_to_string(node1%intent_type) /= "") then
                    print *, "ERROR in ", test_name, ": First param intent should be empty, got '", &
                             intent_type_to_string(node1%intent_type), "'"
                    return
                end if
                if (.not. node1%is_array) then
                    print *, "ERROR in ", test_name, ": First param should be array"
                    return
                end if
                if (.not. allocated(node1%dimension_indices)) then
          print *, "ERROR in ", test_name, ": First param should have dimension_indices"
                    return
                end if
                if (size(node1%dimension_indices) /= 1) then
                    print *, "ERROR in ", test_name, ": First param should have 1 dimension, got ", size(node1%dimension_indices)
                    return
                end if
            class default
    print *, "ERROR in ", test_name, ": First node should be parameter_declaration_node"
                return
            end select
        else
            print *, "ERROR in ", test_name, ": Invalid parameter index"
            return
        end if

        ! Check second parameter (y)
        if (param_indices(2) > 0 .and. param_indices(2) <= arena%size) then
            select type (node2 => arena%entries(param_indices(2))%node)
            type is (parameter_declaration_node)
                if (node2%name /= "y") then
                    print *, "ERROR in ", test_name, ": Second param name should be 'y', got '", node2%name, "'"
                    return
                end if
                if (.not. node2%is_array) then
                    print *, "ERROR in ", test_name, ": Second param should be array"
                    return
                end if
            class default
   print *, "ERROR in ", test_name, ": Second node should be parameter_declaration_node"
                return
            end select
        else
            print *, "ERROR in ", test_name, ": Invalid second parameter index"
            return
        end if

        print *, "PASS: ", test_name
        passed = .true.
    end function test_simple_array_params

end program test_frontend_parser_array_params
