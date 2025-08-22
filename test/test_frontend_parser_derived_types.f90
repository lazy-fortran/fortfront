program test_frontend_parser_derived_types
    use lexer_core
    use ast_core
    use ast_factory
    use ast_nodes_data, only: intent_type_to_string
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_definition_statements_module, only: parse_typed_parameters
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Testing Parser: Derived Type Parameters ==='
    print *

    ! Test parsing derived type parameter declarations
    if (.not. test_simple_derived_type()) all_passed = .false.
    if (.not. test_mixed_derived_scalar()) all_passed = .false.
    if (.not. test_derived_type_with_intent()) all_passed = .false.
    if (.not. test_derived_type_arrays()) all_passed = .false.

    print *
    if (all_passed) then
        print *, "All derived type parameter tests passed!"
        stop 0
    else
        print *, "Some derived type parameter tests failed!"
        stop 1
    end if

contains

    function test_simple_derived_type() result(passed)
        logical :: passed
        type(ast_arena_t) :: arena
        character(len=*), parameter :: source = "type(my_type) :: obj1, obj2"
        character(len=*), parameter :: test_name = "simple_derived_type"
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

        ! Should have 2 parameter_declaration nodes
        if (size(param_indices) /= 2) then
            print *, "ERROR in ", test_name, ": Expected 2 params, got ", size(param_indices)
            return
        end if

        ! Check first parameter (obj1)
        if (param_indices(1) > 0 .and. param_indices(1) <= arena%size) then
            select type (node1 => arena%entries(param_indices(1))%node)
            type is (parameter_declaration_node)
                if (node1%name /= "obj1") then
                    print *, "ERROR in ", test_name, ": First param name should be 'obj1', got '", node1%name, "'"
                    return
                end if
                if (node1%type_name /= "type(my_type)") then
                    print *, "ERROR in ", test_name, ": First param type should be 'type(my_type)', got '", node1%type_name, "'"
                    return
                end if
                if (node1%is_array) then
                    print *, "ERROR in ", test_name, ": First param should be scalar"
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

        ! Check second parameter (obj2)
        if (param_indices(2) > 0 .and. param_indices(2) <= arena%size) then
            select type (node2 => arena%entries(param_indices(2))%node)
            type is (parameter_declaration_node)
                if (node2%name /= "obj2") then
                    print *, "ERROR in ", test_name, ": Second param name should be 'obj2', got '", node2%name, "'"
                    return
                end if
                if (node2%type_name /= "type(my_type)") then
                    print *, "ERROR in ", test_name, ": Second param type should be 'type(my_type)', got '", node2%type_name, "'"
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
    end function test_simple_derived_type

    function test_mixed_derived_scalar() result(passed)
        logical :: passed
        type(ast_arena_t) :: arena
        character(len=*), parameter :: source = "real(8) :: x, type(point) :: p, integer :: n"
        character(len=*), parameter :: test_name = "mixed_derived_scalar"
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

        ! Should have 3 parameter_declaration nodes (different types)
        if (size(param_indices) /= 3) then
            print *, "ERROR in ", test_name, ": Expected 3 params, got ", size(param_indices)
            return
        end if

        ! Check first parameter (x: real)
        select type (node1 => arena%entries(param_indices(1))%node)
        type is (parameter_declaration_node)
            if (node1%name /= "x" .or. node1%type_name /= "real") then
                print *, "ERROR in ", test_name, ": First param should be x:real, got ", node1%name, ":", node1%type_name
                return
            end if
        end select

        ! Check second parameter (p: type(point))
        select type (node2 => arena%entries(param_indices(2))%node)
        type is (parameter_declaration_node)
            if (node2%name /= "p" .or. node2%type_name /= "type(point)") then
                print *, "ERROR in ", test_name, ": Second param should be p:type(point), got ", node2%name, ":", node2%type_name
                return
            end if
        end select

        ! Check third parameter (n: integer)
        select type (node3 => arena%entries(param_indices(3))%node)
        type is (parameter_declaration_node)
            if (node3%name /= "n" .or. node3%type_name /= "integer") then
                print *, "ERROR in ", test_name, ": Third param should be n:integer, got ", node3%name, ":", node3%type_name
                return
            end if
        end select

        print *, "PASS: ", test_name
        passed = .true.
    end function test_mixed_derived_scalar

    function test_derived_type_with_intent() result(passed)
        logical :: passed
        type(ast_arena_t) :: arena
        character(len=*), parameter :: source = "type(vector), intent(in) :: v1, v2"
        character(len=*), parameter :: test_name = "derived_type_with_intent"
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

        ! Should have 2 parameter_declaration nodes
        if (size(param_indices) /= 2) then
            print *, "ERROR in ", test_name, ": Expected 2 params, got ", size(param_indices)
            return
        end if

        ! Check first parameter
        select type (node1 => arena%entries(param_indices(1))%node)
        type is (parameter_declaration_node)
            if (node1%name /= "v1") then
                print *, "ERROR in ", test_name, ": First param name should be 'v1', got '", node1%name, "'"
                return
            end if
            if (node1%type_name /= "type(vector)") then
                print *, "ERROR in ", test_name, ": First param type should be 'type(vector)', got '", node1%type_name, "'"
                return
            end if
            if (intent_type_to_string(node1%intent_type) /= "in") then
                print *, "ERROR in ", test_name, ": First param intent should be 'in', got '", &
                         intent_type_to_string(node1%intent_type), "'"
                return
            end if
        class default
            print *, "ERROR in ", test_name, ": First node should be parameter_declaration_node"
            return
        end select

        print *, "PASS: ", test_name
        passed = .true.
    end function test_derived_type_with_intent

    function test_derived_type_arrays() result(passed)
        logical :: passed
        type(ast_arena_t) :: arena
        character(len=*), parameter :: source = "type(point) :: points(:), grid(:,:)"
        character(len=*), parameter :: test_name = "derived_type_arrays"
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

        ! Should have 2 parameter_declaration nodes
        if (size(param_indices) /= 2) then
            print *, "ERROR in ", test_name, ": Expected 2 params, got ", size(param_indices)
            return
        end if

        ! Check first parameter (points: 1D array)
        select type (node1 => arena%entries(param_indices(1))%node)
        type is (parameter_declaration_node)
            if (node1%name /= "points") then
                print *, "ERROR in ", test_name, ": First param name should be 'points', got '", node1%name, "'"
                return
            end if
            if (node1%type_name /= "type(point)") then
                print *, "ERROR in ", test_name, ": First param type should be 'type(point)', got '", node1%type_name, "'"
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

        ! Check second parameter (grid: 2D array)
        select type (node2 => arena%entries(param_indices(2))%node)
        type is (parameter_declaration_node)
            if (node2%name /= "grid") then
                print *, "ERROR in ", test_name, ": Second param name should be 'grid', got '", node2%name, "'"
                return
            end if
            if (node2%type_name /= "type(point)") then
                print *, "ERROR in ", test_name, ": Second param type should be 'type(point)', got '", node2%type_name, "'"
                return
            end if
            if (.not. node2%is_array) then
                print *, "ERROR in ", test_name, ": Second param should be array"
                return
            end if
            if (.not. allocated(node2%dimension_indices)) then
                print *, "ERROR in ", test_name, ": Second param should have dimension_indices"
                return
            end if
            if (size(node2%dimension_indices) /= 2) then
                print *, "ERROR in ", test_name, ": Second param should have 2 dimensions, got ", size(node2%dimension_indices)
                return
            end if
        class default
            print *, "ERROR in ", test_name, ": Second node should be parameter_declaration_node"
            return
        end select

        print *, "PASS: ", test_name
        passed = .true.
    end function test_derived_type_arrays

end program test_frontend_parser_derived_types