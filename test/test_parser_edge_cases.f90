program test_parser_edge_cases
    use lexer_core
    use ast_core
    use ast_factory
    use parser_state_module, only: parser_state_t, create_parser_state
   use parser_statements_module, only: parse_typed_parameters, parse_function_definition
    implicit none

    logical :: all_passed

    all_passed = .true.

    print *, '=== Testing Parser: Edge Cases and Error Conditions ==='
    print *

    ! Test edge cases for parameter parsing
    if (.not. test_empty_parameter_list()) all_passed = .false.
    if (.not. test_complex_nested_types()) all_passed = .false.
    if (.not. test_long_parameter_lists()) all_passed = .false.
    if (.not. test_mixed_array_dimensions()) all_passed = .false.
    if (.not. test_error_recovery()) all_passed = .false.

    print *
    if (all_passed) then
        print *, "All parser edge case tests passed!"
        stop 0
    else
        print *, "Some parser edge case tests failed!"
        stop 1
    end if

contains

    function test_empty_parameter_list() result(passed)
        logical :: passed
        type(ast_arena_t) :: arena
        character(len=*), parameter :: source = ""
        character(len=*), parameter :: test_name = "empty_parameter_list"
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        integer, allocatable :: param_indices(:)

        passed = .false.

        ! Tokenize
        call tokenize_core(source, tokens)
        if (.not. allocated(tokens)) then
            print *, "ERROR in ", test_name, ": Tokenization failed"
            return
        end if

        ! Create parser and arena
        parser = create_parser_state(tokens)
        arena = create_ast_stack()

        ! Parse typed parameters - should handle empty input gracefully
        call parse_typed_parameters(parser, arena, param_indices)

        ! Should return empty parameter list
        if (.not. allocated(param_indices)) then
            allocate (param_indices(0))  ! Empty array
        end if

        if (size(param_indices) /= 0) then
            print *, "ERROR in ", test_name, ": Expected 0 params for empty input, got ", size(param_indices)
            return
        end if

        print *, "PASS: ", test_name
        passed = .true.
    end function test_empty_parameter_list

    function test_complex_nested_types() result(passed)
        logical :: passed
        type(ast_arena_t) :: arena
        character(len=*), parameter :: source = "type(complex_nested(8)), intent(inout) :: data(:,:,:)"
        character(len=*), parameter :: test_name = "complex_nested_types"
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
        arena = create_ast_stack()

        ! Parse typed parameters
        call parse_typed_parameters(parser, arena, param_indices)

        if (.not. allocated(param_indices) .or. size(param_indices) == 0) then
            print *, "ERROR in ", test_name, ": No parameters parsed"
            return
        end if

        ! Should have 1 parameter
        if (size(param_indices) /= 1) then
        print *, "ERROR in ", test_name, ": Expected 1 param, got ", size(param_indices)
            return
        end if

        ! Check parameter properties
        select type (node => arena%entries(param_indices(1))%node)
        type is (parameter_declaration_node)
            if (node%name /= "data") then
 print *, "ERROR in ", test_name, ": Param name should be 'data', got '", node%name, "'"
                return
            end if
            if (node%type_name /= "type(complex_nested(8))") then
                print *, "ERROR in ", test_name, ": Param type should be 'type(complex_nested(8))', got '", node%type_name, "'"
                return
            end if
            if (node%intent /= "inout") then
                print *, "ERROR in ", test_name, ": Param intent should be 'inout', got '", node%intent, "'"
                return
            end if
            if (.not. node%is_array) then
                print *, "ERROR in ", test_name, ": Param should be array"
                return
            end if
if (.not. allocated(node%dimension_indices) .or. size(node%dimension_indices) /= 3) then
                print *, "ERROR in ", test_name, ": Param should have 3 dimensions"
                return
            end if
        class default
          print *, "ERROR in ", test_name, ": Node should be parameter_declaration_node"
            return
        end select

        print *, "PASS: ", test_name
        passed = .true.
    end function test_complex_nested_types

    function test_long_parameter_lists() result(passed)
        logical :: passed
        type(ast_arena_t) :: arena
        character(len=*), parameter :: source = &
                                       "real(8), intent(in) :: a, b, c, d, e, "// &
                                       "integer, intent(out) :: f, g, h, "// &
                                       "logical :: i, j, k, l, "// &
                                       "type(point) :: p1, p2, p3"
        character(len=*), parameter :: test_name = "long_parameter_lists"
        type(token_t), allocatable :: tokens(:)
        type(parser_state_t) :: parser
        integer, allocatable :: param_indices(:)
        integer :: expected_count

        passed = .false.
        expected_count = 15  ! 5 real + 3 integer + 4 logical + 3 type

        ! Tokenize
        call tokenize_core(source, tokens)
        if (.not. allocated(tokens) .or. size(tokens) == 0) then
            print *, "ERROR in ", test_name, ": Tokenization failed"
            return
        end if

        ! Create parser and arena
        parser = create_parser_state(tokens)
        arena = create_ast_stack()

        ! Parse typed parameters
        call parse_typed_parameters(parser, arena, param_indices)

        if (.not. allocated(param_indices) .or. size(param_indices) == 0) then
            print *, "ERROR in ", test_name, ": No parameters parsed"
            return
        end if

        ! Check parameter count
        if (size(param_indices) /= expected_count) then
            print *, "ERROR in ", test_name, ": Expected ", expected_count, " params, got ", size(param_indices)
            return
        end if

        ! Spot check a few parameters
        select type (node1 => arena%entries(param_indices(1))%node)
        type is (parameter_declaration_node)
    if (node1%name /= "a" .or. node1%type_name /= "real" .or. node1%intent /= "in") then
                print *, "ERROR in ", test_name, ": First param incorrect"
                return
            end if
        end select

        select type (node6 => arena%entries(param_indices(6))%node)
        type is (parameter_declaration_node)
if (node6%name /= "f" .or. node6%type_name /= "integer" .or. node6%intent /= "out") then
                print *, "ERROR in ", test_name, ": Sixth param incorrect"
                return
            end if
        end select

        print *, "PASS: ", test_name
        passed = .true.
    end function test_long_parameter_lists

    function test_mixed_array_dimensions() result(passed)
        logical :: passed
        type(ast_arena_t) :: arena
        character(len=*), parameter :: source = &
                        "real(8) :: scalar, vector(:), matrix(:,:), tensor(:,:,:), "// &
                                       "integer :: fixed(10), mixed(5,:), assumed(*)"
        character(len=*), parameter :: test_name = "mixed_array_dimensions"
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
        arena = create_ast_stack()

        ! Parse typed parameters
        call parse_typed_parameters(parser, arena, param_indices)

        if (.not. allocated(param_indices) .or. size(param_indices) == 0) then
            print *, "ERROR in ", test_name, ": No parameters parsed"
            return
        end if

        ! Should have 7 parameters (4 real + 3 integer)
        if (size(param_indices) /= 7) then
       print *, "ERROR in ", test_name, ": Expected 7 params, got ", size(param_indices)
            return
        end if

        ! Check specific array types
        ! scalar (not array)
        select type (node1 => arena%entries(param_indices(1))%node)
        type is (parameter_declaration_node)
            if (node1%name /= "scalar" .or. node1%is_array) then
                print *, "ERROR in ", test_name, ": 'scalar' should not be array"
                return
            end if
        end select

        ! tensor (3D array)
        select type (node4 => arena%entries(param_indices(4))%node)
        type is (parameter_declaration_node)
            if (node4%name /= "tensor" .or. .not. node4%is_array) then
                print *, "ERROR in ", test_name, ": 'tensor' should be array"
                return
            end if
            if (.not. allocated(node4%dimension_indices) .or. size(node4%dimension_indices) /= 3) then
                print *, "ERROR in ", test_name, ": 'tensor' should have 3 dimensions"
                return
            end if
        end select

        print *, "PASS: ", test_name
        passed = .true.
    end function test_mixed_array_dimensions

    function test_error_recovery() result(passed)
        logical :: passed
        type(ast_arena_t) :: arena
        character(len=*), parameter :: source = "real(8), intent(in :: incomplete_param"
        character(len=*), parameter :: test_name = "error_recovery"
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
        arena = create_ast_stack()

        ! Parse typed parameters - should not crash on malformed input
        call parse_typed_parameters(parser, arena, param_indices)

        ! Parser should recover gracefully - exact behavior depends on implementation
        ! but it shouldn't crash. We'll accept whatever result it gives us.

        print *, "PASS: ", test_name, " (parser survived malformed input)"
        passed = .true.
    end function test_error_recovery

end program test_parser_edge_cases
