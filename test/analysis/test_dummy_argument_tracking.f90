program test_dummy_argument_tracking
    use fortfront
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none

    logical :: all_tests_passed
    all_tests_passed = .true.

    call test_simple_dummy_argument()
    call test_multiple_dummy_arguments()
    call test_dummy_in_expression()
    call test_dummy_with_intent()
    call test_nested_subroutine_dummy()

    if (all_tests_passed) then
        print *, "All dummy argument tracking tests PASSED!"
    else
        print *, "Some dummy argument tracking tests FAILED!"
        stop 1
    end if

contains

    include '../common/read_example.inc'

    subroutine test_simple_dummy_argument()
        use lexer_core, only: tokenize_core
        use parser_state_module, only: parser_state_t, create_parser_state
        use parser_definition_statements_module, only: parse_subroutine_definition
        use parser_prefix_buffer_module, only: parser_prefix_buffer_t
        use parser_prefix_buffer_module, only: parser_prefix_buffer_t
        use parser_prefix_buffer_module, only: parser_prefix_buffer_t
        use variable_usage_tracker_module, only: get_identifiers_in_subtree
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: sub_index, i
        character(len=:), allocatable :: identifiers(:)
        logical :: arg_found
        type(parser_prefix_buffer_t) :: prefix_buffer

        print *, "Testing simple dummy argument tracking..."

        ! Test case from issue #118 - modified to use assignment instead of print
        ! (parser has issues with print statements in subroutine bodies)
        call read_example('examples/f90/dummy_arg_simple.f90', source)

        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        sub_index = parse_subroutine_definition(parser, arena, prefix_buffer)

        if (sub_index <= 0) then
            print *, "FAILED: Could not parse subroutine"
            all_tests_passed = .false.
            return
        end if

        ! Get identifiers from the subroutine subtree
        identifiers = get_identifiers_in_subtree(arena, sub_index)

        ! Check if 'arg' is found in used identifiers
        arg_found = .false.
        if (allocated(identifiers)) then
            do i = 1, size(identifiers)
                if (identifiers(i) == "arg") then
                    arg_found = .true.
                    exit
                end if
            end do
        end if

        if (arg_found) then
            print *, "PASSED: Dummy argument 'arg' detected as used"
        else
            print *, "FAILED: Dummy argument 'arg' not detected as used"
            if (allocated(identifiers)) then
                print *, "  Found identifiers:"
                do i = 1, size(identifiers)
                    print *, "    - ", identifiers(i)
                end do
            else
                print *, "  No identifiers found at all!"
            end if
            all_tests_passed = .false.
        end if

    end subroutine test_simple_dummy_argument

    subroutine test_multiple_dummy_arguments()
        use lexer_core, only: tokenize_core
        use parser_state_module, only: parser_state_t, create_parser_state
        use parser_definition_statements_module, only: parse_subroutine_definition
        use parser_prefix_buffer_module, only: parser_prefix_buffer_t
        use parser_prefix_buffer_module, only: parser_prefix_buffer_t
        use variable_usage_tracker_module, only: get_identifiers_in_subtree
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: sub_index, i
        character(len=:), allocatable :: identifiers(:)
        logical :: a_found, b_found, c_found
        type(parser_prefix_buffer_t) :: prefix_buffer

        print *, "Testing multiple dummy arguments..."

        call read_example('examples/f90/dummy_arg_multiple.f90', source)

        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        sub_index = parse_subroutine_definition(parser, arena, prefix_buffer)

        if (sub_index <= 0) then
            print *, "FAILED: Could not parse subroutine"
            all_tests_passed = .false.
            return
        end if

        ! Get identifiers from the subroutine subtree
        identifiers = get_identifiers_in_subtree(arena, sub_index)

        ! Check if all arguments are found
        a_found = .false.
        b_found = .false.
        c_found = .false.

        if (allocated(identifiers)) then
            do i = 1, size(identifiers)
                if (identifiers(i) == "a") a_found = .true.
                if (identifiers(i) == "b") b_found = .true.
                if (identifiers(i) == "c") c_found = .true.
            end do
        end if

        if (a_found .and. b_found .and. c_found) then
            print *, "PASSED: All dummy arguments detected as used"
        else
            print *, "FAILED: Not all dummy arguments detected"
            print *, "  a found:", a_found
            print *, "  b found:", b_found
            print *, "  c found:", c_found
            all_tests_passed = .false.
        end if

    end subroutine test_multiple_dummy_arguments

    subroutine test_dummy_in_expression()
        use lexer_core, only: tokenize_core
        use parser_state_module, only: parser_state_t, create_parser_state
        use parser_definition_statements_module, only: parse_function_definition
        use parser_prefix_buffer_module, only: parser_prefix_buffer_t
        use variable_usage_tracker_module, only: get_identifiers_in_subtree
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: func_index, i
        character(len=:), allocatable :: identifiers(:)
        logical :: x_found, y_found
        type(parser_prefix_buffer_t) :: prefix_buffer

        print *, "Testing dummy arguments in expressions..."

        call read_example('examples/f90/dummy_arg_expression.f90', source)

        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        func_index = parse_function_definition(parser, arena, prefix_buffer)

        if (func_index <= 0) then
            print *, "FAILED: Could not parse function"
            all_tests_passed = .false.
            return
        end if

        ! Get identifiers from the function subtree
        identifiers = get_identifiers_in_subtree(arena, func_index)

        ! Check if arguments are found
        x_found = .false.
        y_found = .false.

        if (allocated(identifiers)) then
            do i = 1, size(identifiers)
                if (identifiers(i) == "x") x_found = .true.
                if (identifiers(i) == "y") y_found = .true.
            end do
        end if

        if (x_found .and. y_found) then
            print *, "PASSED: Dummy arguments in expressions detected"
        else
            print *, "FAILED: Dummy arguments in expressions not detected"
            print *, "  x found:", x_found
            print *, "  y found:", y_found
            all_tests_passed = .false.
        end if

    end subroutine test_dummy_in_expression

    subroutine test_dummy_with_intent()
        use lexer_core, only: tokenize_core
        use parser_state_module, only: parser_state_t, create_parser_state
        use parser_definition_statements_module, only: parse_subroutine_definition
        use variable_usage_tracker_module, only: get_identifiers_in_subtree
        use parser_prefix_buffer_module, only: parser_prefix_buffer_t
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: sub_index, i
        character(len=:), allocatable :: identifiers(:)
        logical :: input_found, output_found
        type(parser_prefix_buffer_t) :: prefix_buffer

        print *, "Testing dummy arguments with intent..."

        call read_example('examples/f90/dummy_arg_intent.f90', source)

        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        sub_index = parse_subroutine_definition(parser, arena, prefix_buffer)

        if (sub_index <= 0) then
            print *, "FAILED: Could not parse subroutine"
            all_tests_passed = .false.
            return
        end if

        ! Get identifiers from the subroutine subtree
        identifiers = get_identifiers_in_subtree(arena, sub_index)

        ! Check if arguments with intent are found
        input_found = .false.
        output_found = .false.

        if (allocated(identifiers)) then
            do i = 1, size(identifiers)
                if (identifiers(i) == "input") input_found = .true.
                if (identifiers(i) == "output") output_found = .true.
            end do
        end if

        if (input_found .and. output_found) then
            print *, "PASSED: Dummy arguments with intent detected"
        else
            print *, "FAILED: Dummy arguments with intent not detected"
            print *, "  input found:", input_found
            print *, "  output found:", output_found
            all_tests_passed = .false.
        end if

    end subroutine test_dummy_with_intent

    subroutine test_nested_subroutine_dummy()
        use lexer_core, only: tokenize_core
        use parser_state_module, only: parser_state_t, create_parser_state
        use parser_definition_statements_module, only: parse_subroutine_definition
        use parser_prefix_buffer_module, only: parser_prefix_buffer_t
        use variable_usage_tracker_module, only: get_identifiers_in_subtree
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: sub_index, i
        character(len=:), allocatable :: identifiers(:)
        logical :: param_found
        type(parser_prefix_buffer_t) :: prefix_buffer

        print *, "Testing dummy argument in nested context..."

        call read_example('examples/f90/dummy_arg_nested.f90', source)

        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        sub_index = parse_subroutine_definition(parser, arena, prefix_buffer)

        if (sub_index <= 0) then
            print *, "FAILED: Could not parse subroutine"
            all_tests_passed = .false.
            return
        end if

        ! Get identifiers from the subroutine subtree
        identifiers = get_identifiers_in_subtree(arena, sub_index)

        ! Check if param is found
        param_found = .false.

        if (allocated(identifiers)) then
            do i = 1, size(identifiers)
                if (identifiers(i) == "param") then
                    param_found = .true.
                    exit
                end if
            end do
        end if

        if (param_found) then
            print *, "PASSED: Dummy argument in nested context detected"
        else
            print *, "FAILED: Dummy argument in nested context not detected"
            all_tests_passed = .false.
        end if

    end subroutine test_nested_subroutine_dummy


end program test_dummy_argument_tracking
