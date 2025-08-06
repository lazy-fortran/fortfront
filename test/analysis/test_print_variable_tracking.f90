program test_print_variable_tracking
    use iso_fortran_env, only: error_unit
    use lexer_core, only: tokenize_core, token_t
    use parser_state_module, only: parser_state_t, create_parser_state
    use parser_statements_module, only: parse_subroutine_definition
    use ast_core, only: ast_arena_t, create_ast_arena
    use variable_usage_tracker_module, only: get_identifiers_in_subtree
    implicit none

    logical :: all_tests_passed
    all_tests_passed = .true.

    call test_print_statement_variable_usage()
    call test_print_with_multiple_variables()
    call test_print_with_expression()

    if (all_tests_passed) then
        print *, "All print variable tracking tests PASSED!"
    else
        error stop "Some print variable tracking tests FAILED!"
    end if

contains

    subroutine test_print_statement_variable_usage()
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: sub_index, print_node_index
        character(len=:), allocatable :: identifiers(:)
        logical :: found_arg
        integer :: i
        
        print *, "Testing variable usage in simple print statement..."
        
        ! Test case from issue #135
        source = "subroutine test_sub(arg)" // new_line('a') // &
                "  integer :: arg" // new_line('a') // &
                "  print *, arg" // new_line('a') // &
                "end subroutine"
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        sub_index = parse_subroutine_definition(parser, arena)
        
        if (sub_index <= 0) then
            write(error_unit, *) "FAILED: Could not parse subroutine"
            all_tests_passed = .false.
            return
        end if
        
        ! Find the print statement node in the AST
        print_node_index = find_print_statement_in_arena(arena)
        
        if (print_node_index <= 0) then
            write(error_unit, *) "FAILED: Could not find print statement node in AST"
            all_tests_passed = .false.
            return
        end if
        
        ! Test get_identifiers_in_subtree on the print statement
        identifiers = get_identifiers_in_subtree(arena, print_node_index)
        
        ! Check if 'arg' was found
        found_arg = .false.
        if (allocated(identifiers)) then
            do i = 1, size(identifiers)
                if (identifiers(i) == "arg") then
                    found_arg = .true.
                    exit
                end if
            end do
        end if
        
        if (found_arg) then
            print *, "PASSED: Found 'arg' in print statement"
        else
            write(error_unit, *) "FAILED: 'arg' not found in print statement"
            write(error_unit, *) "  Found identifiers:", size(identifiers)
            if (allocated(identifiers)) then
                do i = 1, size(identifiers)
                    print *, "    ", trim(identifiers(i))
                end do
            end if
            all_tests_passed = .false.
        end if
        
    end subroutine test_print_statement_variable_usage

    subroutine test_print_with_multiple_variables()
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: sub_index, print_node_index
        character(len=:), allocatable :: identifiers(:)
        logical :: found_x, found_y
        integer :: i
        
        print *, "Testing variable usage in print with multiple variables..."
        
        source = "subroutine test_multi(x, y)" // new_line('a') // &
                "  integer :: x, y" // new_line('a') // &
                "  print *, x, y" // new_line('a') // &
                "end subroutine"
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        sub_index = parse_subroutine_definition(parser, arena)
        
        if (sub_index <= 0) then
            write(error_unit, *) "FAILED: Could not parse subroutine"
            all_tests_passed = .false.
            return
        end if
        
        print_node_index = find_print_statement_in_arena(arena)
        
        if (print_node_index <= 0) then
            write(error_unit, *) "FAILED: Could not find print statement node in AST"
            all_tests_passed = .false.
            return
        end if
        
        identifiers = get_identifiers_in_subtree(arena, print_node_index)
        
        ! Check if both 'x' and 'y' were found
        found_x = .false.
        found_y = .false.
        if (allocated(identifiers)) then
            do i = 1, size(identifiers)
                if (identifiers(i) == "x") found_x = .true.
                if (identifiers(i) == "y") found_y = .true.
            end do
        end if
        
        if (found_x .and. found_y) then
            print *, "PASSED: Found both 'x' and 'y' in print statement"
        else
            write(error_unit, *) "FAILED: Missing variables in print statement"
            write(error_unit, *) "  Found x:", found_x, " Found y:", found_y
            all_tests_passed = .false.
        end if
        
    end subroutine test_print_with_multiple_variables

    subroutine test_print_with_expression()
        character(len=:), allocatable :: source
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        type(parser_state_t) :: parser
        integer :: sub_index, print_node_index
        character(len=:), allocatable :: identifiers(:)
        logical :: found_a, found_b
        integer :: i
        
        print *, "Testing variable usage in print with expression..."
        
        source = "subroutine test_expr(a, b)" // new_line('a') // &
                "  integer :: a, b" // new_line('a') // &
                "  print *, a + b" // new_line('a') // &
                "end subroutine"
        
        call tokenize_core(source, tokens)
        arena = create_ast_arena()
        parser = create_parser_state(tokens)
        sub_index = parse_subroutine_definition(parser, arena)
        
        if (sub_index <= 0) then
            write(error_unit, *) "FAILED: Could not parse subroutine"
            all_tests_passed = .false.
            return
        end if
        
        print_node_index = find_print_statement_in_arena(arena)
        
        if (print_node_index <= 0) then
            write(error_unit, *) "FAILED: Could not find print statement node in AST"
            all_tests_passed = .false.
            return
        end if
        
        identifiers = get_identifiers_in_subtree(arena, print_node_index)
        
        ! Check if both 'a' and 'b' were found in the expression
        found_a = .false.
        found_b = .false.
        if (allocated(identifiers)) then
            do i = 1, size(identifiers)
                if (identifiers(i) == "a") found_a = .true.
                if (identifiers(i) == "b") found_b = .true.
            end do
        end if
        
        if (found_a .and. found_b) then
            print *, "PASSED: Found both 'a' and 'b' in print expression"
        else
            write(error_unit, *) "FAILED: Missing variables in print expression"
            write(error_unit, *) "  Found a:", found_a, " Found b:", found_b
            all_tests_passed = .false.
        end if
        
    end subroutine test_print_with_expression

    ! Helper function to find print_statement node in the arena
    function find_print_statement_in_arena(arena) result(node_index)
        use ast_core, only: ast_arena_t
        type(ast_arena_t), intent(in) :: arena
        integer :: node_index
        integer :: i
        
        node_index = 0
        
        do i = 1, arena%size
            if (arena%entries(i)%node_type == "print_statement") then
                node_index = i
                return
            end if
        end do
    end function find_print_statement_in_arena

end program test_print_variable_tracking