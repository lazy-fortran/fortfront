program test_variable_usage_tracker
    use fortfront
    use iso_fortran_env, only: error_unit
    implicit none

    logical :: all_tests_passed

    all_tests_passed = .true.

    call test_simple_variable_tracking()
    call test_binary_operation_tracking()
    call test_complex_expression_tracking()
    call test_function_call_tracking()
    call test_array_access_tracking()
    call test_nested_expression_tracking()
    call test_expression_visitor_pattern()
    call test_specific_variable_queries()

    if (all_tests_passed) then
        print *, "All variable usage tracker tests PASSED!"
    else
        error stop "Some variable usage tracker tests FAILED!"
    end if

contains

    subroutine test_simple_variable_tracking()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index, condition_index
        type(variable_usage_info_t) :: usage_info
        
        print *, "Testing simple variable tracking..."
        
        ! Test: if (x > 0) then
        source = "implicit none" // new_line('a') // &
                "integer :: x" // new_line('a') // &
                "x = 5" // new_line('a') // &
                "if (x > 0) then" // new_line('a') // &
                "    print *, 'positive'" // new_line('a') // &
                "end if"
        
        call lex_source(source, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAILED: Lexing error:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0) then
            print *, "FAILED: Parse failed:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        ! Find the if statement and get its condition
        condition_index = find_if_condition_in_program(arena, root_index)
        if (condition_index <= 0) then
            print *, "FAILED: Could not find if condition"
            all_tests_passed = .false.
            return
        end if
        
        ! Track variables in the condition
        usage_info = get_variables_in_expression(arena, condition_index)
        
        ! Should find 'x' variable
        if (size(usage_info%variable_names) /= 1) then
            print *, "FAILED: Expected 1 variable, got", size(usage_info%variable_names)
            all_tests_passed = .false.
            return
        end if
        
        if (usage_info%variable_names(1) /= "x") then
            print *, "FAILED: Expected 'x', got", usage_info%variable_names(1)
            all_tests_passed = .false.
            return
        end if
        
        if (usage_info%usage_counts(1) /= 1) then
            print *, "FAILED: Expected 1 usage, got", usage_info%usage_counts(1)
            all_tests_passed = .false.
            return
        end if
        
        print *, "PASSED: Simple variable tracking test"
        
    end subroutine test_simple_variable_tracking

    subroutine test_binary_operation_tracking()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index, condition_index
        type(variable_usage_info_t) :: usage_info
        integer :: i
        logical :: found_x, found_y, found_z
        
        print *, "Testing binary operation tracking..."
        
        ! Test: if (x + y > z) then
        source = "implicit none" // new_line('a') // &
                "integer :: x, y, z" // new_line('a') // &
                "x = 1; y = 2; z = 3" // new_line('a') // &
                "if (x + y > z) then" // new_line('a') // &
                "    print *, 'condition true'" // new_line('a') // &
                "end if"
        
        call lex_source(source, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAILED: Lexing error:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0) then
            print *, "FAILED: Parse failed:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        ! Find the if condition
        condition_index = find_if_condition_in_program(arena, root_index)
        if (condition_index <= 0) then
            print *, "FAILED: Could not find if condition"
            all_tests_passed = .false.
            return
        end if
        
        ! Track variables in the condition
        usage_info = get_variables_in_expression(arena, condition_index)
        
        ! Should find x, y, z variables
        if (size(usage_info%variable_names) /= 3) then
            print *, "FAILED: Expected 3 variables, got", size(usage_info%variable_names)
            all_tests_passed = .false.
            return
        end if
        
        ! Check that we found x, y, z
        found_x = .false.
        found_y = .false.
        found_z = .false.
        do i = 1, size(usage_info%variable_names)
            if (usage_info%variable_names(i) == "x") found_x = .true.
            if (usage_info%variable_names(i) == "y") found_y = .true.
            if (usage_info%variable_names(i) == "z") found_z = .true.
        end do
        
        if (.not. (found_x .and. found_y .and. found_z)) then
            print *, "FAILED: Did not find expected variables x, y, z"
            print *, "Found:", (usage_info%variable_names(i), i=1,size(usage_info%variable_names))
            all_tests_passed = .false.
            return
        end if
        
        print *, "PASSED: Binary operation tracking test"
        
    end subroutine test_binary_operation_tracking

    subroutine test_complex_expression_tracking()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index, condition_index, i
        type(variable_usage_info_t) :: usage_info
        
        print *, "Testing complex expression tracking..."
        
        ! Test: if ((a + b) * c > (d - e) / f) then
        source = "implicit none" // new_line('a') // &
                "integer :: a, b, c, d, e, f" // new_line('a') // &
                "a = 1; b = 2; c = 3; d = 4; e = 5; f = 6" // new_line('a') // &
                "if ((a + b) * c > (d - e) / f) then" // new_line('a') // &
                "    print *, 'complex'" // new_line('a') // &
                "end if"
        
        call lex_source(source, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAILED: Lexing error:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0) then
            print *, "FAILED: Parse failed:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        ! Find the if condition
        condition_index = find_if_condition_in_program(arena, root_index)
        if (condition_index <= 0) then
            print *, "FAILED: Could not find if condition"
            all_tests_passed = .false.
            return
        end if
        
        ! Track variables in the condition
        usage_info = get_variables_in_expression(arena, condition_index)
        
        ! Should find a, b, c, d, e, f variables
        if (size(usage_info%variable_names) /= 6) then
            print *, "FAILED: Expected 6 variables, got", size(usage_info%variable_names)
            print *, "Found:", (usage_info%variable_names(i), i=1,size(usage_info%variable_names))
            all_tests_passed = .false.
            return
        end if
        
        print *, "PASSED: Complex expression tracking test"
        
    end subroutine test_complex_expression_tracking

    subroutine test_function_call_tracking()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index, condition_index
        type(variable_usage_info_t) :: usage_info
        
        print *, "Testing function call tracking..."
        
        ! Test: if (sin(x) > cos(y)) then
        source = "implicit none" // new_line('a') // &
                "real :: x, y" // new_line('a') // &
                "x = 1.0; y = 2.0" // new_line('a') // &
                "if (sin(x) > cos(y)) then" // new_line('a') // &
                "    print *, 'trig'" // new_line('a') // &
                "end if"
        
        call lex_source(source, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAILED: Lexing error:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0) then
            print *, "FAILED: Parse failed:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        ! Find the if condition
        condition_index = find_if_condition_in_program(arena, root_index)
        if (condition_index <= 0) then
            print *, "FAILED: Could not find if condition"
            all_tests_passed = .false.
            return
        end if
        
        ! Track variables in the condition
        usage_info = get_variables_in_expression(arena, condition_index)
        
        ! Should find x, y variables (and possibly sin, cos function names)
        if (size(usage_info%variable_names) < 2) then
            print *, "FAILED: Expected at least 2 variables, got", size(usage_info%variable_names)
            all_tests_passed = .false.
            return
        end if
        
        print *, "PASSED: Function call tracking test"
        
    end subroutine test_function_call_tracking

    subroutine test_array_access_tracking()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index, condition_index, i
        type(variable_usage_info_t) :: usage_info
        
        print *, "Testing array access tracking..."
        
        ! Test: if (arr(i) > arr(j)) then
        source = "implicit none" // new_line('a') // &
                "integer :: arr(10), i, j" // new_line('a') // &
                "i = 1; j = 2" // new_line('a') // &
                "if (arr(i) > arr(j)) then" // new_line('a') // &
                "    print *, 'array'" // new_line('a') // &
                "end if"
        
        call lex_source(source, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAILED: Lexing error:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0) then
            print *, "FAILED: Parse failed:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        ! Find the if condition
        condition_index = find_if_condition_in_program(arena, root_index)
        if (condition_index <= 0) then
            print *, "FAILED: Could not find if condition"
            all_tests_passed = .false.
            return
        end if
        
        ! Track variables in the condition
        usage_info = get_variables_in_expression(arena, condition_index)
        
        ! Should find arr, i, j variables
        if (size(usage_info%variable_names) < 3) then
            print *, "FAILED: Expected at least 3 variables, got", size(usage_info%variable_names)
            print *, "Found:", (usage_info%variable_names(i), i=1,size(usage_info%variable_names))
            all_tests_passed = .false.
            return
        end if
        
        print *, "PASSED: Array access tracking test"
        
    end subroutine test_array_access_tracking

    subroutine test_nested_expression_tracking()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index, condition_index
        character(len=:), allocatable :: identifiers(:)
        
        print *, "Testing nested expression tracking..."
        
        ! Test: if (x + (y * (z - w))) then
        source = "implicit none" // new_line('a') // &
                "integer :: x, y, z, w" // new_line('a') // &
                "x = 1; y = 2; z = 3; w = 4" // new_line('a') // &
                "if (x + (y * (z - w)) > 0) then" // new_line('a') // &
                "    print *, 'nested'" // new_line('a') // &
                "end if"
        
        call lex_source(source, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAILED: Lexing error:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0) then
            print *, "FAILED: Parse failed:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        ! Find the if condition
        condition_index = find_if_condition_in_program(arena, root_index)
        if (condition_index <= 0) then
            print *, "FAILED: Could not find if condition"
            all_tests_passed = .false.
            return
        end if
        
        ! Get identifiers in subtree
        identifiers = get_identifiers_in_subtree(arena, condition_index)
        
        ! Should find x, y, z, w variables
        if (size(identifiers) /= 4) then
            print *, "FAILED: Expected 4 identifiers, got", size(identifiers)
            all_tests_passed = .false.
            return
        end if
        
        print *, "PASSED: Nested expression tracking test"
        
    end subroutine test_nested_expression_tracking

    subroutine test_expression_visitor_pattern()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index, condition_index
        type(expression_visitor_t) :: visitor
        integer :: visit_count
        
        print *, "Testing expression visitor pattern..."
        
        ! Test: if (a + b > c) then
        source = "implicit none" // new_line('a') // &
                "integer :: a, b, c" // new_line('a') // &
                "a = 1; b = 2; c = 3" // new_line('a') // &
                "if (a + b > c) then" // new_line('a') // &
                "    print *, 'visitor'" // new_line('a') // &
                "end if"
        
        call lex_source(source, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAILED: Lexing error:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0) then
            print *, "FAILED: Parse failed:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        ! Find the if condition
        condition_index = find_if_condition_in_program(arena, root_index)
        if (condition_index <= 0) then
            print *, "FAILED: Could not find if condition"
            all_tests_passed = .false.
            return
        end if
        
        ! Set up visitor
        visitor%visit => count_nodes_visitor
        visit_count = 0
        
        ! Visit expression nodes
        call visit_expression_nodes(arena, condition_index, visitor, visit_count)
        
        ! Should have visited multiple nodes
        if (visit_count < 3) then
            print *, "FAILED: Expected to visit at least 3 nodes, got", visit_count
            all_tests_passed = .false.
            return
        end if
        
        print *, "PASSED: Expression visitor pattern test"
        
    end subroutine test_expression_visitor_pattern

    subroutine test_specific_variable_queries()
        character(len=:), allocatable :: source, error_msg
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index, condition_index
        logical :: x_used, y_used, z_used
        integer :: x_count
        
        print *, "Testing specific variable queries..."
        
        ! Test: if (x + x > y) then (x used twice, y once, z not used)
        source = "implicit none" // new_line('a') // &
                "integer :: x, y, z" // new_line('a') // &
                "x = 1; y = 2; z = 3" // new_line('a') // &
                "if (x + x > y) then" // new_line('a') // &
                "    print *, 'query'" // new_line('a') // &
                "end if"
        
        call lex_source(source, tokens, error_msg)
        if (error_msg /= "") then
            print *, "FAILED: Lexing error:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0) then
            print *, "FAILED: Parse failed:", error_msg
            all_tests_passed = .false.
            return
        end if
        
        ! Find the if condition
        condition_index = find_if_condition_in_program(arena, root_index)
        if (condition_index <= 0) then
            print *, "FAILED: Could not find if condition"
            all_tests_passed = .false.
            return
        end if
        
        ! Test specific variable queries
        x_used = is_variable_used_in_expression(arena, condition_index, "x")
        y_used = is_variable_used_in_expression(arena, condition_index, "y")
        z_used = is_variable_used_in_expression(arena, condition_index, "z")
        
        x_count = count_variable_usage(arena, condition_index, "x")
        
        ! Check results
        if (.not. x_used) then
            print *, "FAILED: x should be used"
            all_tests_passed = .false.
            return
        end if
        
        if (.not. y_used) then
            print *, "FAILED: y should be used"
            all_tests_passed = .false.
            return
        end if
        
        if (z_used) then
            print *, "FAILED: z should not be used"
            all_tests_passed = .false.
            return
        end if
        
        if (x_count /= 2) then
            print *, "FAILED: x should be used 2 times, got", x_count
            all_tests_passed = .false.
            return
        end if
        
        print *, "PASSED: Specific variable queries test"
        
    end subroutine test_specific_variable_queries

    ! Helper function to find if condition in a program
    function find_if_condition_in_program(arena, prog_index) result(condition_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: prog_index
        integer :: condition_index
        
        integer :: i, if_index
        
        condition_index = 0
        
        ! Get program body
        select type (prog_node => arena%entries(prog_index)%node)
        type is (program_node)
            if (allocated(prog_node%body_indices)) then
                ! Look for if statement in body
                do i = 1, size(prog_node%body_indices)
                    if_index = prog_node%body_indices(i)
                    if (if_index > 0 .and. if_index <= arena%size) then
                        if (arena%entries(if_index)%node_type == "if_statement") then
                            ! Get condition from if statement
                            select type (if_stmt => arena%entries(if_index)%node)
                            type is (if_node)
                                condition_index = if_stmt%condition_index
                                return
                            end select
                        end if
                    end if
                end do
            end if
        end select
    end function find_if_condition_in_program

    ! Visitor function to count nodes
    subroutine count_nodes_visitor(arena, node_index, node_type, user_data)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        character(len=*), intent(in) :: node_type
        class(*), intent(inout), optional :: user_data
        
        if (present(user_data)) then
            select type (counter => user_data)
            type is (integer)
                counter = counter + 1
            end select
        end if
    end subroutine count_nodes_visitor

end program test_variable_usage_tracker