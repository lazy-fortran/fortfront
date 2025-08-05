program test_get_identifiers_issue
    use frontend, only: lex_file, parse_tokens
    use lexer_core, only: token_t, tokenize_core
    use ast_core
    use ast_arena
    use variable_usage_tracker_module, only: get_identifiers_in_subtree
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    ! Test that reproduces issue #104
    if (.not. test_print_statement_identifiers()) all_passed = .false.
    if (.not. test_assignment_identifiers()) all_passed = .false.
    if (.not. test_if_condition_identifiers()) all_passed = .false.
    if (.not. test_program_identifiers()) all_passed = .false.
    
    ! Report results
    if (all_passed) then
        print '(a)', "All get_identifiers tests passed"
        stop 0
    else
        print '(a)', "Some get_identifiers tests failed"
        stop 1
    end if
    
contains

    logical function test_print_statement_identifiers()
        ! Test identifiers in print statements
        character(len=*), parameter :: test_code = &
            'program test' // new_line('a') // &
            '  integer :: used_var' // new_line('a') // &
            '  used_var = 42' // new_line('a') // &
            '  print *, used_var' // new_line('a') // &
            'end program'
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=256) :: error_msg
        integer, allocatable :: print_nodes(:)
        character(len=:), allocatable :: identifiers(:)
        integer :: i
        
        test_print_statement_identifiers = .true.
        
        print '(a)', "Testing identifiers in print statements..."
        
        ! Tokenize and parse
        call tokenize_core(test_code, tokens)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0 .or. len_trim(error_msg) > 0) then
            print '(a)', "FAIL: Parsing failed"
            test_print_statement_identifiers = .false.
            return
        end if
        
        ! Find print statement nodes
        call find_print_nodes(arena, root_index, print_nodes)
        
        if (.not. allocated(print_nodes) .or. size(print_nodes) == 0) then
            print '(a)', "FAIL: No print statements found"
            test_print_statement_identifiers = .false.
            return
        end if
        
        ! Get identifiers from the print statement
        identifiers = get_identifiers_in_subtree(arena, print_nodes(1))
        
        print '(a,i0,a)', "Found ", size(identifiers), " identifiers in print statement"
        do i = 1, size(identifiers)
            print '(a,i0,a,a)', "  Identifier ", i, ": ", identifiers(i)
        end do
        
        if (size(identifiers) == 0) then
            print '(a)', "FAIL: No identifiers found in print statement (reproduces issue #104)"
            test_print_statement_identifiers = .false.
        else if (size(identifiers) == 1 .and. identifiers(1) == "used_var") then
            print '(a)', "PASS: Found expected identifier 'used_var'"
        else
            print '(a)', "FAIL: Wrong identifiers found"
            test_print_statement_identifiers = .false.
        end if
        
    end function test_print_statement_identifiers

    logical function test_assignment_identifiers()
        ! Test identifiers in assignments
        character(len=*), parameter :: test_code = &
            'program test' // new_line('a') // &
            '  integer :: x, y, z' // new_line('a') // &
            '  x = y + z * 2' // new_line('a') // &
            'end program'
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=256) :: error_msg
        integer, allocatable :: assignment_nodes(:)
        character(len=:), allocatable :: identifiers(:)
        integer :: i
        
        test_assignment_identifiers = .true.
        
        print '(a)', "Testing identifiers in assignments..."
        
        ! Tokenize and parse
        call tokenize_core(test_code, tokens)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0 .or. len_trim(error_msg) > 0) then
            print '(a)', "FAIL: Parsing failed"
            test_assignment_identifiers = .false.
            return
        end if
        
        ! Find assignment nodes
        call find_assignment_nodes(arena, root_index, assignment_nodes)
        
        if (.not. allocated(assignment_nodes) .or. size(assignment_nodes) == 0) then
            print '(a)', "FAIL: No assignments found"
            test_assignment_identifiers = .false.
            return
        end if
        
        ! Get identifiers from the assignment (right-hand side)
        ! We need to get the right-hand side of the assignment
        block
            use ast_nodes_core, only: assignment_node
            select type (node => arena%entries(assignment_nodes(1))%node)
            type is (assignment_node)
                identifiers = get_identifiers_in_subtree(arena, node%value_index)
            end select
        end block
        
        print '(a,i0,a)', "Found ", size(identifiers), " identifiers in assignment RHS"
        do i = 1, size(identifiers)
            print '(a,i0,a,a)', "  Identifier ", i, ": ", identifiers(i)
        end do
        
        if (size(identifiers) >= 2) then
            print '(a)', "PASS: Found identifiers in assignment"
        else
            print '(a)', "FAIL: Expected at least 2 identifiers (y, z)"
            test_assignment_identifiers = .false.
        end if
        
    end function test_assignment_identifiers

    logical function test_if_condition_identifiers()
        ! Test identifiers in if conditions
        character(len=*), parameter :: test_code = &
            'program test' // new_line('a') // &
            '  integer :: x, y' // new_line('a') // &
            '  if (x > y) then' // new_line('a') // &
            '    print *, "x is greater"' // new_line('a') // &
            '  end if' // new_line('a') // &
            'end program'
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=256) :: error_msg
        integer, allocatable :: if_nodes(:)
        character(len=:), allocatable :: identifiers(:)
        integer :: i
        
        test_if_condition_identifiers = .true.
        
        print '(a)', "Testing identifiers in if conditions..."
        
        ! Tokenize and parse
        call tokenize_core(test_code, tokens)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0 .or. len_trim(error_msg) > 0) then
            print '(a)', "FAIL: Parsing failed"
            test_if_condition_identifiers = .false.
            return
        end if
        
        ! Find if nodes
        call find_if_nodes(arena, root_index, if_nodes)
        
        if (.not. allocated(if_nodes) .or. size(if_nodes) == 0) then
            print '(a)', "FAIL: No if statements found"
            test_if_condition_identifiers = .false.
            return
        end if
        
        ! Get identifiers from the if condition
        block
            use ast_nodes_control, only: if_node
            select type (node => arena%entries(if_nodes(1))%node)
            type is (if_node)
                identifiers = get_identifiers_in_subtree(arena, node%condition_index)
                print '(a,i0)', "Condition index: ", node%condition_index
            end select
        end block
        
        print '(a,i0,a)', "Found ", size(identifiers), " identifiers in if condition"
        do i = 1, size(identifiers)
            print '(a,i0,a,a)', "  Identifier ", i, ": ", identifiers(i)
        end do
        
        if (size(identifiers) >= 2) then
            print '(a)', "PASS: Found identifiers in if condition"
        else
            print '(a)', "FAIL: Expected 2 identifiers (x, y) in condition"
            test_if_condition_identifiers = .false.
        end if
        
    end function test_if_condition_identifiers

    logical function test_program_identifiers()
        ! Test getting all identifiers from a program
        character(len=*), parameter :: test_code = &
            'program test' // new_line('a') // &
            '  integer :: a, b, c' // new_line('a') // &
            '  a = 1' // new_line('a') // &
            '  b = a + 2' // new_line('a') // &
            '  c = a * b' // new_line('a') // &
            '  print *, a, b, c' // new_line('a') // &
            'end program'
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: root_index
        character(len=256) :: error_msg
        character(len=:), allocatable :: identifiers(:)
        integer :: i
        
        test_program_identifiers = .true.
        
        print '(a)', "Testing identifiers in entire program..."
        
        ! Tokenize and parse
        call tokenize_core(test_code, tokens)
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, root_index, error_msg)
        
        if (root_index <= 0 .or. len_trim(error_msg) > 0) then
            print '(a)', "FAIL: Parsing failed"
            test_program_identifiers = .false.
            return
        end if
        
        ! Get all identifiers from the program
        identifiers = get_identifiers_in_subtree(arena, root_index)
        
        print '(a,i0,a)', "Found ", size(identifiers), " identifiers in program"
        do i = 1, size(identifiers)
            print '(a,i0,a,a)', "  Identifier ", i, ": ", identifiers(i)
        end do
        
        if (size(identifiers) > 0) then
            print '(a)', "PASS: Found identifiers in program"
        else
            print '(a)', "FAIL: No identifiers found in entire program"
            test_program_identifiers = .false.
        end if
        
    end function test_program_identifiers

    ! Helper subroutine to find print nodes
    recursive subroutine find_print_nodes(arena, node_index, found_nodes)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable, intent(inout) :: found_nodes(:)
        
        integer :: i
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Check if this is a print statement
        if (arena%entries(node_index)%node_type == "print_statement") then
            if (.not. allocated(found_nodes)) then
                allocate(found_nodes(1))
                found_nodes(1) = node_index
            else
                found_nodes = [found_nodes, node_index]
            end if
        end if
        
        ! Recursively search children based on node type
        select case (arena%entries(node_index)%node_type)
        case ("program")
            block
                use ast_nodes_core, only: program_node
                select type (node => arena%entries(node_index)%node)
                type is (program_node)
                    if (allocated(node%body_indices)) then
                        do i = 1, size(node%body_indices)
                            call find_print_nodes(arena, node%body_indices(i), found_nodes)
                        end do
                    end if
                end select
            end block
        case ("if_statement")
            block
                use ast_nodes_control, only: if_node
                select type (node => arena%entries(node_index)%node)
                type is (if_node)
                    if (allocated(node%then_body_indices)) then
                        do i = 1, size(node%then_body_indices)
                            call find_print_nodes(arena, node%then_body_indices(i), found_nodes)
                        end do
                    end if
                end select
            end block
        end select
    end subroutine find_print_nodes

    ! Helper subroutine to find assignment nodes
    recursive subroutine find_assignment_nodes(arena, node_index, found_nodes)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable, intent(inout) :: found_nodes(:)
        
        integer :: i
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Check if this is an assignment
        if (arena%entries(node_index)%node_type == "assignment") then
            if (.not. allocated(found_nodes)) then
                allocate(found_nodes(1))
                found_nodes(1) = node_index
            else
                found_nodes = [found_nodes, node_index]
            end if
        end if
        
        ! Recursively search children
        select case (arena%entries(node_index)%node_type)
        case ("program")
            block
                use ast_nodes_core, only: program_node
                select type (node => arena%entries(node_index)%node)
                type is (program_node)
                    if (allocated(node%body_indices)) then
                        do i = 1, size(node%body_indices)
                            call find_assignment_nodes(arena, node%body_indices(i), found_nodes)
                        end do
                    end if
                end select
            end block
        end select
    end subroutine find_assignment_nodes

    ! Helper subroutine to find if nodes
    recursive subroutine find_if_nodes(arena, node_index, found_nodes)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        integer, allocatable, intent(inout) :: found_nodes(:)
        
        integer :: i
        
        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return
        
        ! Check if this is an if statement
        if (arena%entries(node_index)%node_type == "if_statement") then
            if (.not. allocated(found_nodes)) then
                allocate(found_nodes(1))
                found_nodes(1) = node_index
            else
                found_nodes = [found_nodes, node_index]
            end if
        end if
        
        ! Recursively search children
        select case (arena%entries(node_index)%node_type)
        case ("program")
            block
                use ast_nodes_core, only: program_node
                select type (node => arena%entries(node_index)%node)
                type is (program_node)
                    if (allocated(node%body_indices)) then
                        do i = 1, size(node%body_indices)
                            call find_if_nodes(arena, node%body_indices(i), found_nodes)
                        end do
                    end if
                end select
            end block
        end select
    end subroutine find_if_nodes

end program test_get_identifiers_issue