program test_range_subscript_node
    use ast_core
    use ast_nodes_core, only: range_subscript_node, create_range_subscript
    use ast_factory, only: push_range_subscript, push_identifier
    use json_module
    use codegen_core, only: codegen_core_generate_arena
    implicit none
    
    logical :: all_passed
    all_passed = .true.
    
    call test_range_subscript_creation(all_passed)
    call test_range_subscript_json(all_passed)
    call test_range_subscript_assign(all_passed)
    call test_range_subscript_codegen(all_passed)
    call test_range_subscript_factory(all_passed)
    
    if (all_passed) then
        print *, "All range_subscript_node tests passed!"
    else
        print *, "Some range_subscript_node tests FAILED!"
        stop 1
    end if
    
contains

    subroutine test_range_subscript_creation(all_passed)
        logical, intent(inout) :: all_passed
        type(range_subscript_node) :: node
        logical :: test_passed
        
        test_passed = .true.
        
        ! Test creation with all parameters
        node = create_range_subscript(1, 2, 3, 10, 20)
        if (node%base_expr_index /= 1) test_passed = .false.
        if (node%start_index /= 2) test_passed = .false.
        if (node%end_index /= 3) test_passed = .false.
        if (node%line /= 10) test_passed = .false.
        if (node%column /= 20) test_passed = .false.
        if (node%is_character_substring) test_passed = .false.  ! Should default to false
        
        if (test_passed) then
            print *, "PASS: range_subscript creation with all parameters"
        else
            print *, "FAIL: range_subscript creation with all parameters"
            all_passed = .false.
        end if
        
        ! Test creation with optional parameters
        node = create_range_subscript(5)
        if (node%base_expr_index /= 5) test_passed = .false.
        if (node%start_index /= -1) test_passed = .false.  ! Should default to -1
        if (node%end_index /= -1) test_passed = .false.    ! Should default to -1
        
        if (test_passed) then
            print *, "PASS: range_subscript creation with minimal parameters"
        else
            print *, "FAIL: range_subscript creation with minimal parameters"
            all_passed = .false.
        end if
    end subroutine test_range_subscript_creation
    
    subroutine test_range_subscript_json(all_passed)
        logical, intent(inout) :: all_passed
        type(range_subscript_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root, value
        character(len=:), allocatable :: json_str
        logical :: test_passed, found
        integer :: int_val
        
        test_passed = .true.
        
        ! Create node and test JSON serialization
        node = create_range_subscript(10, 20, 30, 1, 5)
        node%is_character_substring = .true.
        
        call json%initialize()
        call json%create_object(root, '')
        call node%to_json(json, root)
        
        ! Check JSON fields
        call json%get(root, 'type', value, found)
        if (.not. found) test_passed = .false.
        
        call json%get(root, 'base_expr_index', int_val, found)
        if (.not. found .or. int_val /= 10) test_passed = .false.
        
        call json%get(root, 'start_index', int_val, found)
        if (.not. found .or. int_val /= 20) test_passed = .false.
        
        call json%get(root, 'end_index', int_val, found)
        if (.not. found .or. int_val /= 30) test_passed = .false.
        
        call json%get(root, 'is_character_substring', found)
        if (.not. found) test_passed = .false.
        
        if (test_passed) then
            print *, "PASS: range_subscript JSON serialization"
        else
            print *, "FAIL: range_subscript JSON serialization"
            all_passed = .false.
        end if
        
        call json%destroy(root)
    end subroutine test_range_subscript_json
    
    subroutine test_range_subscript_assign(all_passed)
        logical, intent(inout) :: all_passed
        type(range_subscript_node) :: node1, node2
        logical :: test_passed
        
        test_passed = .true.
        
        ! Create source node
        node1 = create_range_subscript(100, 200, 300, 10, 20)
        node1%is_character_substring = .true.
        
        ! Test assignment
        node2 = node1
        
        if (node2%base_expr_index /= 100) test_passed = .false.
        if (node2%start_index /= 200) test_passed = .false.
        if (node2%end_index /= 300) test_passed = .false.
        if (node2%line /= 10) test_passed = .false.
        if (node2%column /= 20) test_passed = .false.
        if (.not. node2%is_character_substring) test_passed = .false.
        
        if (test_passed) then
            print *, "PASS: range_subscript assignment operator"
        else
            print *, "FAIL: range_subscript assignment operator"
            all_passed = .false.
        end if
    end subroutine test_range_subscript_assign
    
    subroutine test_range_subscript_codegen(all_passed)
        logical, intent(inout) :: all_passed
        type(ast_arena_t) :: arena
        character(len=:), allocatable :: code
        integer :: base_idx, start_idx, end_idx, node_idx
        logical :: test_passed
        
        arena = create_ast_arena()
        test_passed = .true.
        
        ! Create indices for base, start, and end expressions
        base_idx = push_identifier(arena, "arr", 1, 1)
        start_idx = push_identifier(arena, "i", 1, 5)
        end_idx = push_identifier(arena, "j", 1, 7)
        
        ! Create range subscript node
        node_idx = push_range_subscript(arena, base_idx, start_idx, end_idx, 1, 1)
        
        ! Generate code
        code = codegen_core_generate_arena(arena, node_idx)
        
        if (code /= "arr(i:j)") then
            print *, "FAIL: range_subscript codegen - expected 'arr(i:j)', got '", code, "'"
            test_passed = .false.
            all_passed = .false.
        else
            print *, "PASS: range_subscript codegen with all indices"
        end if
        
        ! Test with missing start index
        node_idx = push_range_subscript(arena, base_idx, end_index=end_idx, &
                                       line=1, column=1)
        code = codegen_core_generate_arena(arena, node_idx)
        
        if (code /= "arr(:j)") then
            print *, "FAIL: range_subscript codegen - expected 'arr(:j)', got '", code, "'"
            test_passed = .false.
            all_passed = .false.
        else
            print *, "PASS: range_subscript codegen with missing start"
        end if
        
        ! Test with missing end index
        node_idx = push_range_subscript(arena, base_idx, start_index=start_idx, &
                                       line=1, column=1)
        code = codegen_core_generate_arena(arena, node_idx)
        
        if (code /= "arr(i:)") then
            print *, "FAIL: range_subscript codegen - expected 'arr(i:)', got '", code, "'"
            test_passed = .false.
            all_passed = .false.
        else
            print *, "PASS: range_subscript codegen with missing end"
        end if
    end subroutine test_range_subscript_codegen
    
    subroutine test_range_subscript_factory(all_passed)
        logical, intent(inout) :: all_passed
        type(ast_arena_t) :: arena
        integer :: base_idx, node_idx
        logical :: test_passed
        
        arena = create_ast_arena()
        test_passed = .true.
        
        ! Test invalid base expression
        node_idx = push_range_subscript(arena, -1, 1, 2, 1, 1)
        
        ! Should create an error node
        if (node_idx <= 0) then
            print *, "FAIL: range_subscript factory should handle invalid base"
            test_passed = .false.
            all_passed = .false.
        else
            print *, "PASS: range_subscript factory handles invalid base"
        end if
        
        ! Test with valid base but out of bounds
        node_idx = push_range_subscript(arena, 9999, 1, 2, 1, 1)
        
        if (node_idx <= 0) then
            print *, "FAIL: range_subscript factory should handle out-of-bounds base"
            test_passed = .false.
            all_passed = .false.
        else
            print *, "PASS: range_subscript factory handles out-of-bounds base"
        end if
    end subroutine test_range_subscript_factory

end program test_range_subscript_node