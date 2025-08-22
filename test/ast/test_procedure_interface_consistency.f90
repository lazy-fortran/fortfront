program test_procedure_interface_consistency
    ! Test for issue #74: Function/subroutine interface inconsistencies in AST API
    ! This test verifies that the unified procedure interface works correctly
    use fortfront
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    call test_unified_procedure_interface()
    call test_interface_block_with_procedures()
    call test_procedure_helper_functions()
    call test_procedure_edge_cases()
    
    if (all_tests_passed) then
        print *, "All procedure interface consistency tests PASSED!"
    else
        print *, "Some procedure interface consistency tests FAILED!"
        stop 1
    end if
    
contains

    subroutine test_unified_procedure_interface()
        type(ast_arena_t) :: arena
        type(function_def_node) :: func_node
        type(subroutine_def_node) :: sub_node
        integer :: func_index, sub_index
        character(len=:), allocatable :: name
        integer, allocatable :: params(:), body(:)
        logical :: has_return
        
        print *, "Testing unified procedure interface..."
        
        ! Create test arena
        arena = create_ast_arena()
        
        ! Create function node
        func_node = create_function_def("test_func", [1,2], "real", [3,4])
        call arena%push(func_node, "function_def", 0)
        func_index = arena%size
        
        ! Create subroutine node  
        sub_node = create_subroutine_def("test_sub", [5,6], [7,8])
        call arena%push(sub_node, "subroutine_def", 0)
        sub_index = arena%size
        
        ! Test unified interface with function
        if (is_procedure_node(arena%entries(func_index)%node)) then
            name = get_procedure_name(arena%entries(func_index)%node)
            if (name /= "test_func") then
                print *, "FAILED: Function name mismatch:", name
                all_tests_passed = .false.
                return
            end if
            
            params = get_procedure_params(arena%entries(func_index)%node)
            if (size(params) /= 2 .or. params(1) /= 1 .or. params(2) /= 2) then
                print *, "FAILED: Function params mismatch"
                all_tests_passed = .false.
                return
            end if
            
            body = get_procedure_body(arena%entries(func_index)%node)
            if (size(body) /= 2 .or. body(1) /= 3 .or. body(2) /= 4) then
                print *, "FAILED: Function body mismatch"
                all_tests_passed = .false.
                return
            end if
            
            has_return = procedure_has_return_type(arena%entries(func_index)%node)
            if (.not. has_return) then
                print *, "FAILED: Function should have return type"
                all_tests_passed = .false.
                return
            end if
            
            name = get_procedure_return_type(arena%entries(func_index)%node)
            if (name /= "real") then
                print *, "FAILED: Function return type mismatch:", name
                all_tests_passed = .false.
                return
            end if
        else
            print *, "FAILED: Function not recognized as procedure"
            all_tests_passed = .false.
            return
        end if
        
        ! Test unified interface with subroutine
        if (is_procedure_node(arena%entries(sub_index)%node)) then
            name = get_procedure_name(arena%entries(sub_index)%node)
            if (name /= "test_sub") then
                print *, "FAILED: Subroutine name mismatch:", name
                all_tests_passed = .false.
                return
            end if
            
            params = get_procedure_params(arena%entries(sub_index)%node)
            if (size(params) /= 2 .or. params(1) /= 5 .or. params(2) /= 6) then
                print *, "FAILED: Subroutine params mismatch"
                all_tests_passed = .false.
                return
            end if
            
            body = get_procedure_body(arena%entries(sub_index)%node)
            if (size(body) /= 2 .or. body(1) /= 7 .or. body(2) /= 8) then
                print *, "FAILED: Subroutine body mismatch"
                all_tests_passed = .false.
                return
            end if
            
            has_return = procedure_has_return_type(arena%entries(sub_index)%node)
            if (has_return) then
                print *, "FAILED: Subroutine should not have return type"
                all_tests_passed = .false.
                return
            end if
            
            name = get_procedure_return_type(arena%entries(sub_index)%node)
            if (name /= "") then
                print *, "FAILED: Subroutine should have empty return type"
                all_tests_passed = .false.
                return
            end if
        else
            print *, "FAILED: Subroutine not recognized as procedure"
            all_tests_passed = .false.
            return
        end if
        
        print *, "PASSED: Unified procedure interface"
    end subroutine test_unified_procedure_interface

    subroutine test_interface_block_with_procedures()
        type(ast_arena_t) :: arena
        type(interface_block_node) :: iface_node
        integer :: iface_index
        
        print *, "Testing interface block with procedures..."
        
        ! Create test arena
        arena = create_ast_arena()
        
        ! Create interface block node
        iface_node%name = "test_interface"
        iface_node%kind = "interface"
        ! Procedures would be referenced by indices in procedure_indices array
        allocate(iface_node%procedure_indices(2))
        iface_node%procedure_indices = [100, 200]  ! Mock indices
        
        call arena%push(iface_node, "interface_block", 0)
        iface_index = arena%size
        
        ! Test interface block structure
        select type (n => arena%entries(iface_index)%node)
        type is (interface_block_node)
            if (.not. allocated(n%procedure_indices)) then
                print *, "FAILED: Interface block procedure_indices not allocated"
                all_tests_passed = .false.
                return
            end if
            
            if (size(n%procedure_indices) /= 2) then
                print *, "FAILED: Interface block procedure count mismatch"
                all_tests_passed = .false.
                return
            end if
            
            if (n%procedure_indices(1) /= 100 .or. n%procedure_indices(2) /= 200) then
                print *, "FAILED: Interface block procedure indices mismatch"
                all_tests_passed = .false.
                return
            end if
        class default
            print *, "FAILED: Interface block node type mismatch"
            all_tests_passed = .false.
            return
        end select
        
        print *, "PASSED: Interface block with procedures"
    end subroutine test_interface_block_with_procedures

    subroutine test_procedure_helper_functions()
        type(ast_arena_t) :: arena
        type(identifier_node) :: id_node
        integer :: id_index
        character(len=:), allocatable :: name
        integer, allocatable :: params(:)
        logical :: is_proc
        
        print *, "Testing procedure helper functions with non-procedure..."
        
        ! Create test arena
        arena = create_ast_arena()
        
        ! Create identifier node (not a procedure)
        id_node%name = "not_a_procedure"
        call arena%push(id_node, "identifier", 0)
        id_index = arena%size
        
        ! Test that non-procedures are handled correctly
        is_proc = is_procedure_node(arena%entries(id_index)%node)
        if (is_proc) then
            print *, "FAILED: Identifier incorrectly identified as procedure"
            all_tests_passed = .false.
            return
        end if
        
        name = get_procedure_name(arena%entries(id_index)%node)
        if (name /= "") then
            print *, "FAILED: Non-procedure should return empty name"
            all_tests_passed = .false.
            return
        end if
        
        params = get_procedure_params(arena%entries(id_index)%node)
        if (size(params) /= 0) then
            print *, "FAILED: Non-procedure should return empty params"
            all_tests_passed = .false.
            return
        end if
        
        print *, "PASSED: Procedure helper functions with non-procedure"
    end subroutine test_procedure_helper_functions

    subroutine test_procedure_edge_cases()
        type(ast_arena_t) :: arena
        type(function_def_node) :: empty_func_node
        type(subroutine_def_node) :: empty_sub_node
        integer :: empty_func_index, empty_sub_index
        character(len=:), allocatable :: name, return_type
        integer, allocatable :: params(:), body(:)
        logical :: has_return

        print *, "Testing procedure edge cases (no params, no body, unallocated fields)..."

        ! Create test arena
        arena = create_ast_arena()

        ! Create function node with no parameters and no body (edge case)
        empty_func_node%name = "empty_func"
        empty_func_node%return_type = "integer"
        ! Intentionally leave param_indices and body_indices unallocated
        call arena%push(empty_func_node, "function_def", 0)
        empty_func_index = arena%size

        ! Create subroutine node with no parameters and no body (edge case)
        empty_sub_node%name = "empty_sub"
        ! Intentionally leave param_indices and body_indices unallocated
        call arena%push(empty_sub_node, "subroutine_def", 0)
        empty_sub_index = arena%size

        ! Test empty function
        if (.not. is_procedure_node(arena%entries(empty_func_index)%node)) then
            print *, "FAILED: Empty function not recognized as procedure"
            all_tests_passed = .false.
            return
        end if

        name = get_procedure_name(arena%entries(empty_func_index)%node)
        if (name /= "empty_func") then
            print *, "FAILED: Empty function name mismatch:", name
            all_tests_passed = .false.
            return
        end if

        params = get_procedure_params(arena%entries(empty_func_index)%node)
        if (size(params) /= 0) then
            print *, "FAILED: Empty function should have zero parameters"
            all_tests_passed = .false.
            return
        end if

        body = get_procedure_body(arena%entries(empty_func_index)%node)
        if (size(body) /= 0) then
            print *, "FAILED: Empty function should have zero body elements"
            all_tests_passed = .false.
            return
        end if

        has_return = procedure_has_return_type(arena%entries(empty_func_index)%node)
        if (.not. has_return) then
            print *, "FAILED: Empty function should have return type"
            all_tests_passed = .false.
            return
        end if

        return_type = get_procedure_return_type(arena%entries(empty_func_index)%node)
        if (return_type /= "integer") then
            print *, "FAILED: Empty function return type mismatch:", return_type
            all_tests_passed = .false.
            return
        end if

        ! Test empty subroutine
        if (.not. is_procedure_node(arena%entries(empty_sub_index)%node)) then
            print *, "FAILED: Empty subroutine not recognized as procedure"
            all_tests_passed = .false.
            return
        end if

        name = get_procedure_name(arena%entries(empty_sub_index)%node)
        if (name /= "empty_sub") then
            print *, "FAILED: Empty subroutine name mismatch:", name
            all_tests_passed = .false.
            return
        end if

        params = get_procedure_params(arena%entries(empty_sub_index)%node)
        if (size(params) /= 0) then
            print *, "FAILED: Empty subroutine should have zero parameters"
            all_tests_passed = .false.
            return
        end if

        body = get_procedure_body(arena%entries(empty_sub_index)%node)
        if (size(body) /= 0) then
            print *, "FAILED: Empty subroutine should have zero body elements"
            all_tests_passed = .false.
            return
        end if

        has_return = procedure_has_return_type(arena%entries(empty_sub_index)%node)
        if (has_return) then
            print *, "FAILED: Empty subroutine should not have return type"
            all_tests_passed = .false.
            return
        end if

        return_type = get_procedure_return_type(arena%entries(empty_sub_index)%node)
        if (return_type /= "") then
            print *, "FAILED: Empty subroutine should have empty return type"
            all_tests_passed = .false.
            return
        end if

        print *, "PASSED: Procedure edge cases"
    end subroutine test_procedure_edge_cases

end program test_procedure_interface_consistency
