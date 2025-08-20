program test_character_substring_semantic_simple
    use ast_core
    use ast_nodes_bounds, only: array_slice_node
    implicit none
    
    logical :: all_passed
    all_passed = .true.
    
    call test_flag_defaults(all_passed)
    call test_flag_setting(all_passed)
    
    if (all_passed) then
        print *, "All character substring semantic simple tests passed!"
    else
        print *, "Some tests FAILED!"
        stop 1
    end if
    
contains

    subroutine test_flag_defaults(all_passed)
        logical, intent(inout) :: all_passed
        type(array_slice_node) :: node
        
        ! Test that flag defaults to false
        if (node%is_character_substring) then
            print *, "FAIL: is_character_substring should default to false"
            all_passed = .false.
        else
            print *, "PASS: is_character_substring defaults to false"
        end if
    end subroutine test_flag_defaults
    
    subroutine test_flag_setting(all_passed)
        logical, intent(inout) :: all_passed
        type(ast_arena_t) :: arena
        type(array_slice_node) :: node
        integer :: node_idx
        
        arena = create_ast_arena()
        
        ! Create node and set flag
        node%array_index = 1
        node%bounds_indices(1) = 2
        node%num_dimensions = 1
        node%is_character_substring = .true.
        
        ! Push to arena
        call arena%push(node, "array_slice", 0)
        if (arena%current_index <= 0) then
            print *, "ERROR: Failed to push array_slice to arena"
            stop 1
        end if
        node_idx = arena%size
        
        ! Verify flag is preserved
        select type (stored_node => arena%entries(node_idx)%node)
        type is (array_slice_node)
            if (stored_node%is_character_substring) then
                print *, "PASS: is_character_substring flag preserved in arena"
            else
                print *, "FAIL: is_character_substring flag not preserved"
                all_passed = .false.
            end if
        class default
            print *, "FAIL: Wrong node type in arena"
            all_passed = .false.
        end select
    end subroutine test_flag_setting

end program test_character_substring_semantic_simple