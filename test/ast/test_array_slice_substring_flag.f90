program test_array_slice_substring_flag
    use ast_core
    use ast_nodes_bounds, only: array_slice_node
    use json_module
    implicit none
    
    logical :: all_passed
    all_passed = .true.
    
    call test_array_slice_json_with_flag(all_passed)
    call test_array_slice_default_flag(all_passed)
    
    if (all_passed) then
        print *, "All array_slice substring flag tests passed!"
    else
        print *, "Some array_slice substring flag tests FAILED!"
        stop 1
    end if
    
contains

    subroutine test_array_slice_json_with_flag(all_passed)
        logical, intent(inout) :: all_passed
        type(array_slice_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root
        character(len=:), allocatable :: json_str
        logical :: test_passed, found, bool_val
        
        test_passed = .true.
        
        ! Create node with substring flag set
        node%array_index = 1
        node%bounds_indices(1) = 2
        node%num_dimensions = 1
        node%is_character_substring = .true.
        node%line = 10
        node%column = 20
        
        ! Test JSON serialization
        call json%initialize()
        nullify(root)
        call node%to_json(json, root)
        
        ! Convert to string to verify
        call json%print_to_string(root, json_str)
        
        ! Debug: print the JSON
        print *, "JSON output: ", json_str
        
        ! Check that JSON contains is_character_substring field
        if (index(json_str, '"is_character_substring"') == 0) then
            print *, "FAIL: JSON missing is_character_substring field"
            test_passed = .false.
            all_passed = .false.
        end if
        
        if (index(json_str, '"is_character_substring": true') == 0 .and. &
            index(json_str, '"is_character_substring":true') == 0) then
            print *, "FAIL: is_character_substring not set to true in JSON"
            test_passed = .false.
            all_passed = .false.
        end if
        
        if (test_passed) then
            print *, "PASS: array_slice JSON includes is_character_substring flag"
        end if
        
        call json%destroy(root)
    end subroutine test_array_slice_json_with_flag
    
    subroutine test_array_slice_default_flag(all_passed)
        logical, intent(inout) :: all_passed
        type(array_slice_node) :: node
        type(json_core) :: json
        type(json_value), pointer :: root
        character(len=:), allocatable :: json_str
        logical :: test_passed
        
        test_passed = .true.
        
        ! Create node with default flag (should be false)
        node%array_index = 5
        node%bounds_indices(1) = 6
        node%num_dimensions = 1
        ! Don't set is_character_substring explicitly
        
        ! Test JSON serialization
        call json%initialize()
        nullify(root)
        call node%to_json(json, root)
        
        ! Convert to string
        call json%print_to_string(root, json_str)
        
        ! Check that JSON contains false value
        if (index(json_str, '"is_character_substring": false') == 0 .and. &
            index(json_str, '"is_character_substring":false') == 0) then
            print *, "FAIL: is_character_substring should default to false"
            test_passed = .false.
            all_passed = .false.
        end if
        
        if (test_passed) then
            print *, "PASS: array_slice flag defaults to false correctly"
        end if
        
        call json%destroy(root)
    end subroutine test_array_slice_default_flag

end program test_array_slice_substring_flag