program test_intrinsic_coverage
    use fortfront
    use intrinsic_registry, only: registry_is_intrinsic => is_intrinsic_function, &
                                  registry_get_signature => get_intrinsic_signature, &
                                  get_intrinsic_info, initialize_intrinsic_registry
    implicit none
    
    logical :: all_tests_passed
    
    print *, "=== Intrinsic Registry Coverage Tests ==="
    all_tests_passed = .true.
    
    ! Test case-insensitive function names
    if (.not. test_case_insensitive()) all_tests_passed = .false.
    
    ! Test JSON serialization with intrinsic functions
    if (.not. test_json_serialization()) all_tests_passed = .false.
    
    ! Test assignment operator with intrinsic data
    if (.not. test_assignment_operator()) all_tests_passed = .false.
    
    ! Test helper functions
    if (.not. test_helper_functions()) all_tests_passed = .false.
    
    ! Test registry initialization edge cases
    if (.not. test_registry_edge_cases()) all_tests_passed = .false.
    
    if (all_tests_passed) then
        print *, "All coverage tests passed!"
    else
        print *, "Some coverage tests failed!"
        stop 1
    end if
    
contains
    
    logical function test_case_insensitive()
        logical :: result
        character(len=:), allocatable :: signature
        
        test_case_insensitive = .true.
        print *, "Testing case-insensitive intrinsic recognition..."
        
        ! Test uppercase
        result = registry_is_intrinsic("SIN")
        if (.not. result) then
            print *, "  FAIL: SIN not recognized"
            test_case_insensitive = .false.
        else
            print *, "  PASS: SIN recognized"
        end if
        
        ! Test mixed case
        result = registry_is_intrinsic("CoS")
        if (.not. result) then
            print *, "  FAIL: CoS not recognized"
            test_case_insensitive = .false.
        else
            print *, "  PASS: CoS recognized"
        end if
        
        ! Test signature retrieval with mixed case
        signature = registry_get_signature("SQRT")
        if (len_trim(signature) == 0) then
            print *, "  FAIL: SQRT signature empty"
            test_case_insensitive = .false.
        else
            print *, "  PASS: SQRT signature: ", signature
        end if
        
    end function test_case_insensitive
    
    logical function test_json_serialization()
        character(len=*), parameter :: source = &
            "program test" // new_line('A') // &
            "    real :: x, y" // new_line('A') // &
            "    x = 3.14" // new_line('A') // &
            "    y = abs(x)" // new_line('A') // &
            "end program test"
        
        type(token_t), allocatable :: tokens(:)
        type(ast_arena_t) :: arena
        integer :: prog_index
        character(len=:), allocatable :: error_msg
        integer :: i
        logical :: found_abs_call
        
        test_json_serialization = .true.
        found_abs_call = .false.
        print *, "Testing JSON serialization with intrinsic functions..."
        
        call lex_source(source, tokens, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Lex error"
            test_json_serialization = .false.
            return
        end if
        
        arena = create_ast_arena()
        call parse_tokens(tokens, arena, prog_index, error_msg)
        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "  FAIL: Parse error"
            test_json_serialization = .false.
            return
        end if
        
        ! Test JSON serialization by looking for abs call
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                select type (node => arena%entries(i)%node)
                type is (call_or_subscript_node)
                    if (allocated(node%name) .and. node%name == "abs") then
                        found_abs_call = .true.
                        
                        ! Test that JSON fields are properly set
                        if (.not. node%is_intrinsic) then
                            print *, "  FAIL: abs not marked as intrinsic"
                            test_json_serialization = .false.
                        end if
                        
                        if (.not. allocated(node%intrinsic_signature) .or. &
                            len_trim(node%intrinsic_signature) == 0) then
                            print *, "  FAIL: abs signature missing or empty"
                            test_json_serialization = .false.
                        else
                            print *, "  PASS: abs has signature: ", node%intrinsic_signature
                        end if
                        exit
                    end if
                end select
            end if
        end do
        
        if (.not. found_abs_call) then
            print *, "  FAIL: abs call not found"
            test_json_serialization = .false.
        else
            print *, "  PASS: JSON serialization fields verified"
        end if
        
    end function test_json_serialization
    
    logical function test_assignment_operator()
        type(call_or_subscript_node) :: src_node, dest_node
        
        test_assignment_operator = .true.
        print *, "Testing assignment operator with intrinsic data..."
        
        ! Set up source node with intrinsic data
        src_node%name = "log"
        src_node%is_intrinsic = .true.
        src_node%intrinsic_signature = "real(real)"
        src_node%line = 10
        src_node%column = 5
        
        ! Test assignment
        dest_node = src_node
        
        ! Verify assignment worked correctly
        if (.not. allocated(dest_node%name) .or. dest_node%name /= "log") then
            print *, "  FAIL: Name not copied"
            test_assignment_operator = .false.
        end if
        
        if (.not. dest_node%is_intrinsic) then
            print *, "  FAIL: is_intrinsic not copied"
            test_assignment_operator = .false.
        end if
        
        if (.not. allocated(dest_node%intrinsic_signature) .or. &
            dest_node%intrinsic_signature /= "real(real)") then
            print *, "  FAIL: intrinsic_signature not copied"
            test_assignment_operator = .false.
        end if
        
        if (dest_node%line /= 10 .or. dest_node%column /= 5) then
            print *, "  FAIL: line/column not copied"
            test_assignment_operator = .false.
        end if
        
        if (test_assignment_operator) then
            print *, "  PASS: Assignment operator works correctly"
        end if
        
    end function test_assignment_operator
    
    logical function test_helper_functions()
        logical :: is_intrinsic
        character(len=:), allocatable :: signature
        
        test_helper_functions = .true.
        print *, "Testing helper functions..."
        
        ! Test get_intrinsic_info directly
        call get_intrinsic_info("exp", is_intrinsic, signature)
        
        if (.not. is_intrinsic) then
            print *, "  FAIL: exp not recognized by get_intrinsic_info"
            test_helper_functions = .false.
        else
            print *, "  PASS: exp recognized by get_intrinsic_info"
        end if
        
        if (.not. allocated(signature) .or. len_trim(signature) == 0) then
            print *, "  FAIL: exp signature empty from get_intrinsic_info"
            test_helper_functions = .false.
        else
            print *, "  PASS: exp signature from get_intrinsic_info: ", signature
        end if
        
        ! Test unknown function
        call get_intrinsic_info("unknown_func", is_intrinsic, signature)
        
        if (is_intrinsic) then
            print *, "  FAIL: unknown_func incorrectly recognized"
            test_helper_functions = .false.
        else
            print *, "  PASS: unknown_func correctly not recognized"
        end if
        
        if (allocated(signature)) then
            print *, "  FAIL: unknown_func has signature when it shouldn't"
            test_helper_functions = .false.
        else
            print *, "  PASS: unknown_func has no signature"
        end if
        
    end function test_helper_functions
    
    logical function test_registry_edge_cases()
        test_registry_edge_cases = .true.
        print *, "Testing registry edge cases..."
        
        ! Test double initialization (should be safe)
        call initialize_intrinsic_registry()
        call initialize_intrinsic_registry()
        
        print *, "  PASS: Double initialization handled safely"
        
        ! Test empty string
        if (registry_is_intrinsic("")) then
            print *, "  FAIL: Empty string recognized as intrinsic"
            test_registry_edge_cases = .false.
        else
            print *, "  PASS: Empty string correctly not recognized"
        end if
        
        ! Test whitespace-only string
        if (registry_is_intrinsic("   ")) then
            print *, "  FAIL: Whitespace string recognized as intrinsic"
            test_registry_edge_cases = .false.
        else
            print *, "  PASS: Whitespace string correctly not recognized"
        end if
        
    end function test_registry_edge_cases
    
end program test_intrinsic_coverage