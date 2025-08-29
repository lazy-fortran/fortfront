program test_standardization_integration
    use ast_core
    use codegen_core
    use json_reader
    use json_module
    implicit none
    
    logical :: all_tests_passed
    
    print *, "=== Standardization Integration Tests ==="
    
    all_tests_passed = .true.
    
    ! Test literal standardization through full pipeline
    if (.not. test_real_literal_through_pipeline()) all_tests_passed = .false.
    
    if (all_tests_passed) then
        print *, "All standardization integration tests passed!"
        stop 0
    else
        print *, "Some standardization integration tests failed!"
        stop 1
    end if

contains

    function test_real_literal_through_pipeline() result(passed)
        logical :: passed
        type(ast_arena_t) :: arena
        type(literal_node) :: real_literal
        character(len=:), allocatable :: code_result
        integer :: node_index
        logical :: original_setting
        
        passed = .true.
        
        ! Save original setting
        call get_type_standardization(original_setting)
        
        ! Create arena and literal
        arena = create_ast_arena()
        real_literal = create_literal("1.5", LITERAL_REAL, 1, 1)
        call arena%push(real_literal, "literal", 0)
        node_index = arena%size
        
        ! Test with standardization enabled
        call set_type_standardization(.true.)
        code_result = codegen_core_generate_arena(arena, node_index)
        passed = passed .and. (trim(code_result) == "1.5d0")
        
        ! Test with standardization disabled  
        call set_type_standardization(.false.)
        code_result = codegen_core_generate_arena(arena, node_index)
        passed = passed .and. (trim(code_result) == "1.5")
        
        ! Restore original setting
        call set_type_standardization(original_setting)
        
        if (passed) then
            print *, "PASS: test_real_literal_through_pipeline"
        else
            print *, "FAIL: test_real_literal_through_pipeline"
            print *, "  Last result: '", trim(code_result), "'"
        end if
        
        call arena%clear()
    end function test_real_literal_through_pipeline

end program test_standardization_integration