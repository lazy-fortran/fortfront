program test_procedure_attributes
    use procedure_attribute_analyzer, only: procedure_attribute_analyzer_t, procedure_info_t
    use ast_core, only: ast_arena_t, create_ast_arena
    implicit none
    
    logical :: tests_passed = .true.
    
    ! Test procedure attribute analysis functionality
    call test_analyzer_creation()
    call test_attribute_extraction()
    call test_has_attribute_interface()
    
    if (tests_passed) then
        print *, "TEST PASSED: procedure attributes analyzer"
    else
        print *, "TEST FAILED: procedure attributes analyzer"
        error stop 1
    end if

contains

    subroutine test_analyzer_creation()
        type(procedure_attribute_analyzer_t) :: analyzer
        type(ast_arena_t) :: arena
        integer :: root_index
        
        arena = create_ast_arena()
        root_index = 1
        
        ! Test that analyzer can be created and analyzed
        call analyzer%analyze(0, arena, root_index)
        
        if (.not. analyzer%analysis_complete) then
            print *, "ERROR: Analysis should complete successfully"
            tests_passed = .false.
            return
        end if
        
        print *, "SUCCESS: Analyzer creation and analysis works"
    end subroutine

    subroutine test_attribute_extraction()
        type(procedure_attribute_analyzer_t) :: analyzer
        type(ast_arena_t) :: arena
        character(:), allocatable :: attributes(:)
        
        arena = create_ast_arena()
        call analyzer%analyze(0, arena, 1)
        
        ! Test getting attributes for a non-existent procedure
        attributes = analyzer%get_all_attributes("nonexistent")
        
        if (.not. allocated(attributes)) then
            print *, "ERROR: get_all_attributes should return allocated array"
            tests_passed = .false.
            return
        end if
        
        if (size(attributes) /= 0) then
            print *, "ERROR: Non-existent procedure should have no attributes"
            tests_passed = .false.
            return
        end if
        
        print *, "SUCCESS: Attribute extraction works"
    end subroutine

    subroutine test_has_attribute_interface()
        type(procedure_attribute_analyzer_t) :: analyzer
        type(ast_arena_t) :: arena
        logical :: has_pure
        
        arena = create_ast_arena()
        call analyzer%analyze(0, arena, 1)
        
        ! Test has_attribute for non-existent procedure
        has_pure = analyzer%has_attribute("nonexistent", "PURE")
        
        if (has_pure) then
            print *, "ERROR: Non-existent procedure should not have PURE attribute"
            tests_passed = .false.
            return
        end if
        
        print *, "SUCCESS: has_attribute interface works"
    end subroutine

end program test_procedure_attributes