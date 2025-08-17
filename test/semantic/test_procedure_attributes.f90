program test_procedure_attributes
    use procedure_attribute_analyzer, only: procedure_attribute_analyzer_t, procedure_info_t
    use ast_core, only: ast_arena_t, create_ast_arena
    implicit none
    
    logical :: tests_passed = .true.
    
    ! Test procedure attribute analysis functionality
    call test_analyzer_creation()
    call test_attribute_extraction()
    call test_has_attribute_interface()
    call test_procedure_registry_operations()
    call test_analyzer_assignment()
    
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

    subroutine test_procedure_registry_operations()
        use procedure_attribute_analyzer, only: procedure_registry_t
        type(procedure_registry_t) :: registry
        type(procedure_info_t) :: proc_info
        type(procedure_info_t), allocatable :: all_procs(:)
        logical :: found
        
        ! Test adding and finding procedures
        proc_info%name = "test_proc"
        proc_info%node_index = 1
        proc_info%declaration_line = 10
        proc_info%is_pure = .true.
        
        call registry%add_procedure(proc_info)
        
        if (registry%procedure_count /= 1) then
            print *, "ERROR: Procedure count should be 1"
            tests_passed = .false.
            return
        end if
        
        ! Test finding by name
        call registry%find_by_name("test_proc", proc_info, found)
        if (.not. found) then
            print *, "ERROR: Should find procedure by name"
            tests_passed = .false.
            return
        end if
        
        if (trim(proc_info%name) /= "test_proc") then
            print *, "ERROR: Found procedure has wrong name"
            tests_passed = .false.
            return
        end if
        
        ! Test finding by node
        call registry%find_by_node(1, proc_info, found)
        if (.not. found) then
            print *, "ERROR: Should find procedure by node"
            tests_passed = .false.
            return
        end if
        
        ! Test getting all procedures
        all_procs = registry%get_all_procedures()
        if (.not. allocated(all_procs) .or. size(all_procs) /= 1) then
            print *, "ERROR: Should return one procedure"
            tests_passed = .false.
            return
        end if
        
        print *, "SUCCESS: Procedure registry operations work"
    end subroutine

    subroutine test_analyzer_assignment()
        type(procedure_attribute_analyzer_t) :: analyzer1, analyzer2
        type(ast_arena_t) :: arena
        
        arena = create_ast_arena()
        call analyzer1%analyze(0, arena, 1)
        analyzer1%analysis_complete = .true.
        
        ! Test assignment
        call analyzer2%assign(analyzer1)
        
        if (.not. analyzer2%analysis_complete) then
            print *, "ERROR: Assignment should copy analysis_complete"
            tests_passed = .false.
            return
        end if
        
        if (analyzer2%registry%procedure_count /= analyzer1%registry%procedure_count) then
            print *, "ERROR: Assignment should copy registry"
            tests_passed = .false.
            return
        end if
        
        print *, "SUCCESS: Analyzer assignment works"
    end subroutine

end program test_procedure_attributes