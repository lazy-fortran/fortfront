program test_shared_context
    ! Test basic semantic context functionality
    use semantic_analyzer, only: semantic_context_t, create_semantic_context
    use scope_manager, only: create_scope_stack
    implicit none
    
    logical :: tests_passed = .true.
    
    ! Test basic context functionality  
    call test_context_creation()
    call test_context_copy()
    call test_context_scope_operations()
    
    if (tests_passed) then
        print *, "TEST PASSED: semantic context system"
    else
        print *, "TEST FAILED: semantic context system"
        stop 1
    end if

contains

    subroutine test_context_creation()
        type(semantic_context_t) :: context
        
        context = create_semantic_context()
        
        ! Verify context was created properly
        if (context%next_var_id /= 1) then
            tests_passed = .false.
            print *, "FAILED: context creation test - next_var_id should be 1"
        end if
        
        ! This should not crash
        print *, "Context creation test passed"
    end subroutine test_context_creation

    subroutine test_context_copy()
        type(semantic_context_t) :: original, copied
        
        original = create_semantic_context()
        original%next_var_id = 42
        
        ! Test deep copy
        copied = original%deep_copy()
        
        if (copied%next_var_id /= original%next_var_id) then
            tests_passed = .false.
            print *, "FAILED: context copy test"
        else
            print *, "Context copy test passed"
        end if
    end subroutine test_context_copy

    subroutine test_context_scope_operations()
        type(semantic_context_t) :: context
        
        context = create_semantic_context()
        
        ! Test scope operations exist (these are basic tests)
        call context%scopes%enter_block()
        call context%scopes%leave_scope()
        
        print *, "Scope operations test passed"
    end subroutine test_context_scope_operations

end program test_shared_context