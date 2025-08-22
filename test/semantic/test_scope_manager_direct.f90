program test_scope_manager_direct
    use scope_manager
    use type_system_unified
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== Scope Manager Direct Tests ==="
    
    ! Test basic scope operations
    call test_scope_creation()
    call test_scope_stack_creation()
    call test_scope_stack_operations()
    call test_scope_types()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All scope manager tests passed!"
        stop 0
    else
        print *, "Some scope manager tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_scope_creation()
        type(scope_t) :: scope
        
        call test_start("Basic scope creation")
        
        scope = create_scope(SCOPE_GLOBAL, "global")
        
        if (scope%scope_type == SCOPE_GLOBAL .and. &
            allocated(scope%name) .and. scope%name == "global") then
            call test_pass()
        else
            call test_fail("Scope not created correctly")
        end if
    end subroutine test_scope_creation
    
    subroutine test_scope_stack_creation()
        type(scope_stack_t) :: stack
        
        call test_start("Scope stack creation")
        
        stack = create_scope_stack()
        
        if (stack%depth == 1) then  ! Should have global scope
            call test_pass()
        else
            call test_fail("Stack not initialized with global scope")
        end if
    end subroutine test_scope_stack_creation
    
    subroutine test_scope_stack_operations()
        type(scope_stack_t) :: stack
        type(scope_t) :: scope
        integer :: initial_depth
        
        call test_start("Scope stack push/pop")
        
        stack = create_scope_stack()
        initial_depth = stack%depth
        
        ! Push a new scope
        scope = create_scope(SCOPE_FUNCTION, "test")
        call stack%push(scope)
        
        if (stack%depth == initial_depth + 1) then
            ! Pop the scope
            call stack%pop()
            if (stack%depth == initial_depth) then
                call test_pass()
            else
                call test_fail("Pop didn't restore correct depth")
            end if
        else
            call test_fail("Push didn't increase depth")
        end if
    end subroutine test_scope_stack_operations
    
    subroutine test_scope_types()
        type(scope_stack_t) :: stack
        
        call test_start("Different scope types")
        
        stack = create_scope_stack()
        
        ! Test entering different scope types
        call stack%enter_module("test_module")
        if (stack%get_current_scope_type() == SCOPE_MODULE) then
            call stack%leave_scope()
            
            call stack%enter_function("test_func")
            if (stack%get_current_scope_type() == SCOPE_FUNCTION) then
                call stack%leave_scope()
                
                call stack%enter_subroutine("test_sub")
                if (stack%get_current_scope_type() == SCOPE_SUBROUTINE) then
                    call test_pass()
                else
                    call test_fail("Subroutine scope not correct")
                end if
            else
                call test_fail("Function scope not correct")
            end if
        else
            call test_fail("Module scope not correct")
        end if
    end subroutine test_scope_types
    
    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)', advance='no') "Testing: ", test_name
    end subroutine test_start
    
    subroutine test_pass()
        print *, " ... PASSED"
        pass_count = pass_count + 1
    end subroutine test_pass
    
    subroutine test_fail(reason)
        character(len=*), intent(in) :: reason
        print *, " ... FAILED"
        print *, "  Reason: ", reason
    end subroutine test_fail
    
end program test_scope_manager_direct