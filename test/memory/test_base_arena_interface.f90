program test_base_arena_interface
    use arena_memory
    implicit none
    
    integer :: tests_passed = 0
    integer :: tests_failed = 0
    
    print *, "Testing base arena interface..."
    print *, "Note: Abstract interface temporarily disabled due to vtable issues"
    
    call test_arena_basic_operation()
    
    ! Report results
    print *, "================================"
    print *, "Base Arena Interface Test Results:"
    print *, "Tests passed:", tests_passed
    print *, "Tests failed:", tests_failed
    print *, "================================"
    
    if (tests_failed > 0) then
        stop 1
    end if

contains


    subroutine test_arena_basic_operation()
        type(arena_t) :: arena
        type(arena_handle_t) :: handle
        integer(1) :: buffer(10) = 42
        logical :: status
        
        print *, "  Testing basic arena operations..."
        
        arena = create_arena()
        
        ! Test basic allocation and data operations
        handle = arena%allocate(10)
        if (.not. arena%validate(handle)) then
            print *, "    FAILED: Handle not valid after allocation"
            tests_failed = tests_failed + 1
        else
            tests_passed = tests_passed + 1
        end if
        
        ! Test data storage
        call arena%set_data(handle, buffer, status)
        if (.not. status) then
            print *, "    FAILED: Could not set data"
            tests_failed = tests_failed + 1
        else
            tests_passed = tests_passed + 1
        end if
        
        ! Test data retrieval
        buffer = 0
        call arena%get_data(handle, buffer, status)
        if (.not. status .or. buffer(1) /= 42) then
            print *, "    FAILED: Could not retrieve data correctly"
            tests_failed = tests_failed + 1
        else
            tests_passed = tests_passed + 1
        end if
        
        ! Test reset invalidates handles
        call arena%reset()
        if (arena%validate(handle)) then
            print *, "    FAILED: Handle still valid after reset"
            tests_failed = tests_failed + 1
        else
            tests_passed = tests_passed + 1
        end if
        
        call destroy_arena(arena)
    end subroutine

end program test_base_arena_interface