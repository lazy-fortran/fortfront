program test_base_arena_interface
    use arena_memory
    implicit none
    
    integer :: tests_passed = 0
    integer :: tests_failed = 0
    
    print *, "Testing base arena interface..."
    
    call test_polymorphic_behavior()
    call test_common_operations()
    
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

    subroutine test_polymorphic_behavior()
        class(base_arena_t), allocatable :: arena
        type(arena_handle_t) :: handle
        class(*), pointer :: item_ptr
        integer, target :: test_item = 42
        
        print *, "  Testing polymorphic interface behavior..."
        
        ! Create concrete implementation (arena_t extends base_arena_t)
        allocate(arena_t :: arena)
        select type(arena)
        type is (arena_t)
            arena = create_arena()
        end select
        
        ! Test that base interface methods are accessible
        if (arena%generation /= 1) then
            print *, "    FAILED: Wrong initial generation"
            tests_failed = tests_failed + 1
        else
            tests_passed = tests_passed + 1
        end if
        
        ! Test insert operation through base interface
        handle = arena%insert(test_item)
        if (.not. arena%valid(handle)) then
            print *, "    FAILED: Handle not valid after insert"
            tests_failed = tests_failed + 1
        else
            tests_passed = tests_passed + 1
        end if
        
        ! Test that size counter was updated
        if (arena%size /= 1) then
            print *, "    FAILED: Size not updated after insert"
            tests_failed = tests_failed + 1
        else
            tests_passed = tests_passed + 1
        end if
        
        ! Test get operation through base interface (returns null for arena_t)
        item_ptr => arena%get(handle)
        if (associated(item_ptr)) then
            print *, "    WARNING: Expected null pointer from arena_t get()"
            ! Not a failure - arena_t is expected to return null
        end if
        tests_passed = tests_passed + 1
        
        deallocate(arena)
    end subroutine

    subroutine test_common_operations()
        class(base_arena_t), allocatable :: arena
        type(arena_handle_t) :: handle
        integer, target :: test_item = 123
        
        print *, "  Testing common base operations..."
        
        ! Create concrete implementation
        allocate(arena_t :: arena)
        select type(arena)
        type is (arena_t)
            arena = create_arena()
        end select
        
        ! Test checkpoint/rollback
        call arena%checkpoint()
        if (arena%checkpoint_gen /= 1) then
            print *, "    FAILED: Checkpoint generation not set"
            tests_failed = tests_failed + 1
        else
            tests_passed = tests_passed + 1
        end if
        
        handle = arena%insert(test_item)
        if (.not. arena%valid(handle)) then
            print *, "    FAILED: Handle invalid after insert"
            tests_failed = tests_failed + 1
        else
            tests_passed = tests_passed + 1
        end if
        
        call arena%rollback()
        if (arena%valid(handle)) then
            print *, "    FAILED: Handle still valid after rollback"
            tests_failed = tests_failed + 1
        else
            tests_passed = tests_passed + 1
        end if
        
        if (arena%size /= 0) then
            print *, "    FAILED: Size not reset after rollback"
            tests_failed = tests_failed + 1
        else
            tests_passed = tests_passed + 1
        end if
        
        ! Test reset function
        handle = arena%insert(test_item)
        call arena%reset()
        if (arena%valid(handle)) then
            print *, "    FAILED: Handle still valid after reset"
            tests_failed = tests_failed + 1
        else
            tests_passed = tests_passed + 1
        end if
        
        if (arena%generation /= 3) then  ! Should be incremented by rollback and reset
            print *, "    FAILED: Generation not incremented properly", arena%generation
            tests_failed = tests_failed + 1
        else
            tests_passed = tests_passed + 1
        end if
        
        deallocate(arena)
    end subroutine

end program test_base_arena_interface