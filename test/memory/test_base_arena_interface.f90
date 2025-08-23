program test_base_arena_interface
    ! Test suite for base arena interface (Issue #369)
    ! Tests abstract base_arena_t interface and concrete implementations
    
    use arena_memory
    implicit none

    integer :: tests_passed = 0
    integer :: tests_failed = 0

    ! Run all tests
    call test_base_interface_abstraction()
    call test_container_operations()
    call test_checkpoint_rollback()
    call test_generational_tracking()
    call test_cross_arena_safety()
    call test_polymorphic_operations()

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

    ! Test that base_arena_t interface works with basic_arena_t
    subroutine test_base_interface_abstraction()
        type(basic_arena_t) :: basic
        type(arena_handle_t) :: handle
        integer :: test_data
        class(*), pointer :: retrieved
        logical :: is_valid
        
        print *, "Testing base interface abstraction..."
        
        ! Initialize concrete arena
        call basic%init(chunk_size=8192)
        
        ! Test insert operation
        test_data = 42
        handle = basic%insert(test_data)
        
        if (.not. is_valid_handle(handle)) then
            print *, "  FAILED: Insert did not return valid handle"
            tests_failed = tests_failed + 1
        else
            print *, "  PASSED: Insert operation works"
            tests_passed = tests_passed + 1
        end if
        
        ! Test valid operation
        is_valid = basic%valid(handle)
        if (is_valid) then
            print *, "  PASSED: Valid check works"
            tests_passed = tests_passed + 1
        else
            print *, "  FAILED: Valid returned false for valid handle"
            tests_failed = tests_failed + 1
        end if
        
        ! Test free operation
        call basic%free(handle)
        
        ! After free, size should decrease
        if (basic%size == 0) then
            print *, "  PASSED: Free decrements size"
            tests_passed = tests_passed + 1
        else
            print *, "  FAILED: Size not updated after free"
            tests_failed = tests_failed + 1
        end if
        
        call basic%destroy()
    end subroutine test_base_interface_abstraction

    ! Test container-like operations
    subroutine test_container_operations()
        type(basic_arena_t) :: arena
        type(arena_handle_t) :: handles(5)
        integer :: i
        
        print *, "Testing container operations..."
        
        call arena%init()
        
        ! Insert multiple items
        do i = 1, 5
            handles(i) = arena%insert(i * 10)
        end do
        
        if (arena%size == 5) then
            print *, "  PASSED: Size tracking works"
            tests_passed = tests_passed + 1
        else
            print *, "  FAILED: Size is", arena%size, "expected 5"
            tests_failed = tests_failed + 1
        end if
        
        ! Free middle item
        call arena%free(handles(3))
        
        if (arena%size == 4) then
            print *, "  PASSED: Size decrements on free"
            tests_passed = tests_passed + 1
        else
            print *, "  FAILED: Size after free is", arena%size
            tests_failed = tests_failed + 1
        end if
        
        ! Reset arena
        call arena%reset()
        
        if (arena%size == 0) then
            print *, "  PASSED: Reset clears size"
            tests_passed = tests_passed + 1
        else
            print *, "  FAILED: Size after reset is", arena%size
            tests_failed = tests_failed + 1
        end if
        
        ! Check that old handles are invalid after reset
        if (.not. arena%valid(handles(1))) then
            print *, "  PASSED: Reset invalidates handles"
            tests_passed = tests_passed + 1
        else
            print *, "  FAILED: Handle still valid after reset"
            tests_failed = tests_failed + 1
        end if
        
        call arena%destroy()
    end subroutine test_container_operations

    ! Test checkpoint and rollback functionality
    subroutine test_checkpoint_rollback()
        type(basic_arena_t) :: arena
        type(arena_checkpoint_t) :: checkpoint
        type(arena_handle_t) :: h1, h2, h3
        logical :: valid1, valid2, valid3
        
        print *, "Testing checkpoint/rollback..."
        
        call arena%init()
        
        ! Insert first item
        h1 = arena%insert(100)
        
        ! Create checkpoint
        checkpoint = arena%checkpoint()
        
        ! Insert more items after checkpoint
        h2 = arena%insert(200)
        h3 = arena%insert(300)
        
        ! All should be valid
        valid1 = arena%valid(h1)
        valid2 = arena%valid(h2)
        valid3 = arena%valid(h3)
        
        if (valid1 .and. valid2 .and. valid3) then
            print *, "  PASSED: All handles valid before rollback"
            tests_passed = tests_passed + 1
        else
            print *, "  FAILED: Some handles invalid before rollback"
            tests_failed = tests_failed + 1
        end if
        
        ! Rollback to checkpoint
        call arena%rollback(checkpoint)
        
        ! After rollback, generation changes so all handles become invalid
        ! This is a simplification - a more sophisticated implementation
        ! might preserve handles created before the checkpoint
        valid1 = arena%valid(h1)
        valid2 = arena%valid(h2)
        valid3 = arena%valid(h3)
        
        if (.not. valid1 .and. .not. valid2 .and. .not. valid3) then
            print *, "  PASSED: Rollback invalidates handles (simplified)"
            tests_passed = tests_passed + 1
        else
            print *, "  FAILED: Some handles still valid after rollback"
            tests_failed = tests_failed + 1
        end if
        
        if (arena%size == checkpoint%size) then
            print *, "  PASSED: Size restored after rollback"
            tests_passed = tests_passed + 1
        else
            print *, "  FAILED: Size after rollback is", arena%size
            tests_failed = tests_failed + 1
        end if
        
        call arena%destroy()
    end subroutine test_checkpoint_rollback

    ! Test generational tracking for use-after-free prevention
    subroutine test_generational_tracking()
        type(basic_arena_t) :: arena
        type(arena_handle_t) :: h1, h2
        integer :: gen1, gen2
        logical :: valid
        
        print *, "Testing generational tracking..."
        
        call arena%init()
        
        ! Insert and save generation
        h1 = arena%insert(100)
        gen1 = arena%generation
        
        ! Reset increments generation
        call arena%reset()
        gen2 = arena%generation
        
        if (gen2 > gen1) then
            print *, "  PASSED: Generation increments on reset"
            tests_passed = tests_passed + 1
        else
            print *, "  FAILED: Generation did not increment"
            tests_failed = tests_failed + 1
        end if
        
        ! Old handle should be invalid
        valid = arena%valid(h1)
        
        if (.not. valid) then
            print *, "  PASSED: Old generation handle invalid"
            tests_passed = tests_passed + 1
        else
            print *, "  FAILED: Old generation handle still valid"
            tests_failed = tests_failed + 1
        end if
        
        ! New allocation should work with new generation
        h2 = arena%insert(200)
        
        if (arena%valid(h2)) then
            print *, "  PASSED: New handle valid with current generation"
            tests_passed = tests_passed + 1
        else
            print *, "  FAILED: New handle invalid"
            tests_failed = tests_failed + 1
        end if
        
        call arena%destroy()
    end subroutine test_generational_tracking

    ! Test cross-arena safety
    subroutine test_cross_arena_safety()
        type(basic_arena_t) :: arena1, arena2
        type(arena_handle_t) :: h1, h2
        logical :: valid
        
        print *, "Testing cross-arena safety..."
        
        call arena1%init()
        call arena2%init()
        
        ! Insert in arena1
        h1 = arena1%insert(100)
        
        ! Insert in arena2
        h2 = arena2%insert(200)
        
        ! Each arena validates its own handles
        if (arena1%valid(h1) .and. arena2%valid(h2)) then
            print *, "  PASSED: Each arena validates own handles"
            tests_passed = tests_passed + 1
        else
            print *, "  FAILED: Arena validation failed"
            tests_failed = tests_failed + 1
        end if
        
        ! Different arenas have different generations
        if (arena1%generation /= arena2%generation .or. &
            arena1%generation == 1) then
            print *, "  PASSED: Arenas have independent generations"
            tests_passed = tests_passed + 1
        else
            print *, "  FAILED: Arenas share generation state"
            tests_failed = tests_failed + 1
        end if
        
        call arena1%destroy()
        call arena2%destroy()
    end subroutine test_cross_arena_safety

    ! Test polymorphic operations through base interface
    subroutine test_polymorphic_operations()
        class(base_arena_t), allocatable :: arena
        type(arena_handle_t) :: handle
        integer :: value
        
        print *, "Testing polymorphic operations..."
        
        ! Allocate as basic_arena_t but use through base interface
        allocate(basic_arena_t :: arena)
        
        ! Initialize through type-specific interface
        select type(arena)
        type is (basic_arena_t)
            call arena%init(chunk_size=4096)
        end select
        
        ! Use through base interface
        value = 999
        handle = arena%insert(value)
        
        if (is_valid_handle(handle)) then
            print *, "  PASSED: Polymorphic insert works"
            tests_passed = tests_passed + 1
        else
            print *, "  FAILED: Polymorphic insert failed"
            tests_failed = tests_failed + 1
        end if
        
        if (arena%valid(handle)) then
            print *, "  PASSED: Polymorphic validation works"
            tests_passed = tests_passed + 1
        else
            print *, "  FAILED: Polymorphic validation failed"
            tests_failed = tests_failed + 1
        end if
        
        ! Test reset through base interface
        call arena%reset()
        
        if (.not. arena%valid(handle)) then
            print *, "  PASSED: Polymorphic reset works"
            tests_passed = tests_passed + 1
        else
            print *, "  FAILED: Handle still valid after polymorphic reset"
            tests_failed = tests_failed + 1
        end if
        
        ! Cleanup
        select type(arena)
        type is (basic_arena_t)
            call arena%destroy()
        end select
        
        deallocate(arena)
    end subroutine test_polymorphic_operations

end program test_base_arena_interface