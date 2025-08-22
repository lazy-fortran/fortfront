program test_arena_memory
    use arena_memory
    implicit none

    integer :: tests_passed = 0
    integer :: tests_failed = 0

    ! Run all tests
    call test_arena_creation()
    call test_basic_allocation()
    call test_multiple_allocations()
    call test_generation_validation()
    call test_arena_reset()
    call test_arena_clear()
    call test_arena_growth()
    call test_alignment()
    call test_stress_allocation()
    call test_handle_validation()
    call test_statistics()
    call test_edge_cases()

    ! Report results
    print *, "================================"
    print *, "Arena Memory Test Results:"
    print *, "Tests passed:", tests_passed
    print *, "Tests failed:", tests_failed
    print *, "================================"
    
    if (tests_failed > 0) then
        stop 1
    end if

contains

    ! Test arena creation and destruction
    subroutine test_arena_creation()
        type(arena_t) :: arena
        type(arena_stats_t) :: stats

        print *, "Testing arena creation..."
        
        ! Create with default size
        arena = create_arena()
        stats = arena%get_stats()
        
        if (stats%chunk_count /= 1) then
            print *, "  FAILED: Expected 1 chunk, got", stats%chunk_count
            tests_failed = tests_failed + 1
        else if (stats%total_capacity <= 0) then
            print *, "  FAILED: Invalid capacity:", stats%total_capacity
            tests_failed = tests_failed + 1
        else if (stats%total_allocated /= 0) then
            print *, "  FAILED: Expected 0 allocated, got", stats%total_allocated
            tests_failed = tests_failed + 1
        else
            print *, "  PASSED: Arena created successfully"
            tests_passed = tests_passed + 1
        end if
        
        call destroy_arena(arena)
        
        ! Create with custom size
        arena = create_arena(8192)
        stats = arena%get_stats()
        
        if (stats%total_capacity < 8192) then
            print *, "  FAILED: Custom size not respected"
            tests_failed = tests_failed + 1
        else
            print *, "  PASSED: Custom size arena created"
            tests_passed = tests_passed + 1
        end if
        
        call destroy_arena(arena)
    end subroutine test_arena_creation

    ! Test basic allocation
    subroutine test_basic_allocation()
        type(arena_t) :: arena
        type(arena_handle_t) :: handle
        integer(1) :: buffer(100), read_buffer(100)
        logical :: status

        print *, "Testing basic allocation..."
        
        arena = create_arena()
        
        ! Allocate 100 bytes
        handle = arena%allocate(100)
        
        if (.not. is_valid_handle(handle)) then
            print *, "  FAILED: Invalid handle returned"
            tests_failed = tests_failed + 1
        else if (handle%size /= 100) then
            print *, "  FAILED: Wrong size in handle"
            tests_failed = tests_failed + 1
        else if (.not. arena%validate(handle)) then
            print *, "  FAILED: Handle validation failed"
            tests_failed = tests_failed + 1
        else
            print *, "  PASSED: Basic allocation successful"
            tests_passed = tests_passed + 1
        end if
        
        ! Write and read data
        buffer = 42  ! Set test data
        call arena%set_data(handle, buffer, status)
        
        if (.not. status) then
            print *, "  FAILED: Could not write data"
            tests_failed = tests_failed + 1
        else
            call arena%get_data(handle, read_buffer, status)
            if (.not. status) then
                print *, "  FAILED: Could not read data"
                tests_failed = tests_failed + 1
            else if (all(read_buffer == 42)) then
                print *, "  PASSED: Data access works"
                tests_passed = tests_passed + 1
            else
                print *, "  FAILED: Data corruption detected"
                tests_failed = tests_failed + 1
            end if
        end if
        
        call destroy_arena(arena)
    end subroutine test_basic_allocation

    ! Test multiple allocations
    subroutine test_multiple_allocations()
        type(arena_t) :: arena
        type(arena_handle_t) :: handles(10)
        type(arena_stats_t) :: stats
        integer :: i, total_size

        print *, "Testing multiple allocations..."
        
        arena = create_arena()
        total_size = 0
        
        ! Allocate 10 blocks of different sizes
        do i = 1, 10
            handles(i) = arena%allocate(i * 10)
            total_size = total_size + align_to_8(i * 10)
        end do
        
        ! Validate all handles
        do i = 1, 10
            if (.not. arena%validate(handles(i))) then
                print *, "  FAILED: Handle", i, "invalid"
                tests_failed = tests_failed + 1
                return
            end if
        end do
        
        stats = arena%get_stats()
        if (stats%total_allocated < total_size) then
            print *, "  FAILED: Allocated size mismatch"
            tests_failed = tests_failed + 1
        else
            print *, "  PASSED: Multiple allocations successful"
            tests_passed = tests_passed + 1
        end if
        
        call destroy_arena(arena)
    end subroutine test_multiple_allocations

    ! Test generation validation
    subroutine test_generation_validation()
        type(arena_t) :: arena
        type(arena_handle_t) :: handle1, handle2

        print *, "Testing generation validation..."
        
        arena = create_arena()
        
        ! First allocation
        handle1 = arena%allocate(100)
        
        ! Reset arena (invalidates handle1)
        call arena%reset()
        
        ! Second allocation (same memory location)
        handle2 = arena%allocate(100)
        
        ! Old handle should be invalid
        if (arena%validate(handle1)) then
            print *, "  FAILED: Old handle still valid after reset"
            tests_failed = tests_failed + 1
        else
            print *, "  PASSED: Old handle invalidated"
            tests_passed = tests_passed + 1
        end if
        
        ! New handle should be valid
        if (.not. arena%validate(handle2)) then
            print *, "  FAILED: New handle invalid"
            tests_failed = tests_failed + 1
        else
            print *, "  PASSED: New handle valid"
            tests_passed = tests_passed + 1
        end if
        
        call destroy_arena(arena)
    end subroutine test_generation_validation

    ! Test arena reset
    subroutine test_arena_reset()
        type(arena_t) :: arena
        type(arena_stats_t) :: stats_before, stats_after
        type(arena_handle_t) :: handle

        print *, "Testing arena reset..."
        
        arena = create_arena()
        
        ! Allocate some memory
        handle = arena%allocate(1000)
        stats_before = arena%get_stats()
        
        ! Reset arena
        call arena%reset()
        stats_after = arena%get_stats()
        
        if (stats_after%total_allocated /= 0) then
            print *, "  FAILED: Memory not reset"
            tests_failed = tests_failed + 1
        else if (stats_after%total_capacity /= stats_before%total_capacity) then
            print *, "  FAILED: Capacity changed after reset"
            tests_failed = tests_failed + 1
        else if (stats_after%current_generation <= stats_before%current_generation) then
            print *, "  FAILED: Generation not incremented"
            tests_failed = tests_failed + 1
        else
            print *, "  PASSED: Arena reset successful"
            tests_passed = tests_passed + 1
        end if
        
        call destroy_arena(arena)
    end subroutine test_arena_reset

    ! Test arena clear
    subroutine test_arena_clear()
        type(arena_t) :: arena
        type(arena_stats_t) :: stats
        type(arena_handle_t) :: handle
        integer :: i

        print *, "Testing arena clear..."
        
        arena = create_arena(4096)
        
        ! Force multiple chunks
        do i = 1, 20
            handle = arena%allocate(1000)
        end do
        
        stats = arena%get_stats()
        if (stats%chunk_count <= 1) then
            print *, "  WARNING: Could not create multiple chunks for test"
        end if
        
        ! Clear arena
        call arena%clear()
        stats = arena%get_stats()
        
        if (stats%chunk_count /= 1) then
            print *, "  FAILED: Chunks not reduced to 1"
            tests_failed = tests_failed + 1
        else if (stats%total_allocated /= 0) then
            print *, "  FAILED: Memory not cleared"
            tests_failed = tests_failed + 1
        else
            print *, "  PASSED: Arena clear successful"
            tests_passed = tests_passed + 1
        end if
        
        call destroy_arena(arena)
    end subroutine test_arena_clear

    ! Test arena growth
    subroutine test_arena_growth()
        type(arena_t) :: arena
        type(arena_stats_t) :: stats_before, stats_after
        type(arena_handle_t) :: handle

        print *, "Testing arena growth..."
        
        arena = create_arena(512)  ! Small initial size (below MIN_CHUNK_SIZE)
        stats_before = arena%get_stats()
        
        ! Allocate more than chunk size
        handle = arena%allocate(5000)  ! More than 4096
        stats_after = arena%get_stats()
        
        if (.not. is_valid_handle(handle)) then
            print *, "  FAILED: Large allocation failed"
            tests_failed = tests_failed + 1
        else if (stats_after%chunk_count <= stats_before%chunk_count) then
            print *, "  FAILED: Arena did not grow"
            tests_failed = tests_failed + 1
        else if (stats_after%total_capacity <= stats_before%total_capacity) then
            print *, "  FAILED: Capacity did not increase"
            tests_failed = tests_failed + 1
        else
            print *, "  PASSED: Arena growth successful"
            tests_passed = tests_passed + 1
        end if
        
        call destroy_arena(arena)
    end subroutine test_arena_growth

    ! Test memory alignment
    subroutine test_alignment()
        type(arena_t) :: arena
        type(arena_handle_t) :: h1, h2, h3
        integer :: expected_offset

        print *, "Testing memory alignment..."
        
        arena = create_arena()
        
        ! Allocate unaligned sizes
        h1 = arena%allocate(7)   ! Will be aligned to 8
        h2 = arena%allocate(13)  ! Will be aligned to 16
        h3 = arena%allocate(8)   ! Already aligned
        
        ! Check alignment
        if (mod(h1%offset, 8) /= 0) then
            print *, "  FAILED: First allocation not aligned"
            tests_failed = tests_failed + 1
        else if (h2%offset /= 8) then  ! Should start after aligned h1
            print *, "  FAILED: Second allocation misaligned"
            tests_failed = tests_failed + 1
        else if (h3%offset /= 24) then  ! Should start after aligned h2
            print *, "  FAILED: Third allocation misaligned"
            tests_failed = tests_failed + 1
        else
            print *, "  PASSED: Memory alignment correct"
            tests_passed = tests_passed + 1
        end if
        
        call destroy_arena(arena)
    end subroutine test_alignment

    ! Stress test with many allocations
    subroutine test_stress_allocation()
        type(arena_t) :: arena
        type(arena_handle_t) :: handle
        type(arena_stats_t) :: stats
        integer :: i, allocation_count
        real :: start_time, end_time, elapsed_ns

        print *, "Testing stress allocation..."
        
        arena = create_arena()
        allocation_count = 10000
        
        call cpu_time(start_time)
        
        ! Allocate many small blocks
        do i = 1, allocation_count
            handle = arena%allocate(mod(i, 100) + 1)
            if (.not. is_valid_handle(handle)) then
                print *, "  FAILED: Allocation", i, "failed"
                tests_failed = tests_failed + 1
                call destroy_arena(arena)
                return
            end if
        end do
        
        call cpu_time(end_time)
        elapsed_ns = (end_time - start_time) * 1e9 / allocation_count
        
        stats = arena%get_stats()
        
        if (stats%total_allocated <= 0) then
            print *, "  FAILED: No memory allocated"
            tests_failed = tests_failed + 1
        else
            print *, "  PASSED: Stress test completed"
            print *, "    Allocations:", allocation_count
            print *, "    Total allocated:", stats%total_allocated, "bytes"
            print *, "    Chunks used:", stats%chunk_count
            print *, "    Utilization:", int(stats%utilization * 100), "%"
            print *, "    Time per allocation:", int(elapsed_ns), "ns (approx)"
            tests_passed = tests_passed + 1
        end if
        
        call destroy_arena(arena)
    end subroutine test_stress_allocation

    ! Test handle validation edge cases
    subroutine test_handle_validation()
        type(arena_t) :: arena
        type(arena_handle_t) :: handle, bad_handle

        print *, "Testing handle validation..."
        
        arena = create_arena()
        
        ! Test null handle
        bad_handle = null_handle()
        if (is_valid_handle(bad_handle)) then
            print *, "  FAILED: Null handle appears valid"
            tests_failed = tests_failed + 1
        else if (arena%validate(bad_handle)) then
            print *, "  FAILED: Arena validated null handle"
            tests_failed = tests_failed + 1
        else
            print *, "  PASSED: Null handle rejected"
            tests_passed = tests_passed + 1
        end if
        
        ! Test corrupted handle
        handle = arena%allocate(100)
        bad_handle = handle
        bad_handle%chunk_id = 999  ! Invalid chunk
        
        if (arena%validate(bad_handle)) then
            print *, "  FAILED: Invalid chunk ID accepted"
            tests_failed = tests_failed + 1
        else
            print *, "  PASSED: Invalid chunk rejected"
            tests_passed = tests_passed + 1
        end if
        
        ! Test negative offset
        bad_handle = handle
        bad_handle%offset = -1
        
        if (arena%validate(bad_handle)) then
            print *, "  FAILED: Negative offset accepted"
            tests_failed = tests_failed + 1
        else
            print *, "  PASSED: Negative offset rejected"
            tests_passed = tests_passed + 1
        end if
        
        call destroy_arena(arena)
    end subroutine test_handle_validation

    ! Test statistics reporting
    subroutine test_statistics()
        type(arena_t) :: arena
        type(arena_stats_t) :: stats
        type(arena_handle_t) :: handle

        print *, "Testing statistics..."
        
        arena = create_arena(4096)
        
        ! Initial stats
        stats = arena%get_stats()
        if (stats%utilization /= 0.0) then
            print *, "  FAILED: Initial utilization not zero"
            tests_failed = tests_failed + 1
        else
            tests_passed = tests_passed + 1
        end if
        
        ! After allocation
        handle = arena%allocate(2048)
        stats = arena%get_stats()
        
        if (stats%utilization <= 0.0 .or. stats%utilization > 1.0) then
            print *, "  FAILED: Invalid utilization:", stats%utilization
            tests_failed = tests_failed + 1
        else
            print *, "  PASSED: Statistics tracking works"
            print *, "    Utilization:", int(stats%utilization * 100), "%"
            tests_passed = tests_passed + 1
        end if
        
        call destroy_arena(arena)
    end subroutine test_statistics

    ! Test edge cases
    subroutine test_edge_cases()
        type(arena_t) :: arena
        type(arena_handle_t) :: handle

        print *, "Testing edge cases..."
        
        arena = create_arena()
        
        ! Zero-size allocation
        handle = arena%allocate(0)
        if (is_valid_handle(handle)) then
            print *, "  FAILED: Zero-size allocation accepted"
            tests_failed = tests_failed + 1
        else
            print *, "  PASSED: Zero-size allocation rejected"
            tests_passed = tests_passed + 1
        end if
        
        ! Negative size allocation
        handle = arena%allocate(-100)
        if (is_valid_handle(handle)) then
            print *, "  FAILED: Negative size allocation accepted"
            tests_failed = tests_failed + 1
        else
            print *, "  PASSED: Negative size allocation rejected"
            tests_passed = tests_passed + 1
        end if
        
        ! Very large allocation
        handle = arena%allocate(100000000)  ! 100MB
        if (.not. is_valid_handle(handle)) then
            print *, "  FAILED: Large allocation failed"
            tests_failed = tests_failed + 1
        else
            print *, "  PASSED: Large allocation handled"
            tests_passed = tests_passed + 1
        end if
        
        call destroy_arena(arena)
    end subroutine test_edge_cases

    ! Helper: Align to 8 bytes
    pure function align_to_8(size) result(aligned)
        integer, intent(in) :: size
        integer :: aligned
        integer :: remainder

        remainder = mod(size, 8)
        if (remainder == 0) then
            aligned = size
        else
            aligned = size + (8 - remainder)
        end if
    end function align_to_8

end program test_arena_memory