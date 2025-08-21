program test_type_system_arena
    use type_system_arena
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== Type System Arena Tests ==="
    
    ! Basic arena operations
    call test_arena_creation()
    call test_mono_type_storage()
    call test_poly_type_storage()
    call test_args_array_storage()
    
    ! Handle validation
    call test_handle_validation()
    call test_null_handles()
    
    ! Performance and stress tests
    call test_bulk_allocation()
    call test_arena_reset()
    call test_arena_statistics()
    
    ! Edge cases
    call test_empty_args_array()
    call test_zero_size_allocation()
    call test_invalid_operations()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All type system arena tests passed!"
        stop 0
    else
        print *, "Some type system arena tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_arena_creation()
        type(type_arena_t) :: arena
        
        call test_start("Arena creation and destruction")
        
        arena = create_type_arena()
        if (arena%next_type_id == 1 .and. &
            arena%mono_count == 0 .and. &
            arena%poly_count == 0 .and. &
            arena%args_count == 0) then
            call test_pass()
        else
            call test_fail("Arena not initialized correctly")
        end if
        
        call destroy_type_arena(arena)
    end subroutine test_arena_creation
    
    subroutine test_mono_type_storage()
        type(type_arena_t) :: arena
        type(arena_mono_type_t) :: original, retrieved
        type(mono_handle_t) :: handle
        
        call test_start("Monomorphic type storage and retrieval")
        
        arena = create_type_arena()
        
        ! Create test mono type
        original%kind = 2  ! TINT
        original%var_id = 42
        original%var_name = "test_var"
        original%size = 8
        original%is_allocatable = .true.
        
        ! Store in arena
        handle = store_mono_type(arena, original)
        
        if (.not. is_valid_mono_handle(handle)) then
            call test_fail("Failed to store mono type")
            call destroy_type_arena(arena)
            return
        end if
        
        ! Retrieve from arena
        retrieved = get_mono_type(arena, handle)
        
        ! Verify all fields
        if (retrieved%kind == original%kind .and. &
            retrieved%var_id == original%var_id .and. &
            trim(retrieved%var_name) == trim(original%var_name) .and. &
            retrieved%size == original%size .and. &
            retrieved%is_allocatable .eqv. original%is_allocatable) then
            call test_pass()
        else
            call test_fail("Mono type fields not preserved")
        end if
        
        call destroy_type_arena(arena)
    end subroutine test_mono_type_storage
    
    subroutine test_poly_type_storage()
        type(type_arena_t) :: arena
        type(arena_poly_type_t) :: original, retrieved
        type(poly_handle_t) :: handle
        type(mono_handle_t) :: mono_handle
        type(arena_mono_type_t) :: mono_type
        
        call test_start("Polymorphic type storage and retrieval")
        
        arena = create_type_arena()
        
        ! Create test mono type for poly type
        mono_type%kind = 1  ! TVAR
        mono_type%var_id = 1
        mono_type%var_name = "'a"
        mono_handle = store_mono_type(arena, mono_type)
        
        ! Create test poly type
        original%forall_vars = null_args_handle()  ! No quantified vars for now
        original%mono = mono_handle
        
        ! Store in arena
        handle = store_poly_type(arena, original)
        
        if (.not. is_valid_poly_handle(handle)) then
            call test_fail("Failed to store poly type")
            call destroy_type_arena(arena)
            return
        end if
        
        ! Retrieve from arena
        retrieved = get_poly_type(arena, handle)
        
        ! Verify fields
        if (is_valid_mono_handle(retrieved%mono) .and. &
            retrieved%mono%type_id == original%mono%type_id) then
            call test_pass()
        else
            call test_fail("Poly type fields not preserved")
        end if
        
        call destroy_type_arena(arena)
    end subroutine test_poly_type_storage
    
    subroutine test_args_array_storage()
        type(type_arena_t) :: arena
        type(mono_handle_t), allocatable :: original(:), retrieved(:)
        type(args_handle_t) :: handle
        type(arena_mono_type_t) :: mono_type
        integer :: i
        
        call test_start("Type argument array storage and retrieval")
        
        arena = create_type_arena()
        
        ! Create test mono types
        allocate(original(3))
        do i = 1, 3
            mono_type%kind = i
            mono_type%var_id = i * 10
            write(mono_type%var_name, '(A,I0)') "arg", i
            original(i) = store_mono_type(arena, mono_type)
        end do
        
        ! Store args array
        handle = store_type_args(arena, original)
        
        if (.not. is_valid_args_handle(handle)) then
            call test_fail("Failed to store args array")
            call destroy_type_arena(arena)
            return
        end if
        
        if (handle%count /= 3) then
            call test_fail("Args array count incorrect")
            call destroy_type_arena(arena)
            return
        end if
        
        ! Retrieve args array
        call get_type_args(arena, handle, retrieved)
        
        if (.not. allocated(retrieved)) then
            call test_fail("Failed to retrieve args array")
            call destroy_type_arena(arena)
            return
        end if
        
        if (size(retrieved) /= 3) then
            call test_fail("Retrieved args array size incorrect")
            call destroy_type_arena(arena)
            return
        end if
        
        ! Verify all handles match
        if (all([(retrieved(i)%type_id == original(i)%type_id, i=1,3)])) then
            call test_pass()
        else
            call test_fail("Args array handles not preserved")
        end if
        
        call destroy_type_arena(arena)
    end subroutine test_args_array_storage
    
    subroutine test_handle_validation()
        type(type_arena_t) :: arena
        type(mono_handle_t) :: mono_handle
        type(poly_handle_t) :: poly_handle
        type(args_handle_t) :: args_handle
        type(arena_mono_type_t) :: mono_type
        type(arena_poly_type_t) :: poly_type
        type(mono_handle_t) :: args(1)
        
        call test_start("Handle validation")
        
        arena = create_type_arena()
        
        ! Create valid handles
        mono_handle = store_mono_type(arena, mono_type)
        poly_handle = store_poly_type(arena, poly_type)
        args_handle = store_type_args(arena, args)
        
        ! Test validation
        if (arena%validate_mono(mono_handle) .and. &
            arena%validate_poly(poly_handle) .and. &
            arena%validate_args(args_handle)) then
            call test_pass()
        else
            call test_fail("Valid handles not validated correctly")
        end if
        
        call destroy_type_arena(arena)
    end subroutine test_handle_validation
    
    subroutine test_null_handles()
        type(mono_handle_t) :: null_mono
        type(poly_handle_t) :: null_poly
        type(args_handle_t) :: null_args
        
        call test_start("Null handle creation and validation")
        
        null_mono = null_mono_handle()
        null_poly = null_poly_handle()
        null_args = null_args_handle()
        
        if (.not. is_valid_mono_handle(null_mono) .and. &
            .not. is_valid_poly_handle(null_poly) .and. &
            .not. is_valid_args_handle(null_args)) then
            call test_pass()
        else
            call test_fail("Null handles incorrectly validated as valid")
        end if
    end subroutine test_null_handles
    
    subroutine test_bulk_allocation()
        type(type_arena_t) :: arena
        type(mono_handle_t) :: handles(1000)
        type(arena_mono_type_t) :: mono_type
        type(type_arena_stats_t) :: stats
        integer :: i
        
        call test_start("Bulk allocation performance")
        
        arena = create_type_arena()
        
        ! Bulk allocate 1000 mono types
        mono_type%kind = 2  ! TINT
        do i = 1, 1000
            mono_type%var_id = i
            handles(i) = store_mono_type(arena, mono_type)
        end do
        
        ! Check all handles are valid
        if (all([(is_valid_mono_handle(handles(i)), i=1,1000)])) then
            ! Check statistics
            stats = arena%get_stats()
            if (stats%mono_types == 1000) then
                call test_pass()
            else
                call test_fail("Statistics incorrect after bulk allocation")
            end if
        else
            call test_fail("Some bulk allocations failed")
        end if
        
        call destroy_type_arena(arena)
    end subroutine test_bulk_allocation
    
    subroutine test_arena_reset()
        type(type_arena_t) :: arena
        type(mono_handle_t) :: handle
        type(arena_mono_type_t) :: mono_type
        type(type_arena_stats_t) :: stats
        
        call test_start("Arena reset functionality")
        
        arena = create_type_arena()
        
        ! Allocate some types
        handle = store_mono_type(arena, mono_type)
        stats = arena%get_stats()
        
        if (stats%mono_types /= 1) then
            call test_fail("Initial allocation failed")
            call destroy_type_arena(arena)
            return
        end if
        
        ! Reset arena
        call arena%reset()
        
        ! Check stats are reset
        stats = arena%get_stats()
        if (stats%mono_types == 0 .and. &
            stats%poly_types == 0 .and. &
            stats%arg_arrays == 0) then
            call test_pass()
        else
            call test_fail("Arena reset did not clear statistics")
        end if
        
        call destroy_type_arena(arena)
    end subroutine test_arena_reset
    
    subroutine test_arena_statistics()
        type(type_arena_t) :: arena
        type(type_arena_stats_t) :: stats
        type(arena_mono_type_t) :: mono_type
        type(arena_poly_type_t) :: poly_type
        type(mono_handle_t) :: mono_handle, args(2)
        type(poly_handle_t) :: poly_handle
        type(args_handle_t) :: args_handle
        
        call test_start("Arena statistics tracking")
        
        arena = create_type_arena()
        
        ! Allocate different types
        mono_handle = store_mono_type(arena, mono_type)
        poly_handle = store_poly_type(arena, poly_type)
        args_handle = store_type_args(arena, args)
        
        stats = arena%get_stats()
        
        if (stats%mono_types == 1 .and. &
            stats%poly_types == 1 .and. &
            stats%arg_arrays == 1 .and. &
            stats%total_memory > 0) then
            call test_pass()
        else
            call test_fail("Statistics not tracked correctly")
        end if
        
        call destroy_type_arena(arena)
    end subroutine test_arena_statistics
    
    subroutine test_empty_args_array()
        type(type_arena_t) :: arena
        type(mono_handle_t), allocatable :: empty_args(:), retrieved(:)
        type(args_handle_t) :: handle
        
        call test_start("Empty arguments array handling")
        
        arena = create_type_arena()
        
        ! Create empty args array
        allocate(empty_args(0))
        
        ! This should create a null handle since count is 0
        handle = store_type_args(arena, empty_args)
        
        if (.not. is_valid_args_handle(handle)) then
            ! Empty arrays should result in null handles
            call test_pass()
        else
            call test_fail("Empty args array should create null handle")
        end if
        
        call destroy_type_arena(arena)
    end subroutine test_empty_args_array
    
    subroutine test_zero_size_allocation()
        type(type_arena_t) :: arena
        type(args_handle_t) :: handle
        
        call test_start("Zero size allocation handling")
        
        arena = create_type_arena()
        
        ! Try to allocate zero-size args
        handle = arena%allocate_args(0)
        
        if (.not. is_valid_args_handle(handle)) then
            call test_pass()
        else
            call test_fail("Zero size allocation should fail")
        end if
        
        call destroy_type_arena(arena)
    end subroutine test_zero_size_allocation
    
    subroutine test_invalid_operations()
        type(type_arena_t) :: arena1, arena2
        type(mono_handle_t) :: handle
        type(arena_mono_type_t) :: mono_type
        
        call test_start("Invalid cross-arena operations")
        
        arena1 = create_type_arena()
        arena2 = create_type_arena()
        
        ! Create handle in arena1
        handle = store_mono_type(arena1, mono_type)
        
        ! Try to validate with arena2 (should fail)
        if (.not. arena2%validate_mono(handle)) then
            call test_pass()
        else
            call test_fail("Cross-arena validation should fail")
        end if
        
        call destroy_type_arena(arena1)
        call destroy_type_arena(arena2)
    end subroutine test_invalid_operations
    
    ! Test utilities
    subroutine test_start(name)
        character(len=*), intent(in) :: name
        test_count = test_count + 1
        write(*, '(A,": ")', advance='no') name
    end subroutine test_start
    
    subroutine test_pass()
        write(*, '(A)') "PASS"
        pass_count = pass_count + 1
    end subroutine test_pass
    
    subroutine test_fail(reason)
        character(len=*), intent(in) :: reason
        write(*, '(A,": ",A)') "FAIL", reason
    end subroutine test_fail
    
end program test_type_system_arena