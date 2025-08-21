program test_type_system_bridge
    ! Test the bridge module for converting between traditional and arena types
    
    use type_system_hm
    use type_system_arena
    use type_system_bridge
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== Type System Bridge Tests ==="
    
    ! Test conversions
    call test_mono_type_conversion()
    call test_poly_type_conversion()
    call test_function_type_conversion()
    
    ! Test enhanced context
    call test_type_system_context()
    call test_context_safe_operations()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All type system bridge tests passed!"
        stop 0
    else
        print *, "Some bridge tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_mono_type_conversion()
        type(type_arena_t) :: arena
        type(mono_type_t) :: original, converted_back
        type(mono_handle_t) :: handle
        
        call test_start("Monomorphic type conversion")
        
        arena = create_type_arena()
        
        ! Create original mono type
        original = create_mono_type(TINT)
        original%var%id = 42
        original%var%name = "test_var"
        original%size = 8
        original%alloc_info%is_allocatable = .true.
        
        ! Convert to arena
        handle = convert_mono_to_arena(arena, original)
        
        if (.not. is_valid_mono_handle(handle)) then
            call test_fail("Conversion to arena failed")
            call destroy_type_arena(arena)
            return
        end if
        
        ! Convert back
        converted_back = convert_arena_to_mono(arena, handle)
        
        ! Verify round-trip conversion
        if (converted_back%kind == original%kind .and. &
            converted_back%var%id == original%var%id .and. &
            converted_back%var%name == original%var%name .and. &
            converted_back%size == original%size .and. &
            converted_back%alloc_info%is_allocatable .eqv. original%alloc_info%is_allocatable) then
            call test_pass()
        else
            call test_fail("Round-trip conversion failed")
        end if
        
        call destroy_type_arena(arena)
    end subroutine test_mono_type_conversion
    
    subroutine test_poly_type_conversion()
        type(type_arena_t) :: arena
        type(poly_type_t) :: original, converted_back
        type(poly_handle_t) :: handle
        type(mono_type_t) :: mono
        
        call test_start("Polymorphic type conversion")
        
        arena = create_type_arena()
        
        ! Create original poly type
        mono = create_mono_type(TVAR, create_type_var(1, "'a"))
        original = create_poly_type([create_type_var(1, "'a")], mono)
        
        ! Convert to arena
        handle = convert_poly_to_arena(arena, original)
        
        if (.not. is_valid_poly_handle(handle)) then
            call test_fail("Poly conversion to arena failed")
            call destroy_type_arena(arena)
            return
        end if
        
        ! Convert back
        converted_back = convert_arena_to_poly(arena, handle)
        
        ! Verify basic structure
        if (converted_back%mono%kind == original%mono%kind .and. &
            allocated(converted_back%forall) .and. &
            size(converted_back%forall) == size(original%forall)) then
            call test_pass()
        else
            call test_fail("Poly round-trip conversion failed")
        end if
        
        call destroy_type_arena(arena)
    end subroutine test_poly_type_conversion
    
    subroutine test_function_type_conversion()
        type(type_arena_t) :: arena
        type(mono_type_t) :: original, converted_back
        type(mono_type_t) :: int_type, real_type
        type(mono_handle_t) :: handle
        
        call test_start("Function type conversion")
        
        arena = create_type_arena()
        
        ! Create function type: int -> real
        int_type = create_mono_type(TINT)
        real_type = create_mono_type(TREAL)
        original = create_fun_type(int_type, real_type)
        
        ! Convert to arena
        handle = convert_mono_to_arena(arena, original)
        
        if (.not. is_valid_mono_handle(handle)) then
            call test_fail("Function conversion to arena failed")
            call destroy_type_arena(arena)
            return
        end if
        
        ! Convert back
        converted_back = convert_arena_to_mono(arena, handle)
        
        ! Verify function type structure
        if (converted_back%kind == TFUN .and. &
            allocated(converted_back%args) .and. &
            size(converted_back%args) == 2 .and. &
            converted_back%args(1)%kind == TINT .and. &
            converted_back%args(2)%kind == TREAL) then
            call test_pass()
        else
            call test_fail("Function type round-trip failed")
        end if
        
        call destroy_type_arena(arena)
    end subroutine test_function_type_conversion
    
    subroutine test_type_system_context()
        type(type_system_context_t) :: context
        type(mono_handle_t) :: int_handle, real_handle
        character(len=:), allocatable :: stats
        
        call test_start("Type system context creation and usage")
        
        context = create_type_system_with_arena()
        
        ! Create types using context
        int_handle = context%create_mono_safe(TINT, var_id=1, var_name="integer")
        real_handle = context%create_mono_safe(TREAL, var_id=2, var_name="real")
        
        if (is_valid_mono_handle(int_handle) .and. &
            is_valid_mono_handle(real_handle) .and. &
            context%conversion_count == 2) then
            call test_pass()
        else
            call test_fail("Context type creation failed")
        end if
        
        ! Test statistics
        stats = context%get_stats()
        if (len(stats) > 0) then
            print *, "    Stats: ", stats
        end if
        
        call context%cleanup()
    end subroutine test_type_system_context
    
    subroutine test_context_safe_operations()
        type(type_system_context_t) :: context
        type(mono_handle_t) :: var_handle, poly_mono_handle
        type(mono_handle_t) :: forall_vars(1)
        type(poly_handle_t) :: poly_handle
        
        call test_start("Context safe operations")
        
        context = create_type_system_with_arena()
        
        ! Create type variable
        var_handle = context%create_mono_safe(TVAR, var_id=1, var_name="'a")
        
        ! Create polymorphic identity type: forall 'a. 'a -> 'a
        poly_mono_handle = context%create_mono_safe(TVAR, var_id=1, var_name="'a")
        forall_vars(1) = var_handle
        poly_handle = context%create_poly_safe(poly_mono_handle, forall_vars)
        
        if (is_valid_mono_handle(var_handle) .and. &
            is_valid_poly_handle(poly_handle) .and. &
            context%conversion_count == 3) then
            call test_pass()
        else
            call test_fail("Safe operations failed")
        end if
        
        call context%cleanup()
    end subroutine test_context_safe_operations
    
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
    
end program test_type_system_bridge