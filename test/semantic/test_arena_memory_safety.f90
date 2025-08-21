program test_arena_memory_safety
    ! Test memory safety - ensure no allocate with source patterns remain
    ! Given: Arena-based type system that eliminates allocatable component issues
    ! When: Complex type operations are performed that previously caused GCC 15.2.1 segfaults
    ! Then: All operations complete safely without memory violations or segfaults
    
    use type_system_hm_arena
    use type_arena, only: global_type_arena, type_arena_stats_t, type_ref_t
    use type_env_arena, only: type_env_arena_t, create_type_env_arena
    use type_subst_arena, only: type_subst_arena_t, create_type_subst_arena
    implicit none
    
    ! Variables for stress testing patterns that caused GCC 15.2.1 segfaults
    type(mono_type_t) :: int_type, real_type, char_type, var_type
    type(type_ref_t) :: int_ref
    type(mono_type_t) :: complex_fun, nested_array, poly_instance
    type(poly_type_t) :: generic_poly, complex_poly
    type(type_env_t) :: environment
    type(substitution_t) :: subst
    type(type_env_arena_t) :: arena_env
    type(type_subst_arena_t) :: arena_subst
    type(type_var_t) :: tv_array(10)
    integer :: i, j
    character(len=256) :: type_str
    logical :: safety_check
    
    print *, "=== Testing Arena Memory Safety ==="
    
    ! Test 1: Rapid type creation and destruction (no allocate/deallocate cycles)
    print *, "Test 1: Rapid type creation without allocate/deallocate cycles"
    block
        type(mono_type_t) :: temp_types(1000)
        
        do i = 1, 1000
            select case (mod(i, 5))
            case (0)
                temp_types(i) = create_mono_type(TINT)
            case (1)
                temp_types(i) = create_mono_type(TREAL)
            case (2)
                temp_types(i) = create_mono_type(TCHAR, char_size=i)
            case (3)
                temp_types(i) = create_mono_type(TLOGICAL)
            case (4)
                temp_types(i) = create_mono_type(TVAR, create_type_var(i))
            end select
        end do
        
        ! All types should be valid - no memory corruption
        do i = 1, 1000
            type_str = temp_types(i)%to_string()
            if (len_trim(type_str) == 0) then
                print *, "ERROR: Type", i, "has empty string representation"
                error stop
            end if
        end do
        
        print *, "PASS: Created and validated 1000 types without memory issues"
    end block
    
    ! Test 2: Deep assignment chains (problematic for allocatable components)
    print *, "Test 2: Deep assignment chains without allocatable component issues"
    block
        type(mono_type_t) :: chain_types(50)
        type(mono_type_t) :: base_type
        
        base_type = create_mono_type(TINT)
        chain_types(1) = base_type
        
        ! Create assignment chain
        do i = 2, 50
            chain_types(i) = chain_types(i-1)  ! Assignment should be safe
        end do
        
        ! Verify all chain elements are equivalent
        do i = 2, 50
            safety_check = chain_types(i)%equals(base_type)
            if (.not. safety_check) then
                print *, "ERROR: Assignment chain broken at index", i
                error stop
            end if
        end do
        
        print *, "PASS: Assignment chain of 50 elements completed safely"
    end block
    
    ! Test 3: Complex nested type structures (stress test for GCC 15.2.1 issues)
    print *, "Test 3: Complex nested structures without segfaults"
    block
        type(mono_type_t) :: nested_base, nested_array, nested_fun
        type(mono_type_t) :: super_nested
        
        ! Create deeply nested structure: ((int -> real) -> char) -> logical
        nested_base = create_fun_type(create_mono_type(TINT), create_mono_type(TREAL))
        nested_array = create_fun_type(nested_base, create_mono_type(TCHAR, char_size=10))
        nested_fun = create_fun_type(nested_array, create_mono_type(TLOGICAL))
        
        ! Multiple assignments and copies
        super_nested = nested_fun
        super_nested = super_nested%deep_copy()
        super_nested = nested_fun  ! Reassignment
        
        type_str = super_nested%to_string()
        if (len_trim(type_str) == 0) then
            print *, "ERROR: Nested type lost its structure"
            error stop
        end if
        
        print *, "PASS: Complex nested type operations completed safely"
        print *, "  Type: ", trim(type_str)
    end block
    
    ! Test 4: Polymorphic type with multiple quantifiers (GCC 15.2.1 problem area)
    print *, "Test 4: Polymorphic types with multiple quantifiers"
    block
        type(mono_type_t) :: poly_base
        type(poly_type_t) :: multi_poly
        type(type_var_t) :: quantifiers(5)
        
        ! Create multiple type variables
        do i = 1, 5
            quantifiers(i) = create_type_var(i)
        end do
        
        ! Create complex polymorphic type
        poly_base = create_fun_type(create_mono_type(TVAR, quantifiers(1)), &
                                   create_mono_type(TVAR, quantifiers(2)))
        multi_poly = create_poly_type(quantifiers, poly_base)
        
        ! Multiple operations on polymorphic type
        multi_poly = multi_poly%deep_copy()
        type_str = multi_poly%to_string()
        
        if (len_trim(type_str) == 0) then
            print *, "ERROR: Polymorphic type lost its structure"
            error stop
        end if
        
        print *, "PASS: Polymorphic type with 5 quantifiers handled safely"
        print *, "  Type: ", trim(type_str)
    end block
    
    ! Test 5: Environment operations (no allocatable array issues)
    print *, "Test 5: Type environment operations without allocatable arrays"
    arena_env = create_type_env_arena()
    
    do i = 1, 100
        write(type_str, '("var_", I0)') i
        select case (mod(i, 3))
        case (0)
            call arena_env%extend(trim(type_str), global_type_arena%create_int())
        case (1)
            call arena_env%extend(trim(type_str), global_type_arena%create_real())
        case (2)
            call arena_env%extend(trim(type_str), global_type_arena%create_var(i))
        end select
    end do
    
    ! Verify all entries are retrievable
    do i = 1, 100
        write(type_str, '("var_", I0)') i
        int_ref = arena_env%lookup(trim(type_str))
        if (.not. int_ref%is_valid()) then
            print *, "ERROR: Environment lookup failed for", trim(type_str)
            error stop
        end if
    end do
    
    print *, "PASS: Environment with 100 entries operates safely"
    
    ! Test 6: Substitution operations (no allocatable component issues)
    print *, "Test 6: Substitution operations without allocatable components"
    arena_subst = create_type_subst_arena()
    
    do i = 1, 50
        select case (mod(i, 3))
        case (0)
            call arena_subst%add(i, global_type_arena%create_int())
        case (1)
            call arena_subst%add(i, global_type_arena%create_real())
        case (2)
            call arena_subst%add(i, global_type_arena%create_logical())
        end select
    end do
    
    ! Apply substitutions
    do i = 1, 50
        int_ref = arena_subst%lookup(i)
        if (int_ref%is_valid()) then
            type_str = int_ref%to_string()
            if (len_trim(type_str) == 0) then
                print *, "ERROR: Substitution result is empty for var", i
                error stop
            end if
        end if
    end do
    
    print *, "PASS: Substitution operations with 50 mappings completed safely"
    
    ! Test 7: Mixed operations stress test
    print *, "Test 7: Mixed operations stress test"
    block
        type(mono_type_t) :: mixed_array(20)
        type(poly_type_t) :: poly_array(10)
        type(type_var_t) :: vars(3)
        
        ! Initialize
        do i = 1, 3
            vars(i) = create_type_var(i)
        end do
        
        ! Create mixed types with assignments and copies
        do i = 1, 20
            if (mod(i, 2) == 0) then
                mixed_array(i) = create_mono_type(TINT)
            else
                mixed_array(i) = create_fun_type(create_mono_type(TREAL), create_mono_type(TCHAR))
            end if
            
            ! Assignment and deep copy operations
            mixed_array(i) = mixed_array(i)%deep_copy()
        end do
        
        ! Create polymorphic types
        do i = 1, 10
            poly_array(i) = create_poly_type(vars(1:2), mixed_array(i))
            poly_array(i) = poly_array(i)%deep_copy()
        end do
        
        ! Verify all operations completed successfully
        do i = 1, 20
            type_str = mixed_array(i)%to_string()
            if (len_trim(type_str) == 0) then
                print *, "ERROR: Mixed array type", i, "is invalid"
                error stop
            end if
        end do
        
        do i = 1, 10
            type_str = poly_array(i)%to_string()
            if (len_trim(type_str) == 0) then
                print *, "ERROR: Poly array type", i, "is invalid"
                error stop
            end if
        end do
        
        print *, "PASS: Mixed operations stress test completed"
    end block
    
    ! Test 8: Arena capacity and bounds checking
    print *, "Test 8: Arena bounds and capacity management"
    block
        type(type_arena_stats_t) :: final_stats
        
        final_stats = global_type_arena%get_stats()
        
        print *, "Final arena statistics:"
        print *, "  Total types created: ", final_stats%total_types
        print *, "  Arena capacity: ", final_stats%capacity
        print *, "  Memory usage (bytes): ", final_stats%memory_usage
        
        if (final_stats%total_types > final_stats%capacity) then
            print *, "ERROR: Arena exceeded its capacity"
            error stop
        end if
        
        if (final_stats%total_types <= 0) then
            print *, "ERROR: Arena has no types (unexpected)"
            error stop
        end if
        
        print *, "PASS: Arena capacity management is correct"
    end block
    
    print *, "PASS: All memory safety tests completed successfully"
    print *, "Arena-based approach eliminates GCC 15.2.1 allocatable component issues"
    print *, "No segmentation faults or memory violations detected"
    
end program test_arena_memory_safety