program test_arena_deep_copy_correctness
    ! Test deep copy correctness - verify assignment operations work properly with arena-based approach
    ! Given: Arena-based type system with deep copy operations
    ! When: Complex type structures are copied via assignment and deep_copy methods
    ! Then: All copied structures maintain correct semantics and referential integrity
    
    use type_system_hm_arena
    use type_arena, only: global_type_arena, type_arena_stats_t
    implicit none
    
    type(mono_type_t) :: original_int, original_var, original_fun, original_array
    type(mono_type_t) :: copied_int, copied_var, copied_fun, copied_array
    type(mono_type_t) :: assigned_int, assigned_var, assigned_fun, assigned_array
    type(poly_type_t) :: original_poly, copied_poly, assigned_poly
    type(type_var_t) :: tv1, tv2
    type(type_arena_stats_t) :: stats_before, stats_after
    character(len=256) :: orig_str, copy_str, assign_str
    logical :: equality_test
    integer :: i
    
    print *, "=== Testing Arena Deep Copy Correctness ==="
    
    ! Record arena state before test
    stats_before = global_type_arena%get_stats()
    
    ! Test 1: Basic type deep copy
    original_int = create_mono_type(TINT)
    copied_int = original_int%deep_copy()
    assigned_int = original_int
    
    equality_test = copied_int%equals(original_int)
    if (.not. equality_test) then
        print *, "ERROR: Deep copy of integer type not equal to original"
        error stop
    end if
    
    equality_test = assigned_int%equals(original_int)
    if (.not. equality_test) then
        print *, "ERROR: Assigned integer type not equal to original"
        error stop
    end if
    
    print *, "PASS: Basic integer type copy operations correct"
    
    ! Test 2: Type variable deep copy with different names
    tv1 = create_type_var(1, "'alpha")
    tv2 = create_type_var(2, "'beta")
    
    original_var = create_mono_type(TVAR, tv1)
    copied_var = original_var%deep_copy()
    assigned_var = original_var
    
    orig_str = original_var%to_string()
    copy_str = copied_var%to_string()
    assign_str = assigned_var%to_string()
    
    if (trim(orig_str) /= trim(copy_str)) then
        print *, "ERROR: Type variable copy string mismatch"
        print *, "  Original: ", trim(orig_str)
        print *, "  Copy: ", trim(copy_str)
        error stop
    end if
    
    if (trim(orig_str) /= trim(assign_str)) then
        print *, "ERROR: Type variable assignment string mismatch"
        print *, "  Original: ", trim(orig_str)
        print *, "  Assigned: ", trim(assign_str)
        error stop
    end if
    
    print *, "PASS: Type variable copy operations correct"
    
    ! Test 3: Complex function type deep copy
    original_fun = create_fun_type(original_int, original_var)
    copied_fun = original_fun%deep_copy()
    assigned_fun = original_fun
    
    equality_test = copied_fun%equals(original_fun)
    if (.not. equality_test) then
        orig_str = original_fun%to_string()
        copy_str = copied_fun%to_string()
        print *, "ERROR: Function type copy not equal to original"
        print *, "  Original: ", trim(orig_str)
        print *, "  Copy: ", trim(copy_str)
        error stop
    end if
    
    equality_test = assigned_fun%equals(original_fun)
    if (.not. equality_test) then
        print *, "ERROR: Function type assignment not equal to original"
        error stop
    end if
    
    print *, "PASS: Function type copy operations correct"
    
    ! Test 4: Polymorphic type deep copy
    original_poly = create_poly_type([tv1, tv2], original_fun)
    copied_poly = original_poly%deep_copy()
    assigned_poly = original_poly
    
    orig_str = original_poly%to_string()
    copy_str = copied_poly%to_string()
    assign_str = assigned_poly%to_string()
    
    if (trim(orig_str) /= trim(copy_str)) then
        print *, "ERROR: Polymorphic type copy string mismatch"
        print *, "  Original: ", trim(orig_str)
        print *, "  Copy: ", trim(copy_str)
        error stop
    end if
    
    if (trim(orig_str) /= trim(assign_str)) then
        print *, "ERROR: Polymorphic type assignment string mismatch"
        print *, "  Original: ", trim(orig_str)
        print *, "  Assigned: ", trim(assign_str)
        error stop
    end if
    
    print *, "PASS: Polymorphic type copy operations correct"
    
    ! Test 5: Mass copy stress test for reference integrity
    block
        type(mono_type_t) :: mass_types(100)
        type(mono_type_t) :: mass_copies(100)
        
        ! Create many types
        do i = 1, 100
            if (mod(i, 3) == 0) then
                mass_types(i) = create_mono_type(TINT)
            else if (mod(i, 3) == 1) then
                mass_types(i) = create_mono_type(TREAL)
            else
                mass_types(i) = create_mono_type(TVAR, create_type_var(i))
            end if
        end do
        
        ! Deep copy all types
        do i = 1, 100
            mass_copies(i) = mass_types(i)%deep_copy()
        end do
        
        ! Verify all copies are correct
        do i = 1, 100
            equality_test = mass_copies(i)%equals(mass_types(i))
            if (.not. equality_test) then
                print *, "ERROR: Mass copy test failed at index", i
                error stop
            end if
        end do
        
        print *, "PASS: Mass copy stress test (100 types) completed"
    end block
    
    ! Test 6: Nested function type deep copy
    block
        type(mono_type_t) :: nested_arg, nested_result, nested_fun
        type(mono_type_t) :: outer_fun, copied_outer
        
        nested_arg = create_mono_type(TINT)
        nested_result = create_mono_type(TREAL)
        nested_fun = create_fun_type(nested_arg, nested_result)
        
        outer_fun = create_fun_type(nested_fun, create_mono_type(TLOGICAL))
        copied_outer = outer_fun%deep_copy()
        
        equality_test = copied_outer%equals(outer_fun)
        if (.not. equality_test) then
            orig_str = outer_fun%to_string()
            copy_str = copied_outer%to_string()
            print *, "ERROR: Nested function type copy failed"
            print *, "  Original: ", trim(orig_str)
            print *, "  Copy: ", trim(copy_str)
            error stop
        end if
        
        print *, "PASS: Nested function type deep copy correct"
    end block
    
    ! Test 7: Arena memory efficiency after copying
    stats_after = global_type_arena%get_stats()
    
    print *, "Arena memory analysis:"
    print *, "  Types before test: ", stats_before%total_types
    print *, "  Types after test: ", stats_after%total_types
    print *, "  Types created: ", stats_after%total_types - stats_before%total_types
    print *, "  Memory usage (bytes): ", stats_after%memory_usage
    
    if (stats_after%total_types < stats_before%total_types) then
        print *, "ERROR: Arena lost types during test"
        error stop
    end if
    
    ! Test 8: Self-assignment safety
    block
        type(mono_type_t) :: self_type
        character(len=256) :: before_str, after_str
        
        self_type = create_fun_type(create_mono_type(TINT), create_mono_type(TREAL))
        before_str = self_type%to_string()
        
        ! Self-assignment should be safe
        self_type = self_type
        after_str = self_type%to_string()
        
        if (trim(before_str) /= trim(after_str)) then
            print *, "ERROR: Self-assignment changed type"
            print *, "  Before: ", trim(before_str)
            print *, "  After: ", trim(after_str)
            error stop
        end if
        
        print *, "PASS: Self-assignment safety verified"
    end block
    
    print *, "PASS: All deep copy correctness tests completed successfully"
    print *, "Arena-based approach maintains referential integrity and copy semantics"
    
end program test_arena_deep_copy_correctness