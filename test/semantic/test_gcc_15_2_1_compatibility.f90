program test_gcc_15_2_1_compatibility
    ! Test suite for GCC 15.2.1 compatibility (Issue #354)
    ! Verifies that type system works correctly with fixed-size arrays
    ! instead of allocatable components that cause crashes
    
    use type_system_unified
    use iso_fortran_env, only: int64
    implicit none
    
    logical :: all_tests_passed = .true.
    
    print *, "=== Testing GCC 15.2.1 Compatibility (Issue #354) ==="
    print *, ""
    
    ! Test 1: Basic substitution creation and assignment
    call test_substitution_basic()
    
    ! Test 2: Type environment creation and assignment
    call test_type_env_basic()
    
    ! Test 3: Poly type creation and assignment
    call test_poly_type_basic()
    
    ! Test 4: Complex nested type operations
    call test_nested_type_operations()
    
    ! Test 5: Large substitution handling
    call test_large_substitution()
    
    ! Test 6: Type environment with many entries
    call test_large_type_env()
    
    ! Test 7: Assignment operator safety
    call test_assignment_safety()
    
    ! Test 8: Deep copy verification
    call test_deep_copy_semantics()
    
    if (all_tests_passed) then
        print *, ""
        print *, "SUCCESS: All GCC 15.2.1 compatibility tests passed!"
    else
        print *, ""
        print *, "FAILURE: Some GCC 15.2.1 compatibility tests failed"
        error stop 1
    end if
    
contains
    
    subroutine test_substitution_basic()
        type(substitution_t) :: sub1, sub2
        type(type_var_t) :: var1, var2
        type(mono_type_t) :: type1, type2
        
        print *, "Test 1: Basic substitution creation and assignment..."
        
        ! Create type variables
        var1 = create_type_var(1, "T1")
        var2 = create_type_var(2, "T2")
        
        ! Create mono types
        type1 = create_mono_type(TINT)
        type2 = create_mono_type(TREAL)
        
        ! Add to substitution
        call sub1%add(var1, type1)
        call sub1%add(var2, type2)
        
        ! Test assignment (this would crash with allocatable components)
        sub2 = sub1
        
        ! Verify copy
        if (sub2%count /= 2) then
            print *, "  FAIL: Substitution count mismatch"
            all_tests_passed = .false.
            return
        end if
        
        print *, "  PASS: Basic substitution operations work"
    end subroutine test_substitution_basic
    
    subroutine test_type_env_basic()
        type(type_env_t) :: env1, env2
        type(poly_type_t) :: scheme
        type(mono_type_t) :: mono
        
        print *, "Test 2: Type environment creation and assignment..."
        
        ! Create a simple type scheme
        mono = create_mono_type(TINT)
        scheme = create_poly_type([type_var_t::], mono)
        
        ! Add to environment
        call env1%extend("x", scheme)
        call env1%extend("y", scheme)
        
        ! Test assignment (this would crash with allocatable components)
        env2 = env1
        
        ! Verify copy
        if (env2%count /= 2) then
            print *, "  FAIL: Environment count mismatch"
            all_tests_passed = .false.
            return
        end if
        
        print *, "  PASS: Type environment operations work"
    end subroutine test_type_env_basic
    
    subroutine test_poly_type_basic()
        type(poly_type_t) :: poly1, poly2
        type(mono_type_t) :: mono
        type(type_var_t) :: var
        
        print *, "Test 3: Poly type creation and assignment..."
        
        ! Create polymorphic type
        var = create_type_var(1, "a")
        mono = create_mono_type(TVAR, var=var)
        poly1 = create_poly_type([var], mono)
        
        ! Test assignment (this would crash with uninitialized allocatable)
        poly2 = poly1
        
        ! Verify handle copy
        if (.not. associated(poly2%arena, poly1%arena)) then
            print *, "  FAIL: Arena pointer not copied"
            all_tests_passed = .false.
            return
        end if
        
        print *, "  PASS: Poly type operations work"
    end subroutine test_poly_type_basic
    
    subroutine test_nested_type_operations()
        type(mono_type_t) :: fun_type, arg_type, ret_type
        
        print *, "Test 4: Complex nested type operations..."
        
        ! Create function type: int -> real
        arg_type = create_mono_type(TINT)
        ret_type = create_mono_type(TREAL)
        fun_type = create_fun_type(arg_type, ret_type)
        
        ! Test that complex types don't crash
        if (fun_type%kind /= TFUN) then
            print *, "  FAIL: Function type not created"
            all_tests_passed = .false.
            return
        end if
        
        print *, "  PASS: Nested type operations work"
    end subroutine test_nested_type_operations
    
    subroutine test_large_substitution()
        type(substitution_t) :: sub
        type(type_var_t) :: var
        type(mono_type_t) :: mono
        integer :: i
        character(len=10) :: var_name
        
        print *, "Test 5: Large substitution handling..."
        
        ! Add many substitutions (test capacity)
        do i = 1, 50
            write(var_name, '("T", I0)') i
            var = create_type_var(i, trim(var_name))
            mono = create_mono_type(TINT)
            call sub%add(var, mono)
        end do
        
        if (sub%count /= 50) then
            print *, "  FAIL: Large substitution count incorrect"
            all_tests_passed = .false.
            return
        end if
        
        print *, "  PASS: Large substitution handling works"
    end subroutine test_large_substitution
    
    subroutine test_large_type_env()
        type(type_env_t) :: env
        type(poly_type_t) :: scheme
        type(mono_type_t) :: mono
        integer :: i
        character(len=10) :: var_name
        
        print *, "Test 6: Type environment with many entries..."
        
        mono = create_mono_type(TINT)
        scheme = create_poly_type([type_var_t::], mono)
        
        ! Add many environment entries
        do i = 1, 100
            write(var_name, '("var", I0)') i
            call env%extend(trim(var_name), scheme)
        end do
        
        if (env%count /= 100) then
            print *, "  FAIL: Large environment count incorrect"
            all_tests_passed = .false.
            return
        end if
        
        print *, "  PASS: Large type environment works"
    end subroutine test_large_type_env
    
    subroutine test_assignment_safety()
        type(substitution_t) :: sub1, sub2, sub3
        type(type_var_t) :: var
        type(mono_type_t) :: mono
        
        print *, "Test 7: Assignment operator safety..."
        
        ! Create first substitution
        var = create_type_var(1, "T")
        mono = create_mono_type(TREAL)
        call sub1%add(var, mono)
        
        ! Chain assignments (would crash with bad allocatable handling)
        sub2 = sub1
        sub3 = sub2
        
        ! Verify all copies are valid
        if (sub3%count /= 1) then
            print *, "  FAIL: Chained assignment failed"
            all_tests_passed = .false.
            return
        end if
        
        print *, "  PASS: Assignment chain works safely"
    end subroutine test_assignment_safety
    
    subroutine test_deep_copy_semantics()
        type(substitution_t) :: sub1, sub2
        type(type_var_t) :: var1, var2
        type(mono_type_t) :: type1, type2
        
        print *, "Test 8: Deep copy verification..."
        
        ! Create original
        var1 = create_type_var(1, "T1")
        type1 = create_mono_type(TINT)
        call sub1%add(var1, type1)
        
        ! Make copy
        sub2 = sub1
        
        ! Modify copy
        var2 = create_type_var(2, "T2")
        type2 = create_mono_type(TREAL)
        call sub2%add(var2, type2)
        
        ! Verify original unchanged
        if (sub1%count /= 1) then
            print *, "  FAIL: Original modified by copy changes"
            all_tests_passed = .false.
            return
        end if
        
        if (sub2%count /= 2) then
            print *, "  FAIL: Copy not properly modified"
            all_tests_passed = .false.
            return
        end if
        
        print *, "  PASS: Deep copy semantics preserved"
    end subroutine test_deep_copy_semantics
    
end program test_gcc_15_2_1_compatibility