program test_type_checker_direct
    use type_checker
    use type_system_hm
    implicit none
    
    integer :: test_count, pass_count
    
    test_count = 0
    pass_count = 0
    
    print *, "=== Type Checker Direct Tests ==="
    
    ! Test basic type compatibility
    call test_is_assignable()
    call test_numeric_types()
    call test_compatibility()
    
    ! Test advanced features
    call test_common_type()
    call test_array_conformance()
    call test_argument_types()
    
    print *, ""
    print *, "=== Test Summary ==="
    write(*, '(A,I0,A,I0,A)') "Passed: ", pass_count, "/", test_count, " tests"
    
    if (pass_count == test_count) then
        print *, "All type checker tests passed!"
        stop 0
    else
        print *, "Some type checker tests failed!"
        stop 1
    end if
    
contains
    
    subroutine test_is_assignable()
        type(mono_type_t) :: int_type, real_type, char_type1, char_type2
        
        call test_start("Type assignability")
        
        int_type = create_mono_type(TINT)
        real_type = create_mono_type(TREAL)
        char_type1 = create_mono_type(TCHAR, char_size=10)
        char_type2 = create_mono_type(TCHAR, char_size=20)
        
        ! Same types should be assignable
        if (.not. is_assignable(int_type, int_type)) then
            call test_fail("Same integer types not assignable")
            return
        end if
        
        ! Character types with compatible sizes
        if (.not. is_assignable(char_type1, char_type2)) then
            call test_fail("Compatible character types not assignable")
            return
        end if
        
        ! Different primitive types
        if (is_assignable(int_type, char_type1)) then
            call test_fail("Integer to character should not be assignable")
            return
        end if
        
        call test_pass()
    end subroutine test_is_assignable
    
    subroutine test_numeric_types()
        type(mono_type_t) :: int_type, real_type, char_type
        
        call test_start("Numeric type checking")
        
        int_type = create_mono_type(TINT)
        real_type = create_mono_type(TREAL)
        char_type = create_mono_type(TCHAR)
        
        ! Integer and real should be numeric
        if (.not. is_numeric_type(int_type)) then
            call test_fail("Integer type not recognized as numeric")
            return
        end if
        
        if (.not. is_numeric_type(real_type)) then
            call test_fail("Real type not recognized as numeric")
            return
        end if
        
        ! Character should not be numeric
        if (is_numeric_type(char_type)) then
            call test_fail("Character type incorrectly recognized as numeric")
            return
        end if
        
        call test_pass()
    end subroutine test_numeric_types
    
    subroutine test_compatibility()
        type(mono_type_t) :: int_type, real_type
        integer :: compat_level
        logical :: compatible
        
        call test_start("Type compatibility levels")
        
        int_type = create_mono_type(TINT)
        real_type = create_mono_type(TREAL)
        
        ! Check exact compatibility
        compatible = is_compatible(int_type, int_type, compat_level)
        if (compatible .and. compat_level == COMPAT_EXACT) then
            ! Check compatibility between different numeric types
            compatible = is_compatible(int_type, real_type, compat_level)
            if (compatible .and. compat_level > COMPAT_NONE) then
                call test_pass()
            else
                call test_fail("Numeric types should have some compatibility")
            end if
        else
            call test_fail("Same types should have exact compatibility")
        end if
    end subroutine test_compatibility
    
    subroutine test_common_type()
        type(mono_type_t) :: int_type, real_type, common_type
        
        call test_start("Common type determination")
        
        int_type = create_mono_type(TINT)
        real_type = create_mono_type(TREAL)
        
        ! Get common type between integer and real
        common_type = get_common_type(int_type, real_type)
        
        if (common_type%kind == TREAL) then
            call test_pass()
        else
            call test_fail("Common type should be REAL for INT/REAL")
        end if
    end subroutine test_common_type
    
    subroutine test_array_conformance()
        type(mono_type_t) :: int_array1, int_array2, int_type
        logical :: conformant
        
        call test_start("Array conformance checking")
        
        int_type = create_mono_type(TINT)
        
        ! Create array types with same size (simplified type system)
        int_array1 = create_mono_type(TARRAY)
        int_array1%size = 10
        
        int_array2 = create_mono_type(TARRAY)
        int_array2%size = 10
        
        conformant = check_array_conformance(int_array1, int_array2)
        
        if (conformant) then
            call test_pass()
        else
            call test_fail("Same-size integer arrays should be conformant")
        end if
    end subroutine test_array_conformance
    
    subroutine test_argument_types()
        type(mono_type_t) :: int_type, real_type
        type(mono_type_t), allocatable :: formal_types(:), actual_types(:)
        logical :: valid
        
        call test_start("Argument type checking")
        
        int_type = create_mono_type(TINT)
        real_type = create_mono_type(TREAL)
        
        ! Set up formal and actual parameter types
        allocate(formal_types(2))
        allocate(actual_types(2))
        
        formal_types(1) = int_type
        formal_types(2) = real_type
        actual_types(1) = int_type
        actual_types(2) = real_type
        
        valid = check_argument_types(formal_types, actual_types, .false.)
        
        if (valid) then
            call test_pass()
        else
            call test_fail("Matching argument types should be valid")
        end if
        
        deallocate(formal_types, actual_types)
    end subroutine test_argument_types
    
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
    
end program test_type_checker_direct