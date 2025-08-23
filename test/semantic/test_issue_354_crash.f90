program test_issue_354_crash
    ! Test that specifically triggers GCC 15.2.1 allocatable bugs
    ! This test creates uninitialized derived types with allocatable components
    ! and tests assignment operations that crash with the bug
    
    use type_system_unified
    implicit none
    
    print *, "=== Testing Issue #354: GCC 15.2.1 Allocatable Crash ==="
    print *, ""
    
    call test_uninitialized_substitution()
    call test_uninitialized_type_env()
    call test_array_of_poly_types()
    
    print *, ""
    print *, "SUCCESS: No crashes detected with current implementation"
    
contains
    
    subroutine test_uninitialized_substitution()
        type(substitution_t) :: subs(10)  ! Array of uninitialized types
        type(substitution_t) :: target
        type(type_var_t) :: var
        type(mono_type_t) :: mono
        integer :: i
        
        print *, "Test 1: Uninitialized substitution array assignment..."
        
        ! Create a valid substitution
        var = create_type_var(1, "T")
        mono = create_mono_type(TINT)
        call target%add(var, mono)
        
        ! This loop would crash with GCC 15.2.1 bug
        ! The uninitialized subs(i) has undefined allocatable components
        do i = 1, 10
            subs(i) = target  ! Assignment checks allocated() on undefined memory
        end do
        
        print *, "  PASS: No crash on uninitialized assignment"
    end subroutine test_uninitialized_substitution
    
    subroutine test_uninitialized_type_env()
        type(type_env_t) :: envs(5)  ! Array of uninitialized environments
        type(type_env_t) :: source_env
        type(poly_type_t) :: scheme
        type(mono_type_t) :: mono
        integer :: i
        
        print *, "Test 2: Uninitialized type environment array..."
        
        ! Create valid environment
        mono = create_mono_type(TREAL)
        scheme = create_poly_type([type_var_t::], mono)
        call source_env%extend("x", scheme)
        
        ! This would crash with undefined allocatable components
        do i = 1, 5
            envs(i) = source_env
        end do
        
        print *, "  PASS: No crash on environment assignment"
    end subroutine test_uninitialized_type_env
    
    subroutine test_array_of_poly_types()
        type(poly_type_t) :: poly_array(100)  ! Large array
        type(poly_type_t) :: source
        type(mono_type_t) :: mono
        type(type_var_t) :: var
        integer :: i
        
        print *, "Test 3: Large array of poly types..."
        
        ! Create source poly type
        var = create_type_var(1, "a")
        mono = create_mono_type(TVAR, var=var)
        source = create_poly_type([var], mono)
        
        ! Mass assignment that stresses allocatable handling
        do i = 1, 100
            poly_array(i) = source
        end do
        
        ! Verify all copies
        do i = 1, 100
            if (.not. associated(poly_array(i)%arena)) then
                print *, "  FAIL: Arena not properly copied at index", i
                error stop 1
            end if
        end do
        
        print *, "  PASS: Large array operations successful"
    end subroutine test_array_of_poly_types
    
end program test_issue_354_crash