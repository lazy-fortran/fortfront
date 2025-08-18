program test_minimal_type_regression
    use type_system_hm
    implicit none
    
    type(mono_type_t) :: int_type, real_type, fun_type
    type(type_var_t) :: var_a
    logical :: test_passed
    
    print *, "Testing minimal type system regression..."
    
    ! Test basic type creation
    int_type = create_mono_type(TINT)
    real_type = create_mono_type(TREAL)
    
    ! Test function type creation (this was broken before)
    fun_type = create_fun_type(int_type, real_type)
    
    ! Test occurs check (this was broken before)
    var_a = create_type_var(1, "a")
    
    test_passed = .true.
    
    ! Verify function type has args
    if (.not. allocated(fun_type%data%args)) then
        print *, "FAILED: Function type args not allocated"
        test_passed = .false.
    else if (size(fun_type%data%args) /= 2) then
        print *, "FAILED: Function type args wrong size"
        test_passed = .false.
    else
        print *, "PASSED: Function type args properly allocated"
    end if
    
    ! Test occurs check
    block
        type(mono_type_t) :: var_type
        logical :: occurs
        var_type = create_mono_type(TVAR, var=var_a)
        fun_type = create_fun_type(var_type, int_type)  ! Create a -> int
        occurs = occurs_check(var_a, fun_type)
        if (occurs) then
            print *, "PASSED: Occurs check correctly found variable in function type"
        else
            print *, "FAILED: Occurs check should have found variable in function type"
            test_passed = .false.
        end if
    end block
    
    if (test_passed) then
        print *, "All minimal regression tests PASSED"
        stop 0
    else
        print *, "Some tests FAILED"
        stop 1
    end if
end program test_minimal_type_regression