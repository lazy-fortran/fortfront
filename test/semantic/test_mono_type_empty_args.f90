program test_mono_type_empty_args
    use type_system_hm
    implicit none
    
    type(mono_type_t) :: lhs, rhs
    logical :: test_passed
    
    print *, "Testing simplified mono_type assignment..."
    
    ! Initialize rhs with function type
    rhs%data%kind = TFUN
    rhs%data%var%id = 1
    rhs%data%var%name = "fn"
    rhs%data%size = 0
    
    ! Test simplified type system assignment
    test_passed = .true.
    lhs = rhs
    
    ! Verify the assignment worked correctly
    if (lhs%data%kind /= TFUN) then
        print *, "ERROR: Kind not copied correctly"
        test_passed = .false.
    end if
    
    if (lhs%data%var%id /= 1) then
        print *, "ERROR: var%id not copied correctly"
        test_passed = .false.
    end if
    
    if (lhs%data%var%name /= "fn") then
        print *, "ERROR: var%name not copied correctly"
        test_passed = .false.
    end if
    
    if (lhs%data%size /= 0) then
        print *, "ERROR: size not copied correctly"
        test_passed = .false.
    end if
    
    if (test_passed) then
        print *, "✓ Simplified type assignment handled correctly"
        print *, "All tests passed!"
    else
        error stop 1
    end if
    
end program test_mono_type_empty_args