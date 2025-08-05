program test_mono_type_empty_args
    use type_system_hm
    implicit none
    
    type(mono_type_t) :: lhs, rhs
    logical :: test_passed
    
    print *, "Testing mono_type_assign with empty args array..."
    
    ! Initialize rhs with function type
    rhs%kind = TFUN
    rhs%var%id = 1
    rhs%var%name = "fn"
    
    ! Allocate args array with size 0 (empty array)
    allocate(rhs%args(0))
    
    ! This used to cause array bounds error
    test_passed = .true.
    lhs = rhs
    
    ! Verify the assignment worked correctly
    if (lhs%kind /= TFUN) then
        print *, "ERROR: Kind not copied correctly"
        test_passed = .false.
    end if
    
    if (lhs%var%id /= 1) then
        print *, "ERROR: var%id not copied correctly"
        test_passed = .false.
    end if
    
    if (lhs%var%name /= "fn") then
        print *, "ERROR: var%name not copied correctly"
        test_passed = .false.
    end if
    
    if (.not. allocated(lhs%args)) then
        print *, "ERROR: args not allocated"
        test_passed = .false.
    else if (size(lhs%args) /= 0) then
        print *, "ERROR: args size incorrect, expected 0, got", size(lhs%args)
        test_passed = .false.
    end if
    
    if (test_passed) then
        print *, "✓ Empty args array handled correctly"
        print *, "All tests passed!"
    else
        error stop 1
    end if
    
end program test_mono_type_empty_args