program test_type_system_coverage
    use type_system_hm
    implicit none

    logical :: all_passed
    
    all_passed = .true.
    
    print *, '=== Type System Coverage Tests ==='
    print *
    
    ! Test duplicate variable detection in TVAR case
    print *, 'Testing duplicate variable detection in TVAR...'
    if (.not. test_duplicate_var_detection()) all_passed = .false.
    
    ! Test function/array type variable collection
    print *, 'Testing function/array type variable collection...'
    if (.not. test_function_array_var_collection()) all_passed = .false.
    
    ! Report results
    print *
    if (all_passed) then
        print *, 'All type system coverage tests passed!'
        stop 0
    else
        print *, 'Some type system coverage tests failed!'
        stop 1
    end if

contains

    logical function test_duplicate_var_detection()
        type(mono_type_t) :: type_with_duplicates
        type(mono_type_t), allocatable :: args(:)
        type(type_var_t), allocatable :: vars(:)
        type(type_var_t) :: var1, var2
        
        ! Create function type with duplicate variable arguments
        type_with_duplicates%kind = TFUN
        
        ! Create arguments with duplicate type variables (same ID)
        allocate(args(2))
        
        var1%id = 42
        var1%name = "t1"
        args(1)%kind = TVAR
        args(1)%var = var1
        
        var2%id = 42  ! Same ID to test duplicate detection
        var2%name = "t2"  
        args(2)%kind = TVAR
        args(2)%var = var2
        
        type_with_duplicates%args = args
        
        ! Get free variables - should detect duplicates and return only 1
        call free_type_vars(type_with_duplicates, vars)
        
        ! We should have only 1 unique variable despite 2 args with same ID
        if (size(vars) == 1 .and. vars(1)%id == 42) then
            test_duplicate_var_detection = .true.
            print *, '  PASSED: Duplicate variable detection works'
        else
            test_duplicate_var_detection = .false.
            print *, '  FAILED: Expected 1 variable, got', size(vars)
        end if
        
        if (allocated(vars)) deallocate(vars)
        if (allocated(args)) deallocate(args)
    end function test_duplicate_var_detection

    logical function test_function_array_var_collection()
        type(mono_type_t) :: func_type
        type(mono_type_t), allocatable :: args(:)
        type(type_var_t), allocatable :: vars(:)
        type(type_var_t) :: var1, var2
        
        ! Create function type with type variable arguments
        func_type%kind = TFUN
        
        ! Create arguments with type variables
        allocate(args(2))
        
        var1%id = 100
        var1%name = "a"
        args(1)%kind = TVAR
        args(1)%var = var1
        
        var2%id = 200  
        var2%name = "b"
        args(2)%kind = TVAR
        args(2)%var = var2
        
        func_type%args = args
        
        ! Get free variables from function type
        call free_type_vars(func_type, vars)
        
        ! Should collect both type variables from arguments
        if (size(vars) == 2) then
            test_function_array_var_collection = .true.
            print *, '  PASSED: Function/array type variable collection works'
        else
            test_function_array_var_collection = .false.
            print *, '  FAILED: Expected 2 variables, got', size(vars)
        end if
        
        if (allocated(vars)) deallocate(vars)
        if (allocated(args)) deallocate(args)
    end function test_function_array_var_collection

end program test_type_system_coverage