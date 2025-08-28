program test_issue_652_multi_var
    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    print *, '=== Issue #652: Multi-variable declaration parsing ==='
    print *
    
    if (.not. test_simple_multi_var()) all_passed = .false.
    if (.not. test_multi_var_with_init()) all_passed = .false.
    if (.not. test_multi_var_with_attributes()) all_passed = .false.
    
    print *
    if (all_passed) then
        print *, 'SUCCESS: All multi-variable declaration tests passed!'
        stop 0
    else
        print *, 'FAILURE: Multi-variable declarations not correctly parsed'
        stop 1
    end if
    
contains

    logical function test_simple_multi_var()
        character(len=*), parameter :: input = &
            'program test' // new_line('a') // &
            '    integer :: x, y, z' // new_line('a') // &
            '    x = 1' // new_line('a') // &
            '    y = 2' // new_line('a') // &
            '    z = 3' // new_line('a') // &
            '    print *, x, y, z' // new_line('a') // &
            'end program'
        
        character(len=:), allocatable :: output, error_msg
        
        test_simple_multi_var = .true.
        print *, 'Test 1: Simple multi-variable declaration (integer :: x, y, z)'
        
        ! Transform
        call transform_lazy_fortran_string(input, output, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error - ', trim(error_msg)
            test_simple_multi_var = .false.
            return
        end if
        
        ! Check all variables are declared
        if (index(output, 'x') > 0 .and. &
            index(output, 'y') > 0 .and. &
            index(output, 'z') > 0) then
            
            ! Check assignments are preserved
            if (index(output, 'x = 1') > 0 .and. &
                index(output, 'y = 2') > 0 .and. &
                index(output, 'z = 3') > 0) then
                print *, '  PASS: All variables and assignments preserved'
            else
                print *, '  FAIL: Variable assignments not preserved'
                print *, '  Output:'
                print *, trim(output)
                test_simple_multi_var = .false.
            end if
        else
            print *, '  FAIL: Not all variables found in output'
            print *, '  Output:'
            print *, trim(output)
            test_simple_multi_var = .false.
        end if
        
    end function test_simple_multi_var

    logical function test_multi_var_with_init()
        character(len=*), parameter :: input = &
            'program test' // new_line('a') // &
            '    real :: a, b, c = 3.14' // new_line('a') // &
            '    print *, a, b, c' // new_line('a') // &
            'end program'
        
        character(len=:), allocatable :: output, error_msg
        
        test_multi_var_with_init = .true.
        print *, 'Test 2: Multi-variable with initialization (real :: a, b, c = 3.14)'
        
        ! Transform
        call transform_lazy_fortran_string(input, output, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error - ', trim(error_msg)
            test_multi_var_with_init = .false.
            return
        end if
        
        ! Check all variables are declared
        if (index(output, 'real') > 0) then
            ! Check that 'c' is initialized
            if (index(output, '3.14') > 0) then
                print *, '  PASS: Multi-variable declaration with initializer preserved'
            else
                print *, '  FAIL: Initializer not preserved'
                print *, '  Output:'
                print *, trim(output)
                test_multi_var_with_init = .false.
            end if
        else
            print *, '  FAIL: Type declaration not found'
            print *, '  Output:'
            print *, trim(output)
            test_multi_var_with_init = .false.
        end if
        
    end function test_multi_var_with_init
    
    logical function test_multi_var_with_attributes()
        character(len=*), parameter :: input = &
            'program test' // new_line('a') // &
            '    integer, allocatable :: arr1, arr2, arr3' // new_line('a') // &
            '    allocate(arr1(10))' // new_line('a') // &
            '    allocate(arr2(20))' // new_line('a') // &
            '    allocate(arr3(30))' // new_line('a') // &
            'end program'
        
        character(len=:), allocatable :: output, error_msg
        
        test_multi_var_with_attributes = .true.
        print *, 'Test 3: Multi-variable with attributes (integer, allocatable :: arr1, arr2, arr3)'
        
        ! Transform
        call transform_lazy_fortran_string(input, output, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Compilation error - ', trim(error_msg)
            test_multi_var_with_attributes = .false.
            return
        end if
        
        ! Check allocatable attribute is preserved
        if (index(output, 'allocatable') > 0) then
            ! Check all arrays are present
            if (index(output, 'arr1') > 0 .and. &
                index(output, 'arr2') > 0 .and. &
                index(output, 'arr3') > 0) then
                print *, '  PASS: Multi-variable with attributes preserved'
            else
                print *, '  FAIL: Not all variables preserved'
                print *, '  Output:'
                print *, trim(output)
                test_multi_var_with_attributes = .false.
            end if
        else
            print *, '  FAIL: allocatable attribute not found'
            print *, '  Output:'
            print *, trim(output)
            test_multi_var_with_attributes = .false.
        end if
        
    end function test_multi_var_with_attributes
    
end program test_issue_652_multi_var