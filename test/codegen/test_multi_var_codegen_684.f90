program test_multi_var_codegen_684
    use frontend, only: transform_lazy_fortran_string
    implicit none
    
    logical :: all_passed
    
    all_passed = .true.
    
    print *, '=== Issue #684: Multi-variable codegen fix ==='
    print *
    
    if (.not. test_basic_multi_var()) all_passed = .false.
    if (.not. test_complex_multi_var()) all_passed = .false.
    
    print *
    if (all_passed) then
        print *, 'SUCCESS: All multi-variable codegen tests passed!'
        stop 0
    else
        print *, 'FAILURE: Multi-variable codegen failed'
        stop 1
    end if
    
contains

    logical function test_basic_multi_var()
        character(len=*), parameter :: input = &
            'program test' // new_line('a') // &
            '    integer :: x, y, z' // new_line('a') // &
            '    x = 1' // new_line('a') // &
            '    y = 2' // new_line('a') // &
            '    z = 3' // new_line('a') // &
            'end program'
        
        character(len=:), allocatable :: output, error_msg
        
        test_basic_multi_var = .true.
        print *, 'Test 1: Basic multi-variable declaration'
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Error - ', trim(error_msg)
            test_basic_multi_var = .false.
            return
        end if
        
        ! Critical check: ensure declaration includes all variables
        if (index(output, 'integer :: x, y, z') > 0 .or. &
            index(output, 'integer :: x,y,z') > 0) then
            print *, '  PASS: Multi-variable declaration preserved'
        else
            print *, '  FAIL: Multi-variable declaration broken'
            print *, '  Expected: integer :: x, y, z'
            print *, '  Got output:'
            print *, trim(output)
            test_basic_multi_var = .false.
        end if
        
    end function test_basic_multi_var
    
    logical function test_complex_multi_var()
        character(len=*), parameter :: input = &
            'program test' // new_line('a') // &
            '    real, allocatable :: arr1(:), arr2(:,:), arr3(:,:,:)' // new_line('a') // &
            'end program'
        
        character(len=:), allocatable :: output, error_msg
        
        test_complex_multi_var = .true.
        print *, 'Test 2: Multi-variable with array specs'
        
        call transform_lazy_fortran_string(input, output, error_msg)
        
        if (len_trim(error_msg) > 0) then
            print *, '  FAIL: Error - ', trim(error_msg)
            test_complex_multi_var = .false.
            return
        end if
        
        ! Check all variables are in the declaration
        if ((index(output, 'arr1') > 0) .and. &
            (index(output, 'arr2') > 0) .and. &
            (index(output, 'arr3') > 0)) then
            
            ! Also check for allocatable attribute
            if (index(output, 'allocatable') > 0) then
                print *, '  PASS: All array variables preserved'
            else
                print *, '  FAIL: Allocatable attribute missing'
                print *, '  Output:'
                print *, trim(output)
                test_complex_multi_var = .false.
            end if
        else
            print *, '  FAIL: Not all variables in declaration'
            print *, '  Output:'
            print *, trim(output)
            test_complex_multi_var = .false.
        end if
        
    end function test_complex_multi_var
    
end program test_multi_var_codegen_684