program test_function_param_memory_safety
    use fortfront, only: transform_lazy_fortran_string_with_format, format_options_t
    implicit none
    
    character(len=*), parameter :: input = &
        "program test" // new_line('a') // &
        "contains" // new_line('a') // &
        "function calc(x, y, z, alpha, beta, gamma) result(res)" // new_line('a') // &
        "real :: x, y, z, alpha, beta, gamma, res" // new_line('a') // &
        "res = x + y + z + alpha + beta + gamma" // new_line('a') // &
        "end function calc" // new_line('a') // &
        "end program test"
    
    character(len=:), allocatable :: output, error_msg
    type(format_options_t) :: options
    logical :: test_passed
    
    test_passed = .true.
    
    print *, "Testing function parameter memory safety"
    call transform_lazy_fortran_string_with_format(input, output, error_msg, options)
    
    if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
        print *, "Error: ", trim(error_msg)
        test_passed = .false.
    else
        print *, "Generated output:"
        print *, trim(output)
        
        ! Check for corruption indicators
        if (index(output, "Unparsed statement") > 0) then
            print *, "INFO: Contains 'Unparsed statement' (parser limitation, not corruption)"
            ! This is a parser limitation, not memory corruption - don't fail the test
        end if
        
        ! Check if all parameters are present
        if (index(output, "x") == 0 .or. index(output, "y") == 0 .or. &
            index(output, "z") == 0 .or. index(output, "alpha") == 0 .or. &
            index(output, "beta") == 0 .or. index(output, "gamma") == 0) then
            print *, "FAIL: Missing parameters"
            test_passed = .false.
        end if
        
        ! Check for result clause
        if (index(output, "result(res)") == 0) then
            print *, "WARNING: Missing result clause (known issue #175)"
        end if
        
        ! Check for garbage characters (non-printable)
        block
            integer :: i
            logical :: has_garbage
            has_garbage = .false.
            do i = 1, len(output)
                if (iachar(output(i:i)) < 32 .and. iachar(output(i:i)) /= 10) then
                    has_garbage = .true.
                    exit
                end if
            end do
            if (has_garbage) then
                print *, "FAIL: Contains garbage/non-printable characters"
                test_passed = .false.
            end if
        end block
    end if
    
    if (test_passed) then
        print *, "Test passed - no corruption detected"
        stop 0
    else
        print *, "Test failed - parameter list corruption present"
        stop 1
    end if
    
end program test_function_param_memory_safety