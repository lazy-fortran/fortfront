program test_cmplx_intrinsic_inference
    use fortfront
    implicit none

    character(len=:), allocatable :: output, error_msg
    logical :: success
    integer :: status

    status = 0

    ! Test 1: Basic cmplx() function
    call transform_lazy_fortran_string("z = cmplx(3.0, 4.0)", output, &
                                       error_msg)
    success = len_trim(error_msg) == 0
    if (.not. success) then
        print *, "Test 1 FAILED - Basic cmplx() function"
        print *, "Error: ", error_msg
        status = 1
    else if (index(output, "complex") == 0) then
        print *, "Test 1 FAILED - z not declared as complex"
        print *, "Output: ", trim(output)
        status = 1
    else
        print *, "Test 1 PASSED - Basic cmplx() function"
    end if

    ! Test 2: cmplx() with aimag() usage
    call transform_lazy_fortran_string( &
        "z = cmplx(3.0, 4.0)" // new_line('a') // &
        "im = aimag(z)", output, error_msg)
    success = len_trim(error_msg) == 0
    if (.not. success) then
        print *, "Test 2 FAILED - cmplx() with aimag()"
        print *, "Error: ", error_msg
        status = 1
    else if (index(output, "complex") == 0) then
        print *, "Test 2 FAILED - z not declared as complex"
        print *, "Output: ", trim(output)
        status = 1
    else
        print *, "Test 2 PASSED - cmplx() with aimag()"
    end if

    ! Test 3: cmplx() with real() usage
    call transform_lazy_fortran_string( &
        "z = cmplx(3.0, 4.0)" // new_line('a') // &
        "r = real(z)", output, error_msg)
    success = len_trim(error_msg) == 0
    if (.not. success) then
        print *, "Test 3 FAILED - cmplx() with real()"
        print *, "Error: ", error_msg
        status = 1
    else if (index(output, "complex") == 0) then
        print *, "Test 3 FAILED - z not declared as complex"
        print *, "Output: ", trim(output)
        status = 1
    else
        print *, "Test 3 PASSED - cmplx() with real()"
    end if

    ! Test 4: cmplx() with abs() usage
    call transform_lazy_fortran_string( &
        "z = cmplx(3.0, 4.0)" // new_line('a') // &
        "mag = abs(z)", output, error_msg)
    success = len_trim(error_msg) == 0
    if (.not. success) then
        print *, "Test 4 FAILED - cmplx() with abs()"
        print *, "Error: ", error_msg
        status = 1
    else if (index(output, "complex") == 0) then
        print *, "Test 4 FAILED - z not declared as complex"
        print *, "Output: ", trim(output)
        status = 1
    else
        print *, "Test 4 PASSED - cmplx() with abs()"
    end if

    if (status /= 0) then
        error stop "cmplx() intrinsic inference tests FAILED"
    else
        print *, "All cmplx() intrinsic inference tests PASSED"
    end if

end program test_cmplx_intrinsic_inference
