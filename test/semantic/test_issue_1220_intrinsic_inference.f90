program test_issue_1220_intrinsic_inference
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: test_code
    character(len=:), allocatable :: output_code, error_msg
    logical :: test_failed = .false.

    print *, "Testing issue #1220: Intrinsic function type inference..."

    ! Test case 1: sqrt should infer as real
    test_code = "result = sqrt(16.0)"
    call transform_lazy_fortran_string(test_code, output_code, error_msg)

    if (len(error_msg) == 0) then
        if (index(output_code, "real") > 0 .and. index(output_code, "character") == 0) then
            print *, "✓ sqrt(16.0) correctly inferred as real"
        else
            print *, "✗ sqrt(16.0) incorrectly inferred (should be real, not character)"
            print *, "Generated code:", trim(output_code)
            test_failed = .true.
        end if
    else
        print *, "✗ Failed to transform sqrt expression"
        print *, "Error:", trim(error_msg)
        test_failed = .true.
    end if

    ! Test case 2: sin should infer as real
    test_code = "x = sin(3.14)"
    call transform_lazy_fortran_string(test_code, output_code, error_msg)

    if (len(error_msg) == 0) then
        if (index(output_code, "real") > 0 .and. index(output_code, "character") == 0) then
            print *, "✓ sin(3.14) correctly inferred as real"
        else
            print *, "✗ sin(3.14) incorrectly inferred (should be real, not character)"
            print *, "Generated code:", trim(output_code)
            test_failed = .true.
        end if
    else
        print *, "✗ Failed to transform sin expression"
        print *, "Error:", trim(error_msg)
        test_failed = .true.
    end if

    ! Test case 3: cos should infer as real
    test_code = "y = cos(1.57)"
    call transform_lazy_fortran_string(test_code, output_code, error_msg)

    if (len(error_msg) == 0) then
        if (index(output_code, "real") > 0 .and. index(output_code, "character") == 0) then
            print *, "✓ cos(1.57) correctly inferred as real"
        else
            print *, "✗ cos(1.57) incorrectly inferred (should be real, not character)"
            print *, "Generated code:", trim(output_code)
            test_failed = .true.
        end if
    else
        print *, "✗ Failed to transform cos expression"
        print *, "Error:", trim(error_msg)
        test_failed = .true.
    end if

    ! Test case 4: len should infer as integer
    test_code = 'length = len("hello")'
    call transform_lazy_fortran_string(test_code, output_code, error_msg)

    if (len(error_msg) == 0) then
        if (index(output_code, "integer") > 0 .and. index(output_code, "character") == 0) then
            print *, "✓ len(""hello"") correctly inferred as integer"
        else if (index(output_code, 'character(len=:), allocatable :: length') > 0) then
            ! Special case: variable name "length" might be confused
            print *, "✗ len(""hello"") incorrectly inferred (should be integer, not character)"
            print *, "Generated code:", trim(output_code)
            test_failed = .true.
        end if
    else
        print *, "✗ Failed to transform len expression"
        print *, "Error:", trim(error_msg)
        test_failed = .true.
    end if

    ! Test case 5: exp should infer as real
    test_code = "z = exp(1.0)"
    call transform_lazy_fortran_string(test_code, output_code, error_msg)

    if (len(error_msg) == 0) then
        if (index(output_code, "real") > 0 .and. index(output_code, "character") == 0) then
            print *, "✓ exp(1.0) correctly inferred as real"
        else
            print *, "✗ exp(1.0) incorrectly inferred (should be real, not character)"
            print *, "Generated code:", trim(output_code)
            test_failed = .true.
        end if
    else
        print *, "✗ Failed to transform exp expression"
        print *, "Error:", trim(error_msg)
        test_failed = .true.
    end if

    if (test_failed) then
        print *, "Some tests failed for issue #1220"
        stop 1
    else
        print *, "All tests passed for issue #1220!"
    end if

end program test_issue_1220_intrinsic_inference
