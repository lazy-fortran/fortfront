program test_issue_1859_read_statement
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                             iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(:), allocatable :: input_code, output_code, error_msg

    print *, "=== Issue #1859: READ statement variable inference ==="

    call read_example('examples/lf/read_statement.lf', input_code)

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "FAIL: Transformation failed:", error_msg
        error stop 1
    end if

    if (index(output_code, "real :: x") == 0 .and. &
        index(output_code, "real(dp) :: x") == 0 .and. &
        index(output_code, "real(8) :: x") == 0) then
        print *, "FAIL: Variable x not declared in output"
        print *, "Output:"
        print *, output_code
        error stop 1
    end if

    if (index(output_code, "integer :: y") == 0) then
        print *, "FAIL: Variable y not declared in output"
        print *, "Output:"
        print *, output_code
        error stop 1
    end if

    if (index(output_code, "read(*, *) x") == 0) then
        print *, "FAIL: READ statement not found in output"
        print *, "Output:"
        print *, output_code
        error stop 1
    end if

    print *, "PASS: READ statement variable inference working correctly"


contains


    include 'common/read_example.inc'
end program test_issue_1859_read_statement
