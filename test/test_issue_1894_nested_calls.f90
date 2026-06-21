program test_issue_1894_nested_calls
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                                                              iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(:), allocatable :: input_code
    character(:), allocatable :: output_code
    character(:), allocatable :: error_msg
    integer :: pos_double
    integer :: pos_integer_after
    integer :: pos_real_after

    print *, "=== Issue #1894: nested call parameter inference ==="

    call read_example('examples/lf/issue_1894_nested_calls.lf', input_code)

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "FAIL: transformation returned error:", trim(error_msg)
        error stop 1
    end if

    pos_double = index(output_code, "integer function double(x)")
    if (pos_double <= 0) then
        print *, "FAIL: missing integer function header for double"
        print *, trim(output_code)
        error stop 1
    end if

    pos_integer_after = index(output_code(pos_double:), "integer, intent(in) :: x")
    if (pos_integer_after <= 0) then
        print *, "FAIL: nested inference did not mark double parameter integer"
        print *, trim(output_code)
        error stop 1
    end if

    pos_real_after = index(output_code(pos_double:), "real :: x")
    if (pos_real_after > 0) then
        print *, "FAIL: real parameter persists for double"
        print *, trim(output_code)
        error stop 1
    end if

    print *, "PASS: nested call inference keeps double parameter integer"

contains

    include 'common/read_example.inc'
end program test_issue_1894_nested_calls
