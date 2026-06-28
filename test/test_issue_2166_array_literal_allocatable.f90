program test_issue_2166_array_literal_allocatable
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit
    use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(:), allocatable :: input_code, output_code, error_msg

    print *, "=== Issue #2166: literal slice destination must be fixed size ==="

    call read_example('examples/lf/issue_2166_array_literal_allocatable.lf', &
        input_code)

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "FAIL: Transformation failed:", error_msg
        error stop 1
    end if

    if (index(output_code, "integer :: dest(3)") == 0) then
        print *, "FAIL: Missing explicit size declaration for dest"
        print *, output_code
        error stop 1
    end if

    if (index(output_code, "allocatable :: dest") > 0) then
        print *, "FAIL: dest incorrectly marked allocatable"
        print *, output_code
        error stop 1
    end if

    print *, "PASS: literal slice destination uses explicit extent"


contains


    include 'common/read_example.inc'
end program test_issue_2166_array_literal_allocatable
