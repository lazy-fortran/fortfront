program test_issue_1735_result_type
    use fortfront, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, iostat_eor
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: transformed
    character(len=:), allocatable :: error_msg

    call read_example('examples/f90/issue_1735_result_type.f90', source)

    call transform_lazy_fortran_string(source, transformed, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: unexpected error from transform_lazy_fortran_string'
            print *, trim(error_msg)
            error stop 1
        end if
    end if

    if (index(transformed, 'integer function square(x) result(result)') == 0) then
        print *, 'FAIL: missing integer return type for result clause'
        print *, transformed
        error stop 1
    end if

    if (index(transformed, 'double precision function cube(x) result(res)') == 0) then
        print *, 'FAIL: missing double precision return type for result clause'
        print *, transformed
        error stop 1
    end if

    print *, 'PASS: explicit result types preserved'


contains

    include '../../common/cli_io_reader.inc'

    include '../../common/read_example.inc'
end program test_issue_1735_result_type
