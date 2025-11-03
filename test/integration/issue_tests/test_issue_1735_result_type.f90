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

    subroutine read_example(path, content)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer :: status

        call read_all_stdin_or_file(.true., path, content, status)
        if (status /= 0) then
            write (error_unit, '(A)') 'FAIL: failed to read ' // trim(path)
            error stop 1
        end if
    end subroutine read_example

end program test_issue_1735_result_type
