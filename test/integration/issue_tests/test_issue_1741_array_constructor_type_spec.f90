! Test array constructor with type specification (issue #1741)
program test_issue_1741_array_constructor_type_spec
    use fortfront, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, iostat_eor
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: transformed
    character(len=:), allocatable :: error_msg

    call read_example('examples/f90/issue_1741_array_constructor_type_spec.f90', &
                      source)

    call transform_lazy_fortran_string(source, transformed, error_msg)

    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, 'FAIL: unexpected error:', trim(error_msg)
            stop 1
        end if
    end if

    ! The assignment statement must be present
    if (index(transformed, 'real_arr =') == 0) then
        print *, 'FAIL: assignment statement with type-spec array constructor was removed'
        print *, 'Transformed code:'
        print *, transformed
        stop 1
    end if

    ! The array constructor should be preserved (type spec stripped)
    if (index(transformed, '(/') == 0) then
        print *, 'FAIL: array constructor syntax lost'
        print *, 'Transformed code:'
        print *, transformed
        stop 1
    end if

    print *, 'PASS: array constructor with type spec preserved'

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

end program test_issue_1741_array_constructor_type_spec
