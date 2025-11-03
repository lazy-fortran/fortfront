program test_module_implicit_none_insertion
    use transformation_api, only: transform_lazy_fortran_string
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, iostat_eor

    logical :: all_passed
    all_passed = .true.

    if (.not. test_module_adds_implicit_none()) all_passed = .false.

    if (all_passed) then
        stop 0
    else
        stop 1
    end if

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

    logical function test_module_adds_implicit_none()
        character(len=:), allocatable :: input, output, error_msg

        call read_example('examples/f90/module_implicit_none_insertion.f90', input)

        call transform_lazy_fortran_string(input, output, error_msg)

        if (len_trim(error_msg) > 0) then
            test_module_adds_implicit_none = .false.
            return
        end if

        if (index(output, 'module m') == 0) then
            test_module_adds_implicit_none = .false.
            return
        end if

        if (index(output, 'implicit none') == 0) then
            test_module_adds_implicit_none = .false.
            return
        end if

        test_module_adds_implicit_none = .true.
    end function test_module_adds_implicit_none

end program test_module_implicit_none_insertion

