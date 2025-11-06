program test_issue_2165_variable_undeclared
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                             iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(:), allocatable :: input_code, output_code, error_msg

    print *, "=== Issue #2165: Undeclared variable used as argument ==="

    call read_example('examples/lf/issue_2165_variable_undeclared_argument.lf', &
                      input_code)

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "FAIL: Transformation failed:", trim(error_msg)
        error stop 1
    end if

    if (index(output_code, ":: uninitialized_var") == 0) then
        print *, "FAIL: Missing declaration for uninitialized_var"
        print *, "Output:"
        print *, output_code
        error stop 1
    end if

    print *, "PASS: Undeclared argument variable inferred correctly"

contains

    include 'common/cli_io_reader.inc'

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

end program test_issue_2165_variable_undeclared
