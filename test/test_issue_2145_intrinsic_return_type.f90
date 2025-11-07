program test_issue_2145_intrinsic_return_type
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                             iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: errors
    logical :: has_integer_decl

    print *, "=== Testing Issue #2145: len_trim return type inference ==="

    call read_example('examples/lf/issue_2145_intrinsic_return_type.lf', source)
    call transform_lazy_fortran_string(source, output, errors)

    has_integer_decl = index(output, 'integer :: len_msg') > 0

    if (has_integer_decl .and. len_trim(errors) == 0) then
        print *, "  PASS: len_trim result declared as integer"
    else
        print *, "  FAIL: len_trim declaration incorrect"
        print *, "Output:"
        print *, trim(output)
        if (len_trim(errors) > 0) then
            print *, "Errors:"
            print *, trim(errors)
        end if
        error stop 1
    end if

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

end program test_issue_2145_intrinsic_return_type
