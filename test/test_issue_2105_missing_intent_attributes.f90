program test_issue_2105_missing_intent_attributes
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                             iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: errors
    logical :: test_passed

    print *, "=== Testing Issue #2105: Missing intent attributes ==="

    call read_example('examples/lf/issue_missing_intent_in_function.lf', source)
    call transform_lazy_fortran_string(source, output, errors)

    test_passed = index(output, 'intent(in)') > 0

    if (test_passed) then
        print *, "  PASS: Function parameters have intent(in) attributes"
    else
        print *, "  FAIL: Function parameters missing intent(in) attributes"
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

end program test_issue_2105_missing_intent_attributes
