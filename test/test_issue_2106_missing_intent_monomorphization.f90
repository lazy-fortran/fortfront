program test_issue_2106_missing_intent_monomorphization
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
                                             iostat_eor
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: errors

    print *, "=== Testing Issue #2106: Monomorphized subroutine intents ==="

    call read_example('examples/lf/issue_2106_missing_intent_monomorphization.lf', &
                      source)
    call transform_lazy_fortran_string(source, output, errors)

    if (len_trim(errors) > 0) then
        write (error_unit, '(A)') 'FAIL: transformation reported errors'
        write (error_unit, '(A)') trim(errors)
        error stop 1
    end if

    call assert_contains(output, 'subroutine print_value__i32', &
                         'missing integer print_value specialization')
    call assert_contains(output, 'integer, intent(in) :: x', &
                         'missing intent for integer print_value specialization')

    call assert_contains(output, 'subroutine print_value__r64', &
                         'missing real print_value specialization')
    call assert_contains(output, 'double precision, intent(in) :: x', &
                         'missing intent for real print_value specialization')

    call assert_contains(output, 'subroutine print_value__ch', &
                         'missing character print_value specialization')
    call assert_contains(output, 'character(len=4), intent(in) :: x', &
                         'missing intent for character print_value specialization')

contains

    include 'common/cli_io_reader.inc'

    subroutine assert_contains(text, pattern, message)
        character(len=*), intent(in) :: text
        character(len=*), intent(in) :: pattern
        character(len=*), intent(in) :: message

        if (index(text, pattern) <= 0) then
            write (error_unit, '(A)') trim(message)
            error stop 1
        end if
    end subroutine assert_contains

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

end program test_issue_2106_missing_intent_monomorphization
