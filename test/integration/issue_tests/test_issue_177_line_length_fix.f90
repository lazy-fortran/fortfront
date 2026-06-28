program test_issue_177_line_length_fix
    ! Test for issue #177: Line length enforcement with continuations
    use, intrinsic :: iso_fortran_env, only: error_unit, input_unit, iostat_end, &
        iostat_eor
    use fortfront, only: transform_lazy_fortran_string_with_format, &
        format_options_t
    implicit none

    character(len=:), allocatable :: output, error_msg
    type(format_options_t) :: options

    print *, "=== Testing Issue #177: Line Length Enforcement ==="

    ! Test case 1: Long assignment should be broken
    call test_line_breaking()

    ! Test case 2: Short line should not be broken
    call test_no_breaking()

    print *, "All line length enforcement tests passed"

contains

    include '../../common/read_example.inc'

    subroutine test_line_breaking()
        character(len=:), allocatable :: long_assignment
        integer :: continuation_count, search_start, rel_pos

        call read_example('examples/lf/issue_0177_line_length_long_assignment.lf', &
            long_assignment)

        options%line_length = 50 ! Force breaking

        call transform_lazy_fortran_string_with_format(long_assignment, &
            output, error_msg, &
            options)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "ERROR: ", trim(error_msg)
            stop 1
        end if

        ! Count continuation characters
        continuation_count = 0
        search_start = 1
        do
            if (search_start > len(output)) exit
            rel_pos = index(output(search_start:), " &")
            if (rel_pos == 0) exit
            continuation_count = continuation_count + 1
            search_start = search_start + rel_pos + 1
            ! Skip past the found continuation marker
        end do

        if (continuation_count == 0) then
            print *, "FAIL: No continuation found in long line"
            print *, "Input length: ", len(long_assignment)
            print *, "Output: ", trim(output)
            stop 1
        end if

        if (continuation_count > 1) then
            print *, "PASS: Long line broken with ", continuation_count, &
                " continuations"
        else
            print *, "PASS: Long line broken with continuation"
        end if
    end subroutine test_line_breaking

    subroutine test_no_breaking()
        character(len=:), allocatable :: short_assignment

        call read_example('examples/lf/issue_0177_line_length_short_assignment.lf', &
            short_assignment)

        options%line_length = 80 ! Should not break

        call transform_lazy_fortran_string_with_format(short_assignment, &
            output, error_msg, &
            options)

        if (allocated(error_msg) .and. len_trim(error_msg) > 0) then
            print *, "ERROR: ", trim(error_msg)
            stop 1
        end if

        ! Verify no continuation was added
        if (index(output, " &") > 0) then
            print *, "FAIL: Unnecessary continuation in short line"
            print *, "Output: ", trim(output)
            stop 1
        end if

        print *, "PASS: Short line preserved without continuation"
    end subroutine test_no_breaking


end program test_issue_177_line_length_fix
