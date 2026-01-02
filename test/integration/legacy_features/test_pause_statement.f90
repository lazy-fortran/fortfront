program test_pause_statement
    use, intrinsic :: iso_fortran_env, only: dp => real64, error_unit
    use fortfront, only: transform_lazy_fortran_string
    implicit none
    logical :: all_tests_passed
    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0
    all_tests_passed = .true.

    call test_pause_with_string()
    call test_pause_without_message()
    call test_pause_in_if_block()

    if (all_tests_passed) then
        print *, "All PAUSE statement tests passed"
    else
        print *, "Some PAUSE statement tests failed"
        error stop 1
    end if

contains

    subroutine test_pause_with_string()
        character(len=:), allocatable :: input, output, expected, error_msg
        logical :: test_passed

        test_count = test_count + 1

        call read_example('examples/f90/pause_with_string_in_loop.f90', input)

        call transform_lazy_fortran_string(input, output, error_msg)

        test_passed = index(output, "pause 'Paused at iteration 2'") > 0
        test_passed = test_passed .and. index(output, "if (i == 2) then") > 0

        if (test_passed) then
            pass_count = pass_count + 1
            print *, "PASS: test_pause_with_string"
        else
            all_tests_passed = .false.
            print *, "FAIL: test_pause_with_string"
            print *, "Output:", trim(output)
        end if
    end subroutine test_pause_with_string

    subroutine test_pause_without_message()
        character(len=:), allocatable :: input, output, error_msg
        logical :: test_passed

        test_count = test_count + 1

        call read_example('examples/f90/pause_without_message.f90', input)

        call transform_lazy_fortran_string(input, output, error_msg)

        test_passed = index(output, "pause") > 0
        test_passed = test_passed .and. index(output, "print *, 'Continued'") > 0

        if (test_passed) then
            pass_count = pass_count + 1
            print *, "PASS: test_pause_without_message"
        else
            all_tests_passed = .false.
            print *, "FAIL: test_pause_without_message"
            print *, "Output:", trim(output)
        end if
    end subroutine test_pause_without_message

    subroutine test_pause_in_if_block()
        character(len=:), allocatable :: input, output, error_msg
        logical :: test_passed

        test_count = test_count + 1

        call read_example('examples/f90/pause_in_if_block.f90', input)

        call transform_lazy_fortran_string(input, output, error_msg)

        test_passed = index(output, "pause 'Debug pause'") > 0
        test_passed = test_passed .and. index(output, "if (debug_mode) then") > 0

        if (test_passed) then
            pass_count = pass_count + 1
            print *, "PASS: test_pause_in_if_block"
        else
            all_tests_passed = .false.
            print *, "FAIL: test_pause_in_if_block"
            print *, "Output:", trim(output)
        end if
    end subroutine test_pause_in_if_block

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

end program test_pause_statement
