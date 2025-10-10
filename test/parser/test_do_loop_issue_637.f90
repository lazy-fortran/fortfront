program test_do_loop_issue_637
    ! Test that parser handles do loops with expressions (Issue #637)
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    print *, "=== Testing Do Loop Expression Parsing (Issue #637) ==="

    all_passed = .true.
    all_passed = all_passed .and. test_simple_literals()
    all_passed = all_passed .and. test_with_variables()
    all_passed = all_passed .and. test_with_expressions()
    all_passed = all_passed .and. test_with_function_calls()
    all_passed = all_passed .and. test_complex_expressions()

    if (all_passed) then
        print *, "ALL TESTS PASSED - Issue #637 FIXED!"
        stop 0
    else
        print *, "TESTS FAILED - Issue #637 not fully resolved"
        stop 1
    end if

contains

    logical function test_simple_literals()
        character(len=:), allocatable :: source, output, error_msg

        test_simple_literals = .true.
        print *, "Test 1: Simple literals (do i = 1, 10)..."

        source = "program test" // new_line('a') // &
                 "  integer :: i" // new_line('a') // &
                 "  do i = 1, 10" // new_line('a') // &
                 "    print *, i" // new_line('a') // &
                 "  end do" // new_line('a') // &
                 "end program test"

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: ', trim(error_msg)
                test_simple_literals = .false.
                return
            end if
        end if

        print *, '  PASS: Simple literals work'
    end function

    logical function test_with_variables()
        character(len=:), allocatable :: source, output, error_msg

        test_with_variables = .true.
        print *, "Test 2: With variables (do i = 1, n)..."

        source = "program test" // new_line('a') // &
                 "  integer :: i, n" // new_line('a') // &
                 "  n = 10" // new_line('a') // &
                 "  do i = 1, n" // new_line('a') // &
                 "    print *, i" // new_line('a') // &
                 "  end do" // new_line('a') // &
                 "end program test"

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: ', trim(error_msg)
                test_with_variables = .false.
                return
            end if
        end if

        print *, '  PASS: Variables work'
    end function

    logical function test_with_expressions()
        character(len=:), allocatable :: source, output, error_msg

        test_with_expressions = .true.
        print *, "Test 3: With expressions (do i = n-5, n+5) - CRITICAL TEST..."

        source = "program test" // new_line('a') // &
                 "  integer :: i, n" // new_line('a') // &
                 "  n = 10" // new_line('a') // &
                 "  do i = n-5, n+5" // new_line('a') // &
                 "    print *, i" // new_line('a') // &
                 "  end do" // new_line('a') // &
                 "end program test"

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: ', trim(error_msg)
                test_with_expressions = .false.
                return
            end if
        end if

        print *, '  PASS: Expressions work - ISSUE #637 FIXED!'
    end function

    logical function test_with_function_calls()
        character(len=:), allocatable :: source, output, error_msg

        test_with_function_calls = .true.
        print *, "Test 4: With function calls (do i = 1, size(array))..."

        source = "program test" // new_line('a') // &
                 "  integer :: i" // new_line('a') // &
                 "  integer :: array(10)" // new_line('a') // &
                 "  do i = 1, size(array)" // new_line('a') // &
                 "    print *, i" // new_line('a') // &
                 "  end do" // new_line('a') // &
                 "end program test"

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: ', trim(error_msg)
                test_with_function_calls = .false.
                return
            end if
        end if

        print *, '  PASS: Function calls work'
    end function

    logical function test_complex_expressions()
        character(len=:), allocatable :: source, output, error_msg

        test_complex_expressions = .true.
        print *, "Test 5: Complex expressions (do i = n/2-5, n/2+5, step*2)..."

        source = "program test" // new_line('a') // &
                 "  integer :: i, n, step" // new_line('a') // &
                 "  n = 20" // new_line('a') // &
                 "  step = 1" // new_line('a') // &
                 "  do i = n/2-5, n/2+5, step*2" // new_line('a') // &
                 "    print *, i" // new_line('a') // &
                 "  end do" // new_line('a') // &
                 "end program test"

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  FAIL: ', trim(error_msg)
                test_complex_expressions = .false.
                return
            end if
        end if

        print *, '  PASS: Complex expressions work'
    end function

end program test_do_loop_issue_637
