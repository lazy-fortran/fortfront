program test_chained_assignment_parsing
    ! Test that chained assignment is not misparsed as logical expression
    ! Regression test for issue #2159
    use fortfront, only: transform_lazy_fortran_string
    implicit none

    logical :: all_passed

    print *, "=== Chained Assignment Parsing Tests ==="

    all_passed = test_chained_assignment_not_misparsed()

    if (all_passed) then
        print *, "All chained assignment tests passed!"
        stop 0
    else
        print *, "Some chained assignment tests failed!"
        stop 1
    end if

contains

    logical function test_chained_assignment_not_misparsed()
        character(len=:), allocatable :: source, output, error_msg

        test_chained_assignment_not_misparsed = .true.
        print *, "Testing chained assignment is not misparsed as logical expression..."

        ! This is the exact case from issue #2159
        source = "a = b = c = 5" // new_line('a') // &
            'print *, "a:", a' // new_line('a') // &
            'print *, "b:", b' // new_line('a') // &
            'print *, "c:", c'

        call transform_lazy_fortran_string(source, output, error_msg)

        ! The main goal is to verify the catastrophic misparse is fixed:
        ! 1. Variables should NOT be typed as logical
        ! 2. There should NOT be .and. operators in the output
        ! 3. The assignment should NOT be converted to comparison

        if (index(output, 'logical :: a') > 0) then
            print *, '  FAIL: Variable a incorrectly typed as logical'
            print *, '  Output:', trim(output)
            test_chained_assignment_not_misparsed = .false.
            return
        end if

        if (index(output, '.and.') > 0) then
            print *, '  FAIL: Found .and. operator (catastrophic misparse)'
            print *, '  Output:', trim(output)
            test_chained_assignment_not_misparsed = .false.
            return
        end if

        if (index(output, 'a = (b = c)') > 0 .or. index(output, '(b = c) .and.') > 0) then
            print *, '  FAIL: Assignment misparsed as comparison expression'
            print *, '  Output:', trim(output)
            test_chained_assignment_not_misparsed = .false.
            return
        end if

        ! Note: With the fix, this may produce an error or partial parse
        ! The key is that it should NOT produce the catastrophically wrong output
        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, '  INFO: Parser rejected chained assignment (expected):', trim(error_msg)
                ! This is acceptable - we've fixed the misparse
                test_chained_assignment_not_misparsed = .true.
                return
            end if
        end if

        print *, '  PASS: Chained assignment not misparsed as logical expression'

    end function test_chained_assignment_not_misparsed

end program test_chained_assignment_parsing
