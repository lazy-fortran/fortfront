program test_issue_1857_chained_comparison
    implicit none

    logical :: test_passed

    test_passed = test_chained_comparison_output()

    if (test_passed) then
        print *, "PASS: Issue #1857 chained comparison detection"
    else
        print *, "FAIL: Issue #1857 - chained comparison not properly detected"
        stop 1
    end if

contains

    function test_chained_comparison_output() result(passed)
        use transformation_api, only: transform_lazy_fortran_string
        logical :: passed
        character(len=:), allocatable :: source, output, error_msg

        passed = .true.

        source = &
            "x = 5" // new_line('a') // &
            "result = 1 < x < 10" // new_line('a') // &
            "print *, 'Result:', result"

        print *, "===== Testing Issue 1857: Chained comparison detection ====="
        print *, "Input code:"
        print *, trim(source)

        call transform_lazy_fortran_string(source, output, error_msg)

        if (.not. allocated(output)) then
            print *, "ERROR: No output generated"
            passed = .false.
            return
        end if

        print *, "Generated output:"
        print *, trim(output)

        if (index(output, "< 10") /= 0) then
            print *, "ERROR: Output contains '< 10', chained comparison was not truncated"
            passed = .false.
        else if (index(output, "result = 1 < x") == 0) then
            print *, "ERROR: Output does not contain expected partial expression"
            passed = .false.
        else
            print *, "Good: Chained comparison was detected and truncated"
            print *, "Note: Error message appears on stderr during execution"
        end if

    end function test_chained_comparison_output

end program test_issue_1857_chained_comparison
