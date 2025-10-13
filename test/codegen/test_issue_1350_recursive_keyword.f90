program test_issue_1350_recursive_keyword
    ! Regression tests for GitHub issue #1350:
    !  - Recursive keyword dropped from function declarations
    !  - ELSE branch bodies removed from simple IF statements
    use fortfront
    implicit none

    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg

    print *, "=== Issue #1350: recursive keyword and IF/ELSE preservation ==="

    call run_recursive_if_test()

    print *, "All issue #1350 tests completed"

contains

    subroutine run_recursive_if_test()
        logical :: ok

        source = "module recursion_mod" // new_line('a') // &
                 "    implicit none" // new_line('a') // &
                 "contains" // new_line('a') // new_line('a') // &
                 "    recursive function factorial(n) result(f)" // new_line('a') // &
                 "        integer, intent(in) :: n" // new_line('a') // &
                 "        integer :: f" // new_line('a') // new_line('a') // &
                 "        if (n <= 1) then" // new_line('a') // &
                 "            f = 1" // new_line('a') // &
                 "        else" // new_line('a') // &
                 "            f = n * factorial(n - 1)" // new_line('a') // &
                 "        end if" // new_line('a') // &
                 "    end function factorial" // new_line('a') // new_line('a') // &
                 "end module recursion_mod"

        call transform_lazy_fortran_string(source, output, error_msg)

        if (allocated(error_msg)) then
            if (len_trim(error_msg) > 0) then
                print *, "  ERROR: ", trim(error_msg)
                stop 1
            end if
        end if

        ok = index(output, "recursive function factorial") > 0
        if (.not. ok) then
            print *, "  FAIL: recursive keyword missing"
            print *, "  Output: ", trim(output)
            stop 1
        end if

        ok = index(output, "else"//new_line('a')) > 0
        if (.not. ok) then
            print *, "  FAIL: else block missing from output"
            print *, "  Output: ", trim(output)
            stop 1
        end if

        ok = index(output, "if (!ERROR") == 0
        if (.not. ok) then
            print *, "  FAIL: fallback parser emitted unexpected placeholder"
            print *, "  Output: ", trim(output)
            stop 1
        end if

        ok = index(output, "f = n * factorial") > 0 .or. &
             index(output, "f = n*factorial") > 0
        if (.not. ok) then
            print *, "  FAIL: recursive assignment missing from ELSE branch"
            print *, "  Output: ", trim(output)
            stop 1
        end if

        print *, "  PASS: recursive keyword and IF/ELSE preserved"
    end subroutine run_recursive_if_test

end program test_issue_1350_recursive_keyword

