program test_issue_1573_pointer_assignment_lost
    ! Test for issue #1573: Pointer assignment (=>) lost during transformation
    ! Bug: Pointer assignment is silently dropped when followed by print then regular assignment
    use frontend, only: transform_lazy_fortran_string
    implicit none

    call test_pointer_assignment_preserved_case1()
    call test_pointer_assignment_preserved_case2()
    call test_pointer_assignment_lost_bug()
    print *, ""
    print *, "NOTE: test_pointer_assignment_lost_bug is expected to FAIL until issue #1573 is fixed"

contains

    subroutine test_pointer_assignment_preserved_case1()
        ! This case works: pointer assignment -> regular assignment -> print
        character(len=:), allocatable :: input_code, output_code, error_msg

        input_code = "program test" // new_line('A') // &
                     "integer, target :: x" // new_line('A') // &
                     "integer, pointer :: p" // new_line('A') // &
                     "p => x" // new_line('A') // &
                     "p = 100" // new_line('A') // &
                     "print *, p" // new_line('A') // &
                     "end program test"

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "FAIL case1: transformation error:", trim(error_msg)
            error stop 1
        end if

        if (index(output_code, "p => x") <= 0) then
            print *, "FAIL case1: pointer assignment lost"
            print *, "Expected: p => x"
            print *, "Output:", trim(output_code)
            error stop 1
        end if

        print *, "PASS case1: pointer assignment preserved (assignment before print)"
    end subroutine test_pointer_assignment_preserved_case1

    subroutine test_pointer_assignment_preserved_case2()
        ! This case works: pointer assignment with no subsequent regular assignment
        character(len=:), allocatable :: input_code, output_code, error_msg

        input_code = "program test" // new_line('A') // &
                     "integer, target :: x" // new_line('A') // &
                     "integer, pointer :: p" // new_line('A') // &
                     "p => x" // new_line('A') // &
                     "print *, p" // new_line('A') // &
                     "end program test"

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "FAIL case2: transformation error:", trim(error_msg)
            error stop 1
        end if

        if (index(output_code, "p => x") <= 0) then
            print *, "FAIL case2: pointer assignment lost"
            print *, "Expected: p => x"
            print *, "Output:", trim(output_code)
            error stop 1
        end if

        print *, "PASS case2: pointer assignment preserved (no assignment after)"
    end subroutine test_pointer_assignment_preserved_case2

    subroutine test_pointer_assignment_lost_bug()
        ! BUG: This case fails - pointer assignment -> print -> regular assignment
        character(len=:), allocatable :: input_code, output_code, error_msg

        input_code = "program test" // new_line('A') // &
                     "integer, target :: x" // new_line('A') // &
                     "integer, pointer :: p" // new_line('A') // &
                     "p => x" // new_line('A') // &
                     "print *, p" // new_line('A') // &
                     "p = 100" // new_line('A') // &
                     "end program test"

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "XFAIL bug: transformation error:", trim(error_msg)
            ! Don't error stop - this is expected to fail
            return
        end if

        if (index(output_code, "p => x") <= 0) then
            print *, "XFAIL bug: pointer assignment lost (KNOWN BUG #1573)"
            print *, "Trigger: pointer assignment -> print -> regular assignment"
            print *, "Expected: p => x"
            print *, "Actual output:"
            print *, trim(output_code)
            ! Don't error stop - this is expected to fail
            return
        end if

        print *, "UNEXPECTED PASS: bug appears to be fixed! Issue #1573 may be resolved."
    end subroutine test_pointer_assignment_lost_bug

end program test_issue_1573_pointer_assignment_lost
