program test_nullify_statement
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use fortfront, only: transform_lazy_fortran_string
    implicit none
    logical :: all_tests_passed
    integer :: test_count, pass_count

    test_count = 0
    pass_count = 0
    all_tests_passed = .true.

    call test_basic_nullify()
    call test_nullify_in_if()

    if (all_tests_passed) then
        print *, "All NULLIFY statement tests passed"
    else
        print *, "Some NULLIFY statement tests failed"
        error stop 1
    end if

contains

    subroutine test_basic_nullify()
        character(len=:), allocatable :: input, output, error_msg
        logical :: test_passed

        test_count = test_count + 1

        input = &
            "program test_nullify_basic" // new_line('A') // &
            "    implicit none" // new_line('A') // &
            "    integer, pointer :: ptr1, ptr2" // new_line('A') // &
            "    nullify(ptr1, ptr2)" // new_line('A') // &
            "    print *, 'Done'" // new_line('A') // &
            "end program test_nullify_basic"

        call transform_lazy_fortran_string(input, output, error_msg)

        test_passed = (index(output, "nullify(ptr1, ptr2)") > 0)
        test_passed = test_passed .and. &
            (index(output, "integer, pointer :: ptr1, ptr2") > 0)

        if (test_passed .and. len_trim(error_msg) == 0) then
            pass_count = pass_count + 1
            print *, "PASS: test_basic_nullify"
        else
            all_tests_passed = .false.
            print *, "FAIL: test_basic_nullify"
            if (len_trim(error_msg) > 0) print *, "Error:", trim(error_msg)
            print *, "Output:", trim(output)
        end if
    end subroutine test_basic_nullify

    subroutine test_nullify_in_if()
        character(len=:), allocatable :: input, output, error_msg
        logical :: test_passed

        test_count = test_count + 1

        input = &
            "program test_nullify_if" // new_line('A') // &
            "    implicit none" // new_line('A') // &
            "    integer, pointer :: ptr" // new_line('A') // &
            "    logical :: reset" // new_line('A') // &
            "    reset = .true." // new_line('A') // &
            "    if (reset) then" // new_line('A') // &
            "        nullify(ptr)" // new_line('A') // &
            "    end if" // new_line('A') // &
            "end program test_nullify_if"

        call transform_lazy_fortran_string(input, output, error_msg)

        test_passed = (index(output, "if (reset) then") > 0)
        test_passed = test_passed .and. (index(output, "nullify(ptr)") > 0)

        if (test_passed .and. len_trim(error_msg) == 0) then
            pass_count = pass_count + 1
            print *, "PASS: test_nullify_in_if"
        else
            all_tests_passed = .false.
            print *, "FAIL: test_nullify_in_if"
            if (len_trim(error_msg) > 0) print *, "Error:", trim(error_msg)
            print *, "Output:", trim(output)
        end if
    end subroutine test_nullify_in_if

end program test_nullify_statement
