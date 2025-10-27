program test_issue_1972_do_if_hang
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    integer :: test_count, pass_count
    logical :: ok

    test_count = 1
    pass_count = 0

    call run_transformation_test(ok)
    if (ok) pass_count = pass_count + 1

    if (pass_count == test_count) then
        stop 0
    else
        stop 1
    end if

contains

    subroutine run_transformation_test(success)
        logical, intent(out) :: success
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        character(len=:), allocatable :: error_msg

        input = "a = [-5, -3, -1, 1, 3, 5]" // new_line('A') // &
                "sum_neg = 0" // new_line('A') // &
                "sum_pos = 0" // new_line('A') // new_line('A') // &
                "do i = 1, 6" // new_line('A') // &
                "    if (a(i) < 0) then" // new_line('A') // &
                "        sum_neg = sum_neg + a(i)" // new_line('A') // &
                "    else" // new_line('A') // &
                "        sum_pos = sum_pos + a(i)" // new_line('A') // &
                "    end if" // new_line('A') // &
                "end do" // new_line('A') // new_line('A') // &
                "print *, 'Sum negative:', sum_neg" // new_line('A') // &
                "print *, 'Sum positive:', sum_pos"

        call transform_lazy_fortran_string(input, output, error_msg)

        success = len_trim(error_msg) == 0 .and. &
                  len_trim(output) > 0 .and. &
                  index(output, "do i = 1, 6") > 0 .and. &
                  index(output, "if (a(i) < 0) then") > 0 .and. &
                  index(output, "end if") > 0 .and. &
                  index(output, "print *, 'Sum negative:'") > 0 .and. &
                  index(output, "print *, 'Sum positive:'") > 0
    end subroutine run_transformation_test

end program test_issue_1972_do_if_hang
