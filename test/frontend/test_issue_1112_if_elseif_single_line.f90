program test_issue_1112_if_elseif_single_line
    use transformation_api, only: transform_lazy_fortran_string

    integer :: test_count, pass_count
    logical :: ok

    test_count = 1
    pass_count = 0

    call run_test(ok)
    if (ok) pass_count = pass_count + 1

    if (pass_count == test_count) then
        stop 0
    else
        stop 1
    end if

contains

    subroutine run_test(success)
        logical, intent(out) :: success
        character(len=:), allocatable :: input, output, error_msg

        input = "if x > 0 then print('positive') else if x < 0 then print('negative') else print('zero')"

        call transform_lazy_fortran_string(input, output, error_msg)

        success = (len_trim(error_msg) == 0) .and. &
            (index(output, "if (x > 0) then") > 0) .and. &
            (index(output, "else if (x < 0) then") > 0) .and. &
            (index(output, "print *, 'positive'") > 0) .and. &
            (index(output, "print *, 'negative'") > 0) .and. &
            (index(output, "print *, 'zero'") > 0) .and. &
            (index(output, "end if") > 0)
    end subroutine run_test

end program test_issue_1112_if_elseif_single_line

