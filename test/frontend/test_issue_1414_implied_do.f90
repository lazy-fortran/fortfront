program test_issue_1414_implied_do
    use transformation_api, only: transform_lazy_fortran_string

    call test_print_implied_do()
    print *, ""
    print *, "All tests passed for issue 1414."

contains

    subroutine test_print_implied_do()
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        integer :: idx

        input_code = "program implied" // new_line('A') // &
            "    implicit none" // new_line('A') // &
            "    integer :: i" // new_line('A') // &
            "    print *, (i, i = 1, 3)" // new_line('A') // &
            "end program implied"

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "FAIL: unexpected error:", trim(error_msg)
            error stop 1
        end if

        idx = index(output_code, "print *, (i, i = 1, 3)")
        if (idx <= 0) then
            print *, "FAIL: implied-do syntax not preserved"
            error stop 1
        end if

        if (index(output_code, "print *, i") > 0 .and. idx <= 0) then
            print *, "FAIL: implied-do expanded to flat list"
            error stop 1
        end if

        print *, "PASS: implied-do in print preserved"
    end subroutine test_print_implied_do

end program test_issue_1414_implied_do
