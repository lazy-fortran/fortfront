program test_issue_1409_use_rename
    use transformation_api, only: transform_lazy_fortran_string

    call test_use_statement_with_rename()
    print *, ""
    print *, "All tests passed for issue 1409."

contains

    subroutine test_use_statement_with_rename()
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        integer :: idx_use

        input_code = "module constants" // new_line('A') // &
            "    implicit none" // new_line('A') // &
            "    integer, parameter :: ten = 10" // new_line('A') // &
            "end module constants" // new_line('A') // new_line('A') // &
            "program rename_test" // new_line('A') // &
            "    use constants, only: dozen => ten" // new_line('A') // &
            "    implicit none" // new_line('A') // &
            "    print *, dozen" // new_line('A') // &
            "end program rename_test"

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "FAIL: unexpected transformation error:", trim(error_msg)
            error stop 1
        end if

        idx_use = index(output_code, "use constants, only: dozen => ten")
        if (idx_use <= 0) then
            print *, "FAIL: use statement rename syntax not preserved"
            error stop 1
        end if

        if (index(output_code, "real :: dozen") > 0) then
            print *, "FAIL: superfluous declaration for renamed symbol emitted"
            error stop 1
        end if

        print *, "PASS: USE rename preserved"
    end subroutine test_use_statement_with_rename

end program test_issue_1409_use_rename
