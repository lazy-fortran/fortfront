program test_issue_1783_use_rename_no_only
    use transformation_api, only: transform_lazy_fortran_string

    call test_use_statement_rename_without_only()
    call test_use_statement_rename_with_only()
    print *, ""
    print *, "All tests passed for issue 1783."

contains

    subroutine test_use_statement_rename_without_only()
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        integer :: idx_use

        input_code = "module orig_names" // new_line('A') // &
                     "    implicit none" // new_line('A') // &
                     "    integer :: value = 42" // new_line('A') // &
                     "contains" // new_line('A') // &
                     "    function compute() result(res)" // new_line('A') // &
                     "        integer :: res" // new_line('A') // &
                     "        res = value * 2" // new_line('A') // &
                     "    end function compute" // new_line('A') // &
                     "end module orig_names" // new_line('A') // new_line('A') // &
                     "program test_use_rename" // new_line('A') // &
                     "    use orig_names, my_value => value, my_compute => compute" // &
                     new_line('A') // &
                     "    implicit none" // new_line('A') // &
                     "    print *, 'Value:', my_value" // new_line('A') // &
                     "    print *, 'Compute:', my_compute()" // new_line('A') // &
                     "end program test_use_rename"

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "FAIL: unexpected transformation error:", trim(error_msg)
            error stop 1
        end if

        idx_use = index(output_code, &
                        "use orig_names, my_value => value, my_compute => compute")
        if (idx_use <= 0) then
            print *, "FAIL: use statement rename syntax without only not preserved"
            print *, "Output:"
            print *, trim(output_code)
            error stop 1
        end if

        if (index(output_code, "my_value => value") > 0 .and. &
            index(output_code, "use orig_names, my_value => value") <= 0) then
            print *, "FAIL: rename converted to pointer assignment"
            print *, "Output:"
            print *, trim(output_code)
            error stop 1
        end if

        print *, "PASS: USE rename without only preserved"
    end subroutine test_use_statement_rename_without_only

    subroutine test_use_statement_rename_with_only()
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        integer :: idx_use

        input_code = "module orig_names" // new_line('A') // &
                     "    implicit none" // new_line('A') // &
                     "    integer :: value = 42" // new_line('A') // &
                     "end module orig_names" // new_line('A') // new_line('A') // &
                     "program test_use_rename" // new_line('A') // &
                     "    use orig_names, only: my_value => value" // new_line('A') // &
                     "    implicit none" // new_line('A') // &
                     "    print *, my_value" // new_line('A') // &
                     "end program test_use_rename"

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "FAIL: unexpected transformation error:", trim(error_msg)
            error stop 1
        end if

        idx_use = index(output_code, "use orig_names, only: my_value => value")
        if (idx_use <= 0) then
            print *, "FAIL: use statement rename syntax with only not preserved"
            error stop 1
        end if

        print *, "PASS: USE rename with only preserved"
    end subroutine test_use_statement_rename_with_only

end program test_issue_1783_use_rename_no_only
