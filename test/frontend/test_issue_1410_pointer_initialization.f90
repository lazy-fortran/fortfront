program test_issue_1410_pointer_initialization
    use transformation_api, only: transform_lazy_fortran_string

    call test_pointer_null_initializer()
    print *, ""
    print *, "All tests passed for issue 1410."

contains

    subroutine test_pointer_null_initializer()
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        integer :: idx

        input_code = "program pointer_init" // new_line('A') // &
            "    implicit none" // new_line('A') // &
            "    integer, pointer :: p => null()" // new_line('A') // &
            "    allocate(p)" // new_line('A') // &
            "    p = 5" // new_line('A') // &
            "    print *, p" // new_line('A') // &
            "end program pointer_init"

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "FAIL: unexpected transformation error:", trim(error_msg)
            error stop 1
        end if

        idx = index(output_code, "integer, pointer :: p => null()")
        if (idx <= 0) then
            print *, "FAIL: pointer initializer lost pointer association syntax"
            error stop 1
        end if

        if (index(output_code, "integer, pointer :: p = null") > 0) then
            print *, "FAIL: pointer initializer rewritten with '='"
            error stop 1
        end if

        print *, "PASS: pointer => null() initializer preserved"
    end subroutine test_pointer_null_initializer

end program test_issue_1410_pointer_initialization
