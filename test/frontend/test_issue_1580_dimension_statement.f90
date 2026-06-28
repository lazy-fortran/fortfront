program test_issue_1580_dimension_statement
    use transformation_api, only: transform_lazy_fortran_string
    implicit none

    call run_dimension_conversion_test()
    print *, ""
    print *, "All dimension statement tests passed."

contains

    subroutine run_dimension_conversion_test()
        character(len=:), allocatable :: input_code
        character(len=:), allocatable :: output_code
        character(len=:), allocatable :: error_msg
        logical :: has_assignment

        input_code = "program test_dimension" // new_line('A') // &
            "    implicit none" // new_line('A') // &
            "    integer :: arr1" // new_line('A') // &
            "    real :: arr2" // new_line('A') // &
            "    dimension arr1(10)" // new_line('A') // &
            "    dimension arr2(5, 5)" // new_line('A') // new_line('A') // &
            "    arr1 = [(i, i=1,10)]" // new_line('A') // &
            "    print *, arr1(5)" // new_line('A') // &
            "end program test_dimension"

        call transform_lazy_fortran_string(input_code, output_code, error_msg)

        if (len_trim(error_msg) > 0) then
            print *, "FAIL: unexpected error:", trim(error_msg)
            error stop 1
        end if

        if (index(output_code, "integer :: arr1(10)") <= 0) then
            print *, "FAIL: arr1 dimension missing"
            error stop 1
        end if

        if (index(output_code, ":: arr2(5,5)") <= 0 .and. &
            index(output_code, ":: arr2(5, 5)") <= 0) then
            print *, "FAIL: arr2 dimension missing"
            error stop 1
        end if

        if (index(output_code, "arr1(10) =") > 0) then
            print *, "FAIL: array assignment still subscripts target"
            error stop 1
        end if

        has_assignment = index(output_code, "arr1 = [(i, i=1,10)]") > 0
        has_assignment = has_assignment .or. &
            index(output_code, "arr1 = [(i, i=1, 10)]") > 0
        has_assignment = has_assignment .or. &
            index(output_code, "arr1 = [(i, i = 1, 10)]") > 0

        if (.not. has_assignment) then
            print *, "FAIL: array constructor assignment not preserved"
            error stop 1
        end if

        print *, "PASS: dimension statement preserves array declarations"
    end subroutine run_dimension_conversion_test

end program test_issue_1580_dimension_statement
