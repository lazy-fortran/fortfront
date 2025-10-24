program test_issue_1778_nested
    use fortfront
    implicit none
    character(len=:), allocatable :: input_code, output_code, error_msg

    print *, "Testing nested array literal..."

    ! Nested 2D array
    input_code = "data = [[1, 2], [3, 4]]"

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "Error message:", trim(error_msg)
    end if

    print *, "Input:", trim(input_code)
    print *, "Output length:", len(output_code)
    print *, "Output:"
    print *, trim(output_code)
    print *, ""

    ! Check if 'data' variable is declared
    if (index(output_code, "data") == 0) then
        print *, "FAIL: data variable is missing"
        stop 1
    end if

    ! Check if array literal or assignment is present
    if (index(output_code, "data =") > 0) then
        print *, "PASS: Assignment found"
    else
        print *, "FAIL: Assignment missing"
        print *, "Error:", trim(error_msg)
        stop 1
    end if

end program test_issue_1778_nested
