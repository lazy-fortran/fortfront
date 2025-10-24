program test_issue_1778_simple
    use fortfront
    implicit none
    character(len=:), allocatable :: input_code, output_code, error_msg

    print *, "Testing simple array literal..."

    ! Simple 1D array first
    input_code = "x = [1, 2, 3]"

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    print *, "Input:", trim(input_code)
    print *, "Output:", trim(output_code)
    print *, ""

    ! Check if 'x' variable is declared and assigned
    if (index(output_code, "x") == 0) then
        print *, "FAIL: x variable is missing"
        stop 1
    end if

    if (index(output_code, "[1, 2, 3]") > 0 .or. index(output_code, "x =") > 0) then
        print *, "PASS: Simple array literal"
    else
        print *, "FAIL: Assignment missing"
        stop 1
    end if

end program test_issue_1778_simple
