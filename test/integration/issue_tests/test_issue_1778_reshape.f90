program test_issue_1778_reshape
    use fortfront
    implicit none
    character(len=:), allocatable :: input_code, output_code, error_msg

    print *, "Testing with reshape..."

    ! Use reshape instead of nested arrays
    input_code = "data = reshape([1, 2, 3, 4], [2, 2])"

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "Error:", trim(error_msg)
    end if

    print *, "Input:", trim(input_code)
    if (allocated(output_code)) then
        print *, "Output length:", len(output_code)
        if (len(output_code) > 0) then
            print *, "Output:"
            print *, trim(output_code)
            if (index(output_code, "data") > 0) then
                print *, "PASS"
            else
                print *, "FAIL: data missing"
            end if
        else
            print *, "FAIL: empty output"
        end if
    else
        print *, "FAIL: output not allocated"
    end if

end program test_issue_1778_reshape
