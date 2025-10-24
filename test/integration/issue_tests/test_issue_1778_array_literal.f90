program test_array_literal_bug
    use fortfront
    implicit none
    character(len=:), allocatable :: input_code, output_code, error_msg

    print *, "Testing array literal preservation in lazy fortran..."

    ! Test case from issue #1778
    input_code = "data = [[1, 2], [3, 4], [5, 6]]" // new_line('a') // &
                 "transposed = transpose(data)" // new_line('a') // &
                 "sum_all = sum(data)" // new_line('a') // &
                 "print *, ""Original:"", data" // new_line('a') // &
                 "print *, ""Transposed:"", transposed" // new_line('a') // &
                 "print *, ""Sum:"", sum_all"

    call transform_lazy_fortran_string(input_code, output_code, error_msg)

    if (len_trim(error_msg) > 0) then
        print *, "Error:", trim(error_msg)
    end if

    print *, ""
    print *, "Input:"
    print *, trim(input_code)
    print *, ""
    print *, "Output:"
    print *, trim(output_code)
    print *, ""

    ! Check if 'data' variable is declared
    if (index(output_code, "data") == 0) then
        print *, "FAIL: data variable is missing from output"
        stop 1
    end if

    ! Check if array literal is preserved in some form
    if (index(output_code, "reshape") > 0 .or. index(output_code, "data(") > 0 .or. &
        index(output_code, "data =") > 0) then
        print *, "PASS: Array literal appears to be preserved"
    else
        print *, "FAIL: Array literal was removed from output"
        stop 1
    end if

end program test_array_literal_bug
