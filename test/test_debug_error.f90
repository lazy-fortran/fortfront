program test_debug_error
    use transformation_api, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: source, output, error_msg

    source = "program test" // new_line('a') // &
             "if x > 0" // new_line('a') // &
             "  print *, x" // new_line('a') // &
             "end if" // new_line('a') // &
             "end program"

    call transform_lazy_fortran_string(source, output, error_msg)

    print *, "=== ERROR_MSG CONTENT ==="
    print *, "Length:", len_trim(error_msg)
    if (len_trim(error_msg) > 0) then
        print *, "Content:", trim(error_msg)
        print *, "Has 'then':", index(error_msg, 'then') > 0
        print *, "Has 'Missing':", index(error_msg, 'Missing') > 0
        print *, "Has 'line':", index(error_msg, 'line') > 0
        print *, "Has 'column':", index(error_msg, 'column') > 0
        print *, "Has 'Suggestion':", index(error_msg, 'Suggestion') > 0
    else
        print *, "ERROR_MSG IS EMPTY!"
    end if

end program
