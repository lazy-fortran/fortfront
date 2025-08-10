program test_issue_187_char_arrays
    use frontend, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: input, output, error_msg

    ! Test case: Character array with unequal length elements
    input = 's = ["boy", "girl"]' // char(10) // 'print*,s'
    
    ! Transform the source
    call transform_lazy_fortran_string(input, output, error_msg)
    
    if (len_trim(error_msg) > 0) then
        print *, "FAIL: Compilation failed: ", trim(error_msg)
        stop 1
    end if
    
    ! Debug: Show the actual output
    print *, "Generated output:"
    print *, "=================="
    print *, trim(output)
    print *, "=================="
    
    ! Check that character length is correct (4, not 3)
    if (index(output, "character(len=4)") == 0) then
        print *, "FAIL: Should use character(len=4)"
        stop 1
    end if
    
    ! Check for character type specifier in array constructor
    if (index(output, "[character(len=4) ::") == 0) then
        print *, "FAIL: Should include character type specifier"
        stop 1
    end if
    
    print *, "PASS: Issue 187 - Character arrays with unequal lengths fixed"
end program test_issue_187_char_arrays