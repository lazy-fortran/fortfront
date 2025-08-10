program test_issue_188_general_reassignment
    use frontend, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: input, output, error_msg

    ! Test case 1: General array reassignment (not self-referential)
    print *, "Test 1: General array reassignment"
    input = 'v = [1, 2, 3]' // char(10) // &
            'v = [10, 20, 30, 40]' // char(10) // &
            'print*,v'
    
    call transform_lazy_fortran_string(input, output, error_msg)
    
    if (len_trim(error_msg) > 0) then
        print *, "FAIL: Compilation failed: ", trim(error_msg)
        stop 1
    end if
    
    print *, "Generated output 1:"
    print *, trim(output)
    
    if (index(output, "allocatable ::") > 0) then
        print *, "PASS: Array reassignment triggers allocatable"
    else
        print *, "FAIL: Array reassignment should trigger allocatable"
        stop 1
    end if
    
    ! Test case 2: Array shrinking
    print *, ""
    print *, "Test 2: Array shrinking reassignment"
    input = 'arr = [1.0, 2.0, 3.0, 4.0]' // char(10) // &
            'arr = [5.0, 6.0]' // char(10) // &
            'print*,arr'
    
    call transform_lazy_fortran_string(input, output, error_msg)
    
    if (len_trim(error_msg) > 0) then
        print *, "FAIL: Compilation failed: ", trim(error_msg)
        stop 1
    end if
    
    print *, "Generated output 2:"
    print *, trim(output)
    
    if (index(output, "allocatable ::") > 0) then
        print *, "PASS: Array shrinking triggers allocatable"
    else
        print *, "FAIL: Array shrinking should trigger allocatable"
        stop 1
    end if
    
    print *, ""
    print *, "All tests passed - Fortran automatic reallocation detected!"
end program test_issue_188_general_reassignment