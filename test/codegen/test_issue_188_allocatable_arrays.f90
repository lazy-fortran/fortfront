program test_issue_188_allocatable_arrays
    use frontend, only: transform_lazy_fortran_string
    implicit none
    character(len=:), allocatable :: input, output, error_msg

    ! Test case: Array that grows dynamically
    input = 'v = [10]' // char(10) // &
            'v = [v, v**2]' // char(10) // &
            'print*,v'
    
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
    
    ! Check that output uses allocatable declaration
    if (index(output, "allocatable ::") == 0) then
        print *, "FAIL: Should use allocatable declaration"
        stop 1
    end if
    
    ! Check that we don't have fixed-size dimension  
    if (index(output, ", dimension(1) ::") > 0) then
        print *, "FAIL: Should not have fixed dimension"
        stop 1
    end if
    
    print *, "PASS: Issue 188 - Dynamic arrays use allocatable declaration"
end program test_issue_188_allocatable_arrays