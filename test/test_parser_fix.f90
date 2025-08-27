program test_parser_fix
    ! Test for parser fix to handle explicit program statements
    use frontend
    implicit none
    
    character(len=:), allocatable :: source, generated, error_msg
    
    print *, "=== Testing parser fix for explicit program statements ==="
    
    ! Test 1: Explicit program with write statement
    source = "program test"//char(10)// &
             "    write(*,*) 'Hello'"//char(10)// &
             "end program test"
    
    print *, "Test 1: Explicit program with write statement"
    print *, "Source:"
    print *, trim(source)
    
    call transform_lazy_fortran_string(source, generated, error_msg)
    
    if (len_trim(error_msg) > 0) then
        print *, "ERROR: ", trim(error_msg)
        stop 1
    end if
    
    print *, "Generated:"
    print *, trim(generated)
    
    ! Check if write is preserved
    if (index(generated, "write") == 0) then
        print *, "FAIL: write statement not preserved"
        stop 1
    end if
    
    ! Test 2: Lazy Fortran with write statement
    source = "write(*,*) 'Hello'"
    
    print *, ""
    print *, "Test 2: Lazy Fortran with write statement"
    print *, "Source:"
    print *, trim(source)
    
    call transform_lazy_fortran_string(source, generated, error_msg)
    
    if (len_trim(error_msg) > 0) then
        print *, "ERROR: ", trim(error_msg)
        stop 1
    end if
    
    print *, "Generated:"
    print *, trim(generated)
    
    ! Check if write is preserved
    if (index(generated, "write") == 0) then
        print *, "FAIL: write statement not preserved"
        stop 1
    end if
    
    print *, ""
    print *, "PASS: All tests passed!"
    
end program test_parser_fix