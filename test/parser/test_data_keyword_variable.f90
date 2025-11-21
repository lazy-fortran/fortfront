program test_data_keyword_variable
    ! Test that variable named "data" can be used in allocate statements
    ! Addresses issue #2419
    implicit none
    integer :: exit_code
    character(len=256) :: message
    
    ! Test that fortfront can parse the example file without errors
    call execute_command_line( &
        './build/gfortran_*/app/fortfront ' // &
        'examples/f90/issue_2419_data_variable_in_allocate.f90 > /dev/null', &
        exitstat=exit_code)
    
    if (exit_code /= 0) then
        print *, "FAIL: fortfront should parse variable named 'data' without error"
        error stop 1
    end if
    
    print *, "PASS: Variable named 'data' in allocate handled correctly"
end program test_data_keyword_variable
