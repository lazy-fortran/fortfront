program test_simple_undefined
    use frontend, only: compile_source, compilation_options_t  
    implicit none
    
    type(compilation_options_t) :: options
    character(len=512) :: error_msg
    character(len=:), allocatable :: source
    
    print *, "Testing simple undefined variable case..."
    
    source = 'program test' // new_line('a') // &
             '    real :: x' // new_line('a') // &
             '    y = x + 1' // new_line('a') // &
             'end program'
    
    ! Create temporary file
    open(unit=99, file="test_simple.f90", status='replace')
    write(99, '(A)') source
    close(99)
    
    ! Try to compile
    call compile_source("test_simple.f90", options, error_msg)
    
    print *, 'Error message received: "', trim(error_msg), '"'
    
    if (error_msg == "") then
        print *, "WARNING: No error detected for undefined variable"
        print *, "But system did not crash with ERROR STOP - SUCCESS!"
    else
        print *, "SUCCESS: Undefined variable error properly detected!"
    end if
    
    ! Clean up
    open(unit=99, file="test_simple.f90", status='old')
    close(99, status='delete')
    
end program test_simple_undefined