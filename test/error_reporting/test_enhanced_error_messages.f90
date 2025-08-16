program test_enhanced_error_messages
    use frontend, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source, output, error_msg

    print *, '=== Testing Enhanced Error Message Behavior ==='
    print *

    ! Test 1: Invalid syntax - missing 'then'
    print *, 'Test 1: Missing "then" in if statement'
    source = 'program test' // new_line('a') // &
            'if x > 0' // new_line('a') // &
            '  print *, x' // new_line('a') // &
            'end if' // new_line('a') // &
            'end program'
    
    call transform_lazy_fortran_string(source, output, error_msg)
    print *, 'Error message length:', len_trim(error_msg)
    print *, 'Error message: "' // error_msg // '"'
    print *, 'Output contains error comment:', index(output, '! COMPILATION') > 0
    print *, 'First few lines of output:'
    print *, output(1:min(200, len(output)))
    print *

    ! Test 2: Complete garbage  
    print *, 'Test 2: Complete garbage input'
    source = 'this is not fortran at all 123 *** %%% invalid'
    
    call transform_lazy_fortran_string(source, output, error_msg)
    print *, 'Error message length:', len_trim(error_msg)
    print *, 'Error message: "' // error_msg // '"'
    print *, 'Output contains error comment:', index(output, '! COMPILATION') > 0
    print *, 'First few lines of output:'
    print *, output(1:min(200, len(output)))
    print *

end program test_enhanced_error_messages