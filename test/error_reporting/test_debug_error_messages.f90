program test_debug_error_messages
    use frontend, only: transform_lazy_fortran_string
    implicit none

    character(len=:), allocatable :: source, output, error_msg

    print *, '=== Debugging Current Error Message Behavior ==='
    print *

    ! Test 1: Invalid syntax - missing 'then'
    print *, 'Test 1: Missing "then" in if statement'
    source = 'program test' // new_line('a') // &
            'if x > 0' // new_line('a') // &
            '  print *, x' // new_line('a') // &
            'end if' // new_line('a') // &
            'end program'
    
    call transform_lazy_fortran_string(source, output, error_msg)
    print *, 'Error message: "' // error_msg // '"'
    print *, 'Output length:', len_trim(output)
    if (len_trim(output) > 0) then
        print *, 'Output: "' // output // '"'
    end if
    print *

    ! Test 2: Parameter declaration  
    print *, 'Test 2: Parameter declaration'
    source = 'program test' // new_line('a') // &
            'integer, parameter :: n = 10' // new_line('a') // &
            'print *, n' // new_line('a') // &
            'end program'
    
    call transform_lazy_fortran_string(source, output, error_msg)
    print *, 'Error message: "' // error_msg // '"'
    print *, 'Output length:', len_trim(output)
    if (len_trim(output) > 0) then
        print *, 'Output: "' // output // '"'
    end if
    print *

    ! Test 3: Incomplete expression
    print *, 'Test 3: Incomplete expression (trailing operator)'
    source = 'program test' // new_line('a') // &
            'integer :: x' // new_line('a') // &
            'x = 42 +' // new_line('a') // &
            'print *, x' // new_line('a') // &
            'end program'
    
    call transform_lazy_fortran_string(source, output, error_msg)
    print *, 'Error message: "' // error_msg // '"'
    print *, 'Output length:', len_trim(output)
    if (len_trim(output) > 0) then
        print *, 'Output: "' // output // '"'
    end if
    print *

    ! Test 4: Complete garbage
    print *, 'Test 4: Complete garbage input'
    source = 'this is not fortran at all 123 *** %%% invalid'
    
    call transform_lazy_fortran_string(source, output, error_msg)
    print *, 'Error message: "' // error_msg // '"'
    print *, 'Output length:', len_trim(output)
    if (len_trim(output) > 0) then
        print *, 'Output: "' // output // '"'
    end if
    print *

    ! Test 5: Missing end program
    print *, 'Test 5: Missing end program'
    source = 'program test' // new_line('a') // &
            'integer :: x' // new_line('a') // &
            'x = 42'
    
    call transform_lazy_fortran_string(source, output, error_msg)
    print *, 'Error message: "' // error_msg // '"'
    print *, 'Output length:', len_trim(output)
    if (len_trim(output) > 0) then
        print *, 'Output: "' // output // '"'
    end if
    print *

end program test_debug_error_messages