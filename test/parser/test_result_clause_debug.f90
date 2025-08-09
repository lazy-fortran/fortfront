program test_result_clause_debug
    ! Debug test for result clause functionality
    use fortfront
    implicit none
    
    character(len=:), allocatable :: source
    character(len=:), allocatable :: output
    character(len=:), allocatable :: error_msg
    
    print *, "=== Debug: Result Clause Functionality ==="
    
    source = "program test" // new_line('a') // &
             "contains" // new_line('a') // &
             "function simple_func() result(res)" // new_line('a') // &
             "real :: res" // new_line('a') // &
             "res = 42.0" // new_line('a') // &
             "end function simple_func" // new_line('a') // &
             "end program test"
    
    call transform_lazy_fortran_string(source, output, error_msg)
    
    if (allocated(error_msg)) then
        if (len_trim(error_msg) > 0) then
            print *, "ERROR: ", trim(error_msg)
            stop 1
        end if
    end if
    
    print *, "INPUT:"
    print *, trim(source)
    print *, ""
    print *, "OUTPUT:"
    print *, trim(output)
    print *, ""
    
    if (index(output, "result(res)") > 0) then
        print *, "PASS: Result clause found in output"
    else
        print *, "FAIL: Result clause missing from output"
        stop 1
    end if
    
end program test_result_clause_debug