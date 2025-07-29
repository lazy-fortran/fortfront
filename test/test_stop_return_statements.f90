program test_stop_return
    implicit none
    integer :: result
    
    ! Test STOP with integer code
    if (.false.) stop 0
    
    ! Test STOP with string message
    if (.false.) stop "Error: this should not happen"
    
    ! Test RETURN in function
    result = test_return_function(5)
    if (result /= 10) error stop "test_return_function(5) should return 10"
    
    result = test_return_function(0)
    if (result /= -1) error stop "test_return_function(0) should return -1"
    
    ! Test early RETURN
    result = test_early_return(3)
    if (result /= 6) error stop "test_early_return(3) should return 6"
    
    result = test_early_return(-1)
    if (result /= 0) error stop "test_early_return(-1) should return 0"
    
    print *, "All tests passed!"
    stop 0
    
contains
    
    function test_return_function(x) result(y)
        integer, intent(in) :: x
        integer :: y
        
        if (x == 0) then
            y = -1
            return
        end if
        
        y = x * 2
        return
    end function test_return_function
    
    function test_early_return(x) result(y)
        integer, intent(in) :: x
        integer :: y
        
        y = 0  ! Default value
        
        if (x < 0) return  ! Early return with default
        
        y = x * 2
    end function test_early_return
    
end program test_stop_return