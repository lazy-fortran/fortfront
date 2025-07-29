program test_optional_params
    implicit none
    real :: result1, result2, result3
    
    ! Test subroutine with optional parameter
    call compute(1.0, 2.0, result1)
    if (abs(result1 - 3.0) > 1e-6) then
        error stop "Optional test failed: default tolerance result incorrect"
    end if
    
    call compute(1.0, 2.0, result2, 0.1)
    if (abs(result2 - 3.0) > 1e-6) then
        error stop "Optional test failed: custom tolerance result incorrect"
    end if
    
    ! Test function with optional parameter
    result3 = add_numbers(5.0, 3.0)
    if (abs(result3 - 8.0) > 1e-6) then
        error stop "Optional test failed: function without optional incorrect"
    end if
    
    result3 = add_numbers(5.0, 3.0, 2.0)
    if (abs(result3 - 10.0) > 1e-6) then
        error stop "Optional test failed: function with optional incorrect"
    end if
    
    print *, "All OPTIONAL parameter tests passed!"
    
contains
    
    subroutine compute(x, y, result, tol)
        real, intent(in) :: x, y
        real, intent(out) :: result
        real, intent(in), optional :: tol
        real :: tolerance
        
        if (present(tol)) then
            tolerance = tol
        else
            tolerance = 1.0e-6
        end if
        
        result = x + y + tolerance * 0.0  ! Just to use tolerance
    end subroutine compute
    
    function add_numbers(a, b, c) result(sum)
        real, intent(in) :: a, b
        real, intent(in), optional :: c
        real :: sum
        
        if (present(c)) then
            sum = a + b + c
        else
            sum = a + b
        end if
    end function add_numbers
    
end program test_optional_params