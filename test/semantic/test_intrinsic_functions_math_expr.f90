program test_intrinsic_functions_math_expr
    use frontend
    implicit none
    
    character(len=:), allocatable :: test_code
    character(len=:), allocatable :: output_code, error_msg
    
    print *, "Testing intrinsic functions in mathematical expressions..."
    
    ! Test the exact code from issue #92
    test_code = "program test" // new_line('a') // &
                "    implicit none" // new_line('a') // &
                "    real :: a = 1.0, b = 2.0, c = 3.0" // new_line('a') // &
                "    real :: result" // new_line('a') // &
                "    result = (a + b) * c / (a - b) + sqrt(a**2 + b**2)" // new_line('a') // &
                "    print *, result" // new_line('a') // &
                "end program"
    
    call transform_lazy_fortran_string(test_code, output_code, error_msg)
    
    if (len(error_msg) == 0) then
        print *, "✓ Complex mathematical expression with sqrt compiled successfully"
        print *, "✓ No type mismatch errors occurred"
    else
        print *, "ERROR: Failed to analyze mathematical expression"
        print *, "Error message:", trim(error_msg)
        stop 1
    end if
    
    ! Test simpler sqrt case
    test_code = "program test_simple" // new_line('a') // &
                "    real :: x = 4.0" // new_line('a') // &
                "    real :: result" // new_line('a') // &
                "    result = sqrt(x)" // new_line('a') // &
                "end program"
    
    call transform_lazy_fortran_string(test_code, output_code, error_msg)
    
    if (len(error_msg) == 0) then
        print *, "✓ Simple sqrt expression compiled successfully"
    else
        print *, "ERROR: Failed to analyze simple sqrt expression"
        print *, "Error message:", trim(error_msg)
        stop 1
    end if
    
    ! Test nested sqrt expressions
    test_code = "program test_nested" // new_line('a') // &
                "    real :: x = 4.0, y = 9.0" // new_line('a') // &
                "    real :: result" // new_line('a') // &
                "    result = sqrt(sqrt(x) + sqrt(y))" // new_line('a') // &
                "end program"
    
    call transform_lazy_fortran_string(test_code, output_code, error_msg)
    
    if (len(error_msg) == 0) then
        print *, "✓ Nested sqrt expressions compiled successfully"
    else
        print *, "ERROR: Failed to analyze nested sqrt expressions"
        print *, "Error message:", trim(error_msg)
        stop 1
    end if
    
    print *, "All intrinsic function tests passed!"
    
end program test_intrinsic_functions_math_expr