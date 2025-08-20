program test_intrinsic_functions
    implicit none
    integer :: arr(5) = [1, 2, 3, 4, 5]
    integer :: n, s, m
    integer :: dims(1)
    real :: root
    
    ! Test SIZE function
    n = size(arr)
    if (n /= 5) error stop "size(arr) should be 5"
    
    ! Test SUM function  
    s = sum(arr)
    if (s /= 15) error stop "sum(arr) should be 15"
    
    ! Test SHAPE function
    dims = shape(arr)
    if (dims(1) /= 5) error stop "shape(arr) should be [5]"
    
    ! Test MAX function - integers
    m = max(1, 2, 3)
    if (m /= 3) error stop "max(1, 2, 3) should be 3"
    
    m = max(10, 5)
    if (m /= 10) error stop "max(10, 5) should be 10"
    
    m = max(-5, -10, -2)
    if (m /= -2) error stop "max(-5, -10, -2) should be -2"
    
    ! Test MIN function - integers
    m = min(1, 2, 3)
    if (m /= 1) error stop "min(1, 2, 3) should be 1"
    
    m = min(10, 5)
    if (m /= 5) error stop "min(10, 5) should be 5"
    
    m = min(-5, -10, -2)
    if (m /= -10) error stop "min(-5, -10, -2) should be -10"
    
    ! Test MAX/MIN with real numbers
    block
        real :: r
        
        r = max(1.5, 2.3, 1.8)
        if (abs(r - 2.3) > 1e-6) error stop "max(1.5, 2.3, 1.8) should be 2.3"
        
        r = min(3.7, 2.1, 4.2)
        if (abs(r - 2.1) > 1e-6) error stop "min(3.7, 2.1, 4.2) should be 2.1"
        
        r = max(-1.5, -0.5, -2.0)
        if (abs(r - (-0.5)) > 1e-6) error stop "max(-1.5, -0.5, -2.0) should be -0.5"
    end block
    
    ! Test with more integer variations
    block
        integer :: result_int
        
        result_int = max(5, 10, 3)  ! all integers → integer
        if (result_int /= 10) error stop "max(5, 10, 3) should be 10"
        
        result_int = min(8, 2, 15)  ! all integers → integer  
        if (result_int /= 2) error stop "min(8, 2, 15) should be 2"
    end block
    
    ! Test with more real variations
    block
        real :: result_real
        
        result_real = max(1.0, 2.5, 1.8)  ! all reals → real
        if (abs(result_real - 2.5) > 1e-6) error stop "max(1.0, 2.5, 1.8) should be 2.5"
        
        result_real = min(3.2, 1.1, 4.7)  ! all reals → real  
        if (abs(result_real - 1.1) > 1e-6) error stop "min(3.2, 1.1, 4.7) should be 1.1"
    end block
    
    ! Test SQRT function
    root = sqrt(16.0)
    if (abs(root - 4.0) > 1e-6) error stop "sqrt(16.0) should be 4.0"
    
    root = sqrt(25.0)
    if (abs(root - 5.0) > 1e-6) error stop "sqrt(25.0) should be 5.0"
    
    print *, "All tests passed!"
end program test_intrinsic_functions