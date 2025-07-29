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
    
    ! Test MAX function
    m = max(1, 2, 3)
    if (m /= 3) error stop "max(1, 2, 3) should be 3"
    
    m = max(10, 5)
    if (m /= 10) error stop "max(10, 5) should be 10"
    
    ! Test MIN function
    m = min(1, 2, 3)
    if (m /= 1) error stop "min(1, 2, 3) should be 1"
    
    m = min(10, 5)
    if (m /= 5) error stop "min(10, 5) should be 5"
    
    ! Test SQRT function
    root = sqrt(16.0)
    if (abs(root - 4.0) > 1e-6) error stop "sqrt(16.0) should be 4.0"
    
    root = sqrt(25.0)
    if (abs(root - 5.0) > 1e-6) error stop "sqrt(25.0) should be 5.0"
    
    print *, "All tests passed!"
end program test_intrinsic_functions