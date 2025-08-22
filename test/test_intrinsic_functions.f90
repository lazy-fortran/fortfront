program test_intrinsic_functions
    implicit none
    integer :: arr(5) = [1, 2, 3, 4, 5]
    integer :: n, s, m
    integer :: dims(1)
    real :: root
    
    ! Test SIZE function
    n = size(arr)
    if (n /= 5) then
        print *, "size(arr) should be 5"
        stop 1
    end if
    
    ! Test SUM function  
    s = sum(arr)
    if (s /= 15) then
        print *, "sum(arr) should be 15"
        stop 1
    end if
    
    ! Test SHAPE function
    dims = shape(arr)
    if (dims(1) /= 5) then
        print *, "shape(arr) should be [5]"
        stop 1
    end if
    
    ! Test MAX function
    m = max(1, 2, 3)
    if (m /= 3) then
        print *, "max(1, 2, 3) should be 3"
        stop 1
    end if
    
    m = max(10, 5)
    if (m /= 10) then
        print *, "max(10, 5) should be 10"
        stop 1
    end if
    
    ! Test MIN function
    m = min(1, 2, 3)
    if (m /= 1) then
        print *, "min(1, 2, 3) should be 1"
        stop 1
    end if
    
    m = min(10, 5)
    if (m /= 5) then
        print *, "min(10, 5) should be 5"
        stop 1
    end if
    
    ! Test SQRT function
    root = sqrt(16.0)
    if (abs(root - 4.0) > 1e-6) then
        print *, "sqrt(16.0) should be 4.0"
        stop 1
    end if
    
    root = sqrt(25.0)
    if (abs(root - 5.0) > 1e-6) then
        print *, "sqrt(25.0) should be 5.0"
        stop 1
    end if
    
    print *, "All tests passed!"
end program test_intrinsic_functions