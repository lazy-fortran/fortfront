program test_intrinsic_simple
    use intrinsic_registry
    implicit none
    
    logical :: result
    character(len=:), allocatable :: signature
    
    print *, "=== Simple Intrinsic Registry Test ==="
    
    ! Test is_intrinsic_function
    result = is_intrinsic_function("sin")
    print *, "sin is intrinsic:", result
    
    result = is_intrinsic_function("cos")
    print *, "cos is intrinsic:", result
    
    result = is_intrinsic_function("my_func")
    print *, "my_func is intrinsic:", result
    
    ! Test get_intrinsic_signature
    signature = get_intrinsic_signature("sin")
    print *, "sin signature:", signature
    
    signature = get_intrinsic_signature("sqrt")
    print *, "sqrt signature:", signature
    
    print *, "Simple test completed!"
    
end program test_intrinsic_simple