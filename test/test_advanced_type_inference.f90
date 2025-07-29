program test_advanced_type_inference
    implicit none
    integer :: i
    real :: avg
    
    ! Test 1: Array literal type inference
    block
        integer :: arr(5)
        arr = [1, 2, 3, 4, 5]
        
        do i = 1, 5
            if (arr(i) /= i) then
                error stop "Type inference test failed: array literal values incorrect"
            end if
        end do
    end block
    
    ! Test 2: Mixed type arithmetic with intrinsics
    block
        integer :: int_arr(5)
        real :: result
        
        int_arr = [10, 20, 30, 40, 50]
        result = sum(int_arr) / size(int_arr)  ! Should promote to real
        
        if (abs(result - 30.0) > 1e-6) then
            error stop "Type inference test failed: mixed arithmetic incorrect"
        end if
    end block
    
    ! Test 3: Reshape intrinsic
    block
        integer :: matrix(2,2), flat(4)
        
        flat = [1, 2, 3, 4]
        matrix = reshape(flat, [2, 2])
        
        if (matrix(1,1) /= 1 .or. matrix(2,1) /= 2 .or. &
            matrix(1,2) /= 3 .or. matrix(2,2) /= 4) then
            error stop "Type inference test failed: reshape incorrect"
        end if
    end block
    
    ! Test 4: Function composition
    block
        real :: x, y, result
        
        x = 2.0
        y = 3.0
        result = sqrt(x*x + y*y)  ! Should infer real throughout
        
        if (abs(result - sqrt(13.0)) > 1e-6) then
            error stop "Type inference test failed: function composition incorrect"
        end if
    end block
    
    ! Test 5: String operations
    block
        character(len=:), allocatable :: str1, str2, combined
        
        str1 = "Hello"
        str2 = "World"
        combined = trim(str1) // " " // trim(str2)
        
        if (combined /= "Hello World") then
            error stop "Type inference test failed: string operations incorrect"
        end if
    end block
    
    print *, "All advanced type inference tests passed!"
    
end program test_advanced_type_inference