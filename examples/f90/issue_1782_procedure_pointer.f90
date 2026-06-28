program test_function_pointer
    implicit none
    procedure(real_func), pointer :: fptr
    real :: result

    fptr => square
    result = fptr(5.0)
    print *, 'Result:', result

contains
    function square(x) result(res)
        real, intent(in) :: x
        real :: res
        res = x * x
    end function square

    interface
        function real_func(x) result(res)
            real, intent(in) :: x
            real :: res
        end function real_func
    end interface
end program test_function_pointer
