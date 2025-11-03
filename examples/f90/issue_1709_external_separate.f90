program test_external
    implicit none
    real, external :: my_func
    real :: result

    result = my_func(5.0)
    print *, result
end program test_external

function my_func(x) result(y)
    real, intent(in) :: x
    real :: y
    y = x * 2.0
end function my_func
