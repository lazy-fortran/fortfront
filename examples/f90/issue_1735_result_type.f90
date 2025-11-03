module issue_1735_module
    implicit none
contains
    integer function square(x) result(result)
        integer :: x
        result = x * x
    end function square
    double precision function cube(x) result(res)
        double precision :: x
        res = x * x * x
    end function cube
end module issue_1735_module
