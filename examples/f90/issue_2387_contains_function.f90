module test_module
    implicit none

contains

    function square(x) result(res)
        implicit none
        integer, intent(in) :: x
        integer :: res
        res = x * x
    end function square

    subroutine print_square(n)
        implicit none
        integer, intent(in) :: n
        print *, square(n)
    end subroutine print_square

end module test_module