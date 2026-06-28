module test_mod
    use iso_fortran_env
    interface operator(+)
        module procedure add_custom
    end interface
    type :: point
        real :: x, y
    end type
contains
    function add_custom(a, b)
        type(point), intent(in) :: a, b
        type(point) :: add_custom
    end function
    subroutine test_sub(x, y)
        real, intent(in) :: x
        real, intent(out) :: y
        y = x * 2
    end subroutine
end module
