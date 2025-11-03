! Module with interface block and module procedures
module math_interface_block
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    interface add
        module procedure add_int, add_real
    end interface add
contains
    function add_int(a, b) result(c)
        integer, intent(in) :: a, b
        integer :: c
        c = a + b
    end function add_int

    function add_real(a, b) result(c)
        real(dp), intent(in) :: a, b
        real(dp) :: c
        c = a + b
    end function add_real
end module math_interface_block
