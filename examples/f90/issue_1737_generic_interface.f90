! Issue #1737: generic interface inside a program must emit procedure bindings
program test_interface_generic
    implicit none

    interface swap
        module procedure swap_int, swap_real
    end interface swap

    integer :: a, b
    real :: x, y

    a = 5
    b = 10
    call swap(a, b)

    x = 1.5
    y = 2.5
    call swap(x, y)

contains

    subroutine swap_int(p, q)
        implicit none
        integer, intent(inout) :: p, q
        integer :: temp
        temp = p
        p = q
        q = temp
    end subroutine swap_int

    subroutine swap_real(p, q)
        implicit none
        real, intent(inout) :: p, q
        real :: temp
        temp = p
        p = q
        q = temp
    end subroutine swap_real

end program test_interface_generic
