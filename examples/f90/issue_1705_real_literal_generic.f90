! Issue #1705: real literals must not be promoted in generic interfaces
module math_mod
    implicit none
    interface add_values
        module procedure add_integers, add_reals
    end interface add_values
contains
    function add_integers(a, b) result(sum)
        implicit none
        integer, intent(in) :: a, b
        integer :: sum
        sum = a + b
    end function add_integers

    function add_reals(a, b) result(sum)
        implicit none
        real, intent(in) :: a, b
        real :: sum
        sum = a + b
    end function add_reals
end module math_mod

program test_real_literal_generic
    use math_mod
    implicit none

    print *, add_values(5, 3)
    print *, add_values(5.0, 3.0)
end program test_real_literal_generic
