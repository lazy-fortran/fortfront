! Minimal reproducer for issue #1347: INTERFACE blocks cause parser error
module math_interface
    implicit none

    interface add
        module procedure add_int
    end interface add

contains

    function add_int(a, b) result(c)
        integer, intent(in) :: a, b
        integer :: c
        c = a + b
    end function add_int

end module math_interface

program test_interface
    use math_interface
    implicit none
    print *, add(5, 3)
end program test_interface
