program test_generic_interface
    implicit none

    interface add
        module procedure add_int, add_real
    end interface add

    print *, 'Int result:', add(5, 3)
    print *, 'Real result:', add(5.0, 3.0)

contains

    integer function add_int(a, b)
        integer, intent(in) :: a, b
        add_int = a + b
    end function add_int

    real function add_real(a, b)
        real, intent(in) :: a, b
        add_real = a + b
    end function add_real

end program test_generic_interface
