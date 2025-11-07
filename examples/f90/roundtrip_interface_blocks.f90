! Interface blocks round-trip test
module roundtrip_interface_blocks
    implicit none
    interface add
        module procedure add_int
        module procedure add_real
    end interface add
contains
    integer function add_int(a, b)
        integer, intent(in) :: a, b
        add_int = a + b
    end function add_int

    real function add_real(a, b)
        real, intent(in) :: a, b
        add_real = a + b
    end function add_real
end module roundtrip_interface_blocks
