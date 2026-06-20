module issue_2813_value_attribute
    use iso_c_binding, only: c_int, c_double
    implicit none

contains

    integer(c_int) function add(a, b)
        integer(c_int), value :: a, b
        add = a + b
    end function add

    subroutine scale_value(n, factor)
        integer(c_int), value :: n
        real(c_double), value :: factor
        print *, n, factor
    end subroutine scale_value

end module issue_2813_value_attribute
