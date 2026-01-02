! Reproducer for issue #1353: derived type attributes in a program
program attr_type_test
    use, intrinsic :: iso_c_binding, only: c_double
    implicit none

    type, bind(c) :: point_t
        real(c_double) :: x
        real(c_double) :: y
    end type point_t

    type(point_t) :: p

    p%x = 1.0_c_double
    p%y = 2.0_c_double

    print *, p%x, p%y
end program attr_type_test
