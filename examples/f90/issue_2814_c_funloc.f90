program test_c_funloc
    use, intrinsic :: iso_c_binding
    implicit none

    interface
        subroutine c_sub() bind(c)
        end subroutine
    end interface

    type(c_funptr) :: fptr
    fptr = c_funloc(c_sub)
end program test_c_funloc
