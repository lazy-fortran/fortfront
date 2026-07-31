! Rejection fixture (gfortran.dg/pr89943_3.f90, PR fortran/89943):
! a separate module subroutine must repeat the binding label of its module
! procedure interface body (F2018 C1550).
module pr89943_3_mod
    implicit none
    interface
        module subroutine run_foo(ndim) bind(c, name="runFoo")
            integer, intent(in) :: ndim
        end subroutine run_foo
    end interface
end module pr89943_3_mod

submodule (pr89943_3_mod) pr89943_3_sub
contains
    module subroutine run_foo(ndim) bind(c, name="runFu")
        integer, intent(in) :: ndim
        print *, ndim
    end subroutine run_foo
end submodule pr89943_3_sub
