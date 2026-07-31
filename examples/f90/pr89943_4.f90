! Rejection fixture (gfortran.dg/pr89943_4.f90, PR fortran/89943):
! a separate module function must repeat the binding label of its module
! procedure interface body (F2018 C1550).
module pr89943_4_mod
    implicit none
    interface
        module function run_foo(ndim) bind(c, name="runFoo")
            integer, intent(in) :: ndim
            integer :: run_foo
        end function run_foo
    end interface
end module pr89943_4_mod

submodule (pr89943_4_mod) pr89943_4_sub
contains
    module function run_foo(ndim) bind(c, name="runFu")
        integer, intent(in) :: ndim
        integer :: run_foo
        run_foo = ndim
    end function run_foo
end submodule pr89943_4_sub
