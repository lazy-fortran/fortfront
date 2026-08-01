! Negative fixture for issue #2887 (reject-use-01).
! The ONLY clause imports a defined operator the module does not export.
! F2023 14.2.2: every name in an only-list shall be a public entity of the
! module. Derived from gfortran.dg/use_9.f90.
module test
    implicit none
    interface operator(.bar.)
        module procedure func
    end interface
contains
    integer function func(a)
        integer, intent(in) :: a
        func = a + 1
    end function func
end module test

program main
    use test, only: operator(.func.)
    implicit none
end program main
