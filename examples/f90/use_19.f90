! Negative fixture for issue #2887 (reject-use-01).
! The ONLY clause imports an intrinsic operator the module does not extend.
! Derived from gfortran.dg/use_19.f90.
module m
    implicit none
end module m

program main
    use m, only: operator(/)
    implicit none
end program main
