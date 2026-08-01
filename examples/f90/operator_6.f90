! Negative fixture for issue #2887 (reject-use-01).
! An ONLY clause naming a defined operator of an empty module.
! Derived from gfortran.dg/operator_6.f90.
module foo
    implicit none
end module foo

program test
    use foo, only: operator(.none.)
    implicit none
end program test
