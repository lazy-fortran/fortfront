! Negative fixture for issue #2897, mirrored on gfortran.dg/pr56520.f90.
! A stray closing parenthesis follows a complete right-hand-side expression,
! so the statement is unclassifiable and must be rejected.
program misleading
    implicit none
    real a, c
    a = 1.0
    c = exp(+a) )
end program misleading
