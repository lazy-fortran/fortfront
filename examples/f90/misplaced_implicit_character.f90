! Invalid: IMPLICIT must precede every data declaration statement of its
! scoping unit. Reduced from gfortran.dg/misplaced_implicit_character.f90.
subroutine misplaced_implicit_character(x)
    real :: x
    implicit character(a)
    x = 1
end subroutine misplaced_implicit_character
