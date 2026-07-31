! Negative fixture for issue #2897, mirrored on gfortran.dg/print_fmt_2.f90.
! Free form requires a blank between a statement keyword and a following name,
! so printf and printmynml are names, not PRINT statements, and neither forms a
! classifiable statement.
program p
    implicit none
    character(len=5) :: f = "(a)"
    real x
    namelist /mynml/ x
    printf, "check"
    x = 1
    printmynml
end program p
