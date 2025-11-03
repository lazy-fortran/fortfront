! INTRINSIC statement should be preserved during transformation
program intrinsic_statement_program
    implicit none
    intrinsic :: sin, cos
    real :: x
    x = sin(0.0)
    print *, cos(x)
end program intrinsic_statement_program
