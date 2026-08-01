! Invalid: SEQUENCE belongs to a derived-type definition, not to a function
! body. Reduced from gfortran.dg/misplaced_statement.f90.
real function misplaced_statement(x)
    real :: x
    sequence
end function misplaced_statement
