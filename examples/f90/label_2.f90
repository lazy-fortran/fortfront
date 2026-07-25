! INVALID: in free source form a statement label must be separated from the
! statement it labels. Fortran 2023 clause 6.3.2.3; gfortran.dg/label_2.f90.
program label_2
    10: a = 10
end program label_2
