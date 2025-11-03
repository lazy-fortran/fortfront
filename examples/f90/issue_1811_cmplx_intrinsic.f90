program test_complex_numbers
    implicit none
    complex :: z1, z2, z3
    z1 = (1.0, 2.0)
    z2 = cmplx(3.0, 4.0)
    z3 = z1 + z2
    print *, z1
    print *, z2
    print *, z3
    print *, real(z3)
    print *, aimag(z3)
end program test_complex_numbers
