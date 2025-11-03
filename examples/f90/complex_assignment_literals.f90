! Complex assignment should preserve literal imaginary parts
program complex_assignment_literals
    implicit none
    complex :: z1, z2, result, zsum
    z1 = (3.0, 4.0)
    z2 = (1.0, 2.0)
    result = z1 + z2
    zsum = (3.0, 4.0) + z2
end program complex_assignment_literals
