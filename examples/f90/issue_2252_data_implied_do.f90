program demo_data
    implicit none
    real :: coeff(2)
    data coeff / (i * 1.0, i = 1, 2) /
    print *, coeff
end program demo_data
