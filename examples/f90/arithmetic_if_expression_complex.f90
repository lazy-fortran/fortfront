program test_arithmetic_if_complex_expr
    implicit none
    integer :: a, b, c

    a = 3
    b = 4
    c = -2
    if (a * b + c) 10, 20, 30

    10  print *, "Negative"
    goto 40

    20  print *, "Zero"
    goto 40

    30  print *, "Positive"

    40  continue
end program test_arithmetic_if_complex_expr
