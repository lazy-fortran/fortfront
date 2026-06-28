program test_arithmetic_if
    implicit none
    integer :: x

    x = -5
    if (x) 10, 20, 30

    10  print *, "Negative"
    goto 40

    20  print *, "Zero"
    goto 40

    30  print *, "Positive"

    40  continue
end program test_arithmetic_if
