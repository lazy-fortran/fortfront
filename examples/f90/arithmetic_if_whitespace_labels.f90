program test_arithmetic_if_whitespace
    implicit none
    integer :: x

    x = 0
    if (x)   10  ,  20  ,  30

    10  print *, "Negative"
    goto 40

    20  print *, "Zero"
    goto 40

    30  print *, "Positive"

    40  continue
end program test_arithmetic_if_whitespace
