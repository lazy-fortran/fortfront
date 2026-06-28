program test_arithmetic_if_neg
    implicit none
    integer :: val

    val = -10
    if (val) 1, 2, 3

    1   print *, "Negative"
    goto 4

    2   print *, "Zero"
    goto 4

    3   print *, "Positive"

    4   continue
end program test_arithmetic_if_neg
