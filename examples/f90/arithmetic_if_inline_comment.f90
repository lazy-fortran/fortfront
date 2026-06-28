program test_arithmetic_if_comment
    implicit none
    integer :: y

    y = 5
    if (y) 100, 200, 300 ! comment after arithmetic if

    100 print *, "Negative"
    goto 400

    200 print *, "Zero"
    goto 400

    300 print *, "Positive"

    400 continue
end program test_arithmetic_if_comment
