program main
    implicit none
    integer :: x

    x = 1
    if (x > 0) then
        print *, "positive"
    else if (x < 0) then
        print *, "negative"
    else
        print *, "zero"
    end if
end program main
