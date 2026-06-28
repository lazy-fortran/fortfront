program demo
    implicit none
    integer :: i
    i = 0
    10  i = i + 1
    if (i < 3) goto 10
    stop
end program demo
