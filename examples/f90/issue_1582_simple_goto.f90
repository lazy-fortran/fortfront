program test_goto
    implicit none
    integer :: i

    i = 0
    goto 100
    i = 999
    100 continue
    print *, i
end program test_goto
