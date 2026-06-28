program test_cgoto
    implicit none
    integer :: choice
    choice = 2
    goto (100, 200, 300), choice
    100 print *, "One"
    goto 999
    200 print *, "Two"
    goto 999
    300 print *, "Three"
    999 continue
end program test_cgoto
