! Minimal reproducer for issue #1546: GOTO and labels dropped
program test
    implicit none
    integer :: i

    i = 0
10  i = i + 1
    print *, i
    if (i < 3) goto 10
end program test
