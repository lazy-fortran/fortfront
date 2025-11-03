program issue_1575
    implicit none
    integer :: arr(10)
    integer :: i

    arr = [(i*2, i=1,10)]

    print *, arr
end program issue_1575
