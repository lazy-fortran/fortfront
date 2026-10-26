program io_implied_do_objects
    implicit none
    integer :: i, j, a(2, 2)
    character(len=32) :: record

    a = reshape([1, 2, 3, 4], [2, 2])
    print '(4(I0,1X))', ((a(i, j), i=1, 2), j=1, 2)
    write (record, '(4(I0,1X))') (i, -i, i=2, 1, -1)
    read (record, *) ((a(i, j), i=1, 2), j=1, 2)
    if (any(a /= reshape([2, -2, 1, -1], [2, 2]))) error stop 1
end program io_implied_do_objects
