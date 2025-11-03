program test_intrinsics
    implicit none
    integer :: arr(5)
    integer :: s, m, n

    arr = [1, 5, 3, 9, 2]

    s = sum(arr)
    m = maxval(arr)
    n = size(arr)

    print *, 'Sum:', s
    print *, 'Max:', m
    print *, 'Size:', n
end program test_intrinsics
