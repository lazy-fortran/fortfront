program test_memory_corruption
    implicit none
    real :: a, b, c, matrix(3,3)
    integer :: i, j, k, n
    n = 3
    ! Test nested loops with matrix operations (original crash scenario)
    do i = 1, n
        do j = 1, n
            matrix(i,j) = real(i) * real(j)
            if (i == j) then
                matrix(i,j) = matrix(i,j) + 1.0
            end if
        end do
    end do
    ! Test complex binary expressions
    a = 1.0
    b = 2.0
    c = a + b * 3.0
    k = i + j - 5
    c = c + real(k) + matrix(1,1)
    print *, 'Memory corruption test completed'
end program test_memory_corruption
