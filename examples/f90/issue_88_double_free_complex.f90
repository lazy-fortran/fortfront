program test
    implicit none
    integer :: i, j, n
    real :: matrix(100, 100)
    n = 100
    do i = 1, n
        do j = 1, n
            matrix(j, i) = real(i * j)
        end do
    end do
    call some_proc(matrix, n)
contains
    subroutine some_proc(mat, size)
        real, intent(in) :: mat(:,:)
        integer, intent(in) :: size
        print *, sum(mat)
    end subroutine
end program test
