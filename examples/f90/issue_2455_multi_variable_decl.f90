! Test multi-variable parameter declaration
program test_multi_variable_decl
    implicit none
    real :: result(3, 3)
    real :: a(3, 3), b(3, 3)
    integer :: i, j

    ! Initialize test arrays
    do i = 1, 3
        do j = 1, 3
            a(i, j) = real(i + j)
            b(i, j) = real(i * j)
        end do
    end do

    call compute(a, b, result)

    print *, "Result:", result(1, 1)

contains
    subroutine compute(a, b, c)
        real, intent(in) :: a(3, 3), b(3, 3)
        real, intent(out) :: c(3, 3)

        c = a + b
    end subroutine compute
end program test_multi_variable_decl
