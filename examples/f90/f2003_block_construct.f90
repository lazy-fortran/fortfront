! Example demonstrating block construct within procedure bodies
! ISO/IEC 1539-1:2018 Section 11.1.4 - BLOCK construct
module block_construct_demo_mod
    implicit none

contains

    subroutine compute_with_blocks(arr, n, total)
        integer, intent(in) :: n
        real, intent(in) :: arr(n)
        real, intent(out) :: total
        integer :: i

        total = 0.0

        ! Block construct inside procedure body
        block
            real :: partial_sum
            partial_sum = 0.0
            do i = 1, n / 2
                partial_sum = partial_sum + arr(i)
            end do
            total = total + partial_sum
        end block

        ! Second block with different local scope
        block
            real :: partial_sum
            partial_sum = 0.0
            do i = n / 2 + 1, n
                partial_sum = partial_sum + arr(i)
            end do
            total = total + partial_sum
        end block

    end subroutine compute_with_blocks

end module block_construct_demo_mod
