! Test case for issue #2458: Multiple array declarations on one line
! The bug: real, dimension(:,:), intent(in) :: a, b
! Was being transformed to: real :: b (scalar) instead of preserving array
module test_multi_array
contains
    subroutine process(a, b, c)
        real, dimension(:, :), intent(in) :: a, b
        real, dimension(:, :), intent(out) :: c
        c = matmul(a, transpose(b))
    end subroutine process
end module test_multi_array
