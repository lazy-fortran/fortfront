subroutine add_arrays(a, b, c, n)
    implicit none
    integer, intent(in) :: n
    real, dimension(n), intent(in) :: a, b
    real, dimension(n), intent(out) :: c

    c = a + b
end subroutine add_arrays
