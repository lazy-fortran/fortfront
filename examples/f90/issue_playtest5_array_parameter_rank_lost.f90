subroutine add_arrays(a, b, c, n)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: a(n), b(n)
    real, intent(out) :: c(n)

    c = a + b
end subroutine add_arrays
