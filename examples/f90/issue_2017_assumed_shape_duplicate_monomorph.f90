program test_assumed_shape
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    real(dp), dimension(10) :: arr
    integer :: i

    arr = [(real(i, dp), i = 1, 10)]

    call print_array(arr)
    call print_array(arr(3:7))

contains

    subroutine print_array(a)
        real(dp), dimension(:), intent(in) :: a
        integer :: n

        n = size(a)
        print *, 'Array size:', n
        print *, 'Values:', a
        print *, 'Sum:', sum(a)
    end subroutine print_array

end program test_assumed_shape
