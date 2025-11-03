program test_assumed_shape
    implicit none
    real, dimension(10) :: arr
    integer :: i

    arr = [(real(i), i = 1, 10)]

    call print_array(arr)
    call print_array(arr(3:7))

contains

    subroutine print_array(a)
        real, dimension(:), intent(in) :: a
        integer :: n

        n = size(a)
        print *, 'Array size:', n
        print *, 'Values:', a
        print *, 'Sum:', sum(a)
    end subroutine print_array

end program test_assumed_shape
