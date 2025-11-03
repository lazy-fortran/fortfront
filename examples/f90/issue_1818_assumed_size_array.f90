! Issue #1818: assumed-size dummy arguments must stay as x(*)
program test_assumed_size_array
    implicit none
    integer :: arr(5)
    arr = [1, 2, 3, 4, 5]
    call print_array(arr, 5)
contains
    subroutine print_array(x, n)
        integer, intent(in) :: n
        integer, intent(in) :: x(*)
        integer :: i
        do i = 1, n
            print *, x(i)
        end do
    end subroutine print_array
end program test_assumed_size_array
