! Issue #2418: Codegen incorrectly declares intrinsic keyword arguments as variables
! Original test case demonstrating the bug
program test_intrinsic_keywords
    implicit none
    integer, dimension(3, 3) :: matrix
    integer :: min_index
    logical, dimension(3, 3) :: condition

    ! Initialize test data
    matrix = reshape([1, 4, 7, 2, 5, 8, 3, 6, 9], [3, 3])
    condition = matrix > 4

    ! Call intrinsic with keyword arguments
    ! Bug: fortfront incorrectly emits "real :: dim, mask" declarations
    min_index = minloc(matrix, dim=1, mask=condition)

    print *, "Minimum location:", min_index
end program test_intrinsic_keywords
