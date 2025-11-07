! Comprehensive test for Module Features round-trip
! Tests: module procedures with array parameters, subroutines, module variables,
!        public/private access control, generic interfaces

module roundtrip_module_features
    implicit none

    ! Module variables with access control
    integer, public :: public_counter = 0
    real, private :: private_data = 0.0

    ! Public/private declarations
    private :: internal_helper
    public :: add_arrays, process_matrix, compute_result

    ! Generic interface
    interface compute_result
        module procedure compute_result_real
        module procedure compute_result_integer
    end interface compute_result

contains

    ! Subroutine with multiple array parameters of different ranks
    subroutine add_arrays(a, b, c, n)
        integer, intent(in) :: n
        real, dimension(n), intent(in) :: a, b
        real, dimension(n), intent(out) :: c

        c = a + b
        public_counter = public_counter + 1
    end subroutine add_arrays

    ! Subroutine with 2D array parameter
    subroutine process_matrix(mat, nrows, ncols)
        integer, intent(in) :: nrows, ncols
        real, dimension(nrows, ncols), intent(inout) :: mat
        integer :: i, j

        do i = 1, nrows
            do j = 1, ncols
                mat(i, j) = mat(i, j) * 2.0
            end do
        end do
    end subroutine process_matrix

    ! Generic interface implementations
    real function compute_result_real(x, y) result(res)
        real, intent(in) :: x, y
        res = x + y + private_data
    end function compute_result_real

    integer function compute_result_integer(x, y) result(res)
        integer, intent(in) :: x, y
        res = x + y
    end function compute_result_integer

    ! Private helper function
    subroutine internal_helper()
        private_data = private_data + 1.0
    end subroutine internal_helper

end module roundtrip_module_features

program test_module_features
    use roundtrip_module_features, only: add_arrays, compute_result, public_counter
    implicit none

    real :: a(5), b(5), c(5)
    integer :: i, int_result
    real :: real_result

    do i = 1, 5
        a(i) = real(i)
        b(i) = real(i) * 2.0
    end do

    call add_arrays(a, b, c, 5)

    int_result = compute_result(3, 4)
    real_result = compute_result(3.0, 4.0)

    print *, 'Counter:', public_counter
    print *, 'Results:', int_result, real_result
    print *, 'Array c:', c(1:3)

end program test_module_features
