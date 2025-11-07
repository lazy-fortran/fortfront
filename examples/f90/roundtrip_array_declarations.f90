! Comprehensive test for Array Declarations round-trip
! Tests: fixed-size arrays (various ranks), allocatable arrays, pointer arrays,
!        array parameters in procedures, assumed-shape arrays, explicit-shape arrays

program roundtrip_array_declarations
    implicit none

    ! Fixed-size arrays of various ranks
    integer :: fixed_1d(10)
    real :: fixed_2d(5, 5)
    real :: fixed_3d(3, 3, 3)

    ! Allocatable arrays
    integer, allocatable :: alloc_1d(:)
    real, allocatable :: alloc_2d(:, :)
    real, allocatable :: alloc_3d(:, :, :)

    ! Pointer arrays
    real, pointer :: ptr_1d(:)
    real, pointer :: ptr_2d(:, :)

    integer :: i, j, k

    ! Initialize fixed-size arrays
    do i = 1, 10
        fixed_1d(i) = i
    end do

    do i = 1, 5
        do j = 1, 5
            fixed_2d(i, j) = real(i + j)
        end do
    end do

    do i = 1, 3
        do j = 1, 3
            do k = 1, 3
                fixed_3d(i, j, k) = real(i * j * k)
            end do
        end do
    end do

    ! Allocate and initialize allocatable arrays
    allocate (alloc_1d(5))
    allocate (alloc_2d(3, 4))
    allocate (alloc_3d(2, 2, 2))

    alloc_1d = [1, 2, 3, 4, 5]
    alloc_2d = 0.0
    alloc_3d = 1.0

    ! Test procedure with explicit-shape array
    call process_explicit(fixed_1d, 10)

    ! Test procedure with assumed-shape array
    call process_assumed_shape(alloc_2d)

    ! Test pointer arrays
    ptr_1d => alloc_1d
    ptr_2d => alloc_2d

    print *, 'Fixed 1D:', fixed_1d(1:5)
    print *, 'Alloc 1D:', alloc_1d
    print *, 'Pointer sum:', sum(ptr_1d)

    ! Cleanup
    deallocate (alloc_1d)
    deallocate (alloc_2d)
    deallocate (alloc_3d)

contains

    ! Subroutine with explicit-shape array parameter
    subroutine process_explicit(arr, n)
        integer, intent(in) :: n
        integer, dimension(n), intent(inout) :: arr
        integer :: i

        do i = 1, n
            arr(i) = arr(i) * 2
        end do
    end subroutine process_explicit

    ! Subroutine with assumed-shape array parameter
    subroutine process_assumed_shape(matrix)
        real, dimension(:, :), intent(inout) :: matrix
        integer :: i, j

        do i = 1, size(matrix, 1)
            do j = 1, size(matrix, 2)
                matrix(i, j) = real(i * j)
            end do
        end do
    end subroutine process_assumed_shape

    ! Function returning allocatable array
    function create_array(n) result(arr)
        integer, intent(in) :: n
        real, allocatable :: arr(:)

        allocate (arr(n))
        arr = 1.0
    end function create_array

end program roundtrip_array_declarations
