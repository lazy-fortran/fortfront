program test_array_scalar_assign
    implicit none
    integer, dimension(5) :: arr
    integer :: i

    ! Assign scalar to entire array
    arr = 7

    print *, 'Array after scalar assignment:', arr

    ! Multiply entire array by scalar
    arr = arr * 3

    print *, 'Array after multiplication:', arr
end program test_array_scalar_assign
