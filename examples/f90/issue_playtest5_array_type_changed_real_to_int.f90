program issue_playtest5_array_type_changed_real_to_int
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    integer, parameter :: n = 5
    integer :: i
    real(dp), allocatable :: arr(:)

    allocate(arr(n))
    do i = 1, n
        arr(i) = real(i, kind=dp) * 2.0_dp
    end do

    print *, arr

end program issue_playtest5_array_type_changed_real_to_int
