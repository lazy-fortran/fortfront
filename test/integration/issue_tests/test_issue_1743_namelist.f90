program test_namelist
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    integer :: nx, ny
    real(dp) :: dx, dy

    namelist /grid/ nx, ny, dx, dy

    nx = 10
    ny = 20
    dx = 0.1_dp
    dy = 0.2_dp

    open (unit=10, file='namelist.txt', status='replace')
    write (10, nml=grid)
    close (10)

    print *, 'Namelist written'

end program test_namelist
