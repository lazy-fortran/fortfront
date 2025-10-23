program test_namelist
    implicit none
    integer :: nx, ny
    real :: dx, dy

    namelist /grid/ nx, ny, dx, dy

    nx = 10
    ny = 20
    dx = 0.1
    dy = 0.2

    open(unit=10, file='namelist.txt', status='replace')
    write(10, nml=grid)
    close(10)

    print *, 'Namelist written'

end program test_namelist
