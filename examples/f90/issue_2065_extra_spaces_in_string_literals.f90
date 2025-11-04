program test_namelist_io
    implicit none
    integer :: x, y
    real :: z
    namelist /input_data/ x, y, z

    x = 10
    y = 20
    z = 3.14

    open(unit=10, file='/tmp/test_namelist.dat', status='replace')
    write(10, nml=input_data)
    close(10)

    print *, "Written:", x, y, z
end program test_namelist_io
