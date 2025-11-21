! Issue #2416: READ with NAMELIST support
program issue_2416_io_namelist_read
    implicit none
    integer :: val, unit
    namelist /mydata/ val

    val = 0
    unit = 10

    ! READ with namelist (nml=group)
    ! This was not supported in READ statements before fix
    read (unit, nml=mydata)

    print *, val
end program issue_2416_io_namelist_read
