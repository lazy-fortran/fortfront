! Issue #2416: READ/WRITE with format variables must parse correctly
program issue_2416_io_format_variable
    implicit none
    integer :: val, unit
    character(10) :: fmt

    val = 42
    unit = 10
    fmt = '(I5)'

    ! READ with format variable (not keyword fmt=)
    ! This was incorrectly consumed as keyword before fix
    read (unit, fmt) val

    ! WRITE with format variable
    write (unit, fmt) val
end program issue_2416_io_format_variable
