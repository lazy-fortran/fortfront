! Issue #2416: Comprehensive I/O format test
program issue_2416_io_formats_comprehensive
    implicit none
    integer :: val
    character(10) :: fmt

    val = 42
    fmt = '(I5)'

    ! Label format with READ
    read (10, 100) val
    100 format (I5)

    ! String literal format
    read (10, '(I5)') val

    ! Format variable
    read (10, fmt) val

    ! List-directed (star format)
    read (*, *) val

    ! Keyword format (fmt=)
    read (10, fmt='(I5)') val

    ! WRITE variants
    write (*, 100) val
    write (*, '(I5)') val
    write (*, fmt) val
    write (*, *) val
end program issue_2416_io_formats_comprehensive
