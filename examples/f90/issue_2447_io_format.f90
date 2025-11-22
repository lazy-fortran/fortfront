program test_io_format
    implicit none
    integer :: unit_num, io_stat
    real :: value
    character(len=100) :: io_msg

    ! Test OPEN with various specifiers
    open(unit=10, file='test.dat', status='unknown', iostat=io_stat, iomsg=io_msg)

    ! Test WRITE with format
    write(10, *) 42.0

    ! Test REWIND
    rewind(10)

    ! Test READ with iostat/iomsg
    read(10, *, iostat=io_stat, iomsg=io_msg) value

    ! Test CLOSE
    close(10)

    ! Test PRINT
    print *, 'Value:', value
end program test_io_format
