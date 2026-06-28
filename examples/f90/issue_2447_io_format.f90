program test_io_format
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    integer :: unit_num, io_stat
    real(dp) :: value
    character(len=100) :: io_msg

    unit_num = 10

    ! Test OPEN with various specifiers
    open(unit=unit_num, file='test.dat', status='unknown', iostat=io_stat, &
        iomsg=io_msg)

    ! Test WRITE with format
    write(10, *) 42.0_dp

    ! Test REWIND
    rewind(unit_num)

    ! Test READ with iostat/iomsg
    read(unit_num, *, iostat=io_stat, iomsg=io_msg) value

    ! Test CLOSE
    close(unit_num)

    ! Test PRINT
    print *, 'Value:', value
end program test_io_format
