program test_io_iostat
    implicit none
    integer :: unit, iostat_val
    character(len=100) :: iomsg_val
    real :: value

    unit = 10
    value = 42.0

    write (unit, "(f10.5)", iostat=iostat_val, iomsg=iomsg_val) value
    read (unit, iostat=iostat_val, iomsg=iomsg_val) value
end program test_io_iostat
