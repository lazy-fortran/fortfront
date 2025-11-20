! DTIO interface with unformatted I/O
module dtio_unformatted_module
    implicit none

    type :: binary_type
        integer :: id
        real :: data(10)
    end type binary_type

    interface write(unformatted)
        module procedure write_binary
    end interface

    interface read(unformatted)
        module procedure read_binary
    end interface

contains

    subroutine write_binary(dtv, unit, iostat, iomsg)
        class(binary_type), intent(in) :: dtv
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg
        write (unit, iostat=iostat, iomsg=iomsg) dtv%id, dtv%data
    end subroutine write_binary

    subroutine read_binary(dtv, unit, iostat, iomsg)
        class(binary_type), intent(inout) :: dtv
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg
        read (unit, iostat=iostat, iomsg=iomsg) dtv%id, dtv%data
    end subroutine read_binary

end module dtio_unformatted_module
