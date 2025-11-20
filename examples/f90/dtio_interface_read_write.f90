! DTIO interface with both read and write
module dtio_read_write_module
    implicit none

    type :: data_type
        real :: x
        real :: y
    end type data_type

    interface write(formatted)
        module procedure write_data
    end interface

    interface read(formatted)
        module procedure read_data
    end interface

contains

    subroutine write_data(dtv, unit, iotype, v_list, iostat, iomsg)
        class(data_type), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg
        write (unit, "(2f10.3)", iostat=iostat, iomsg=iomsg) dtv%x, dtv%y
    end subroutine write_data

    subroutine read_data(dtv, unit, iotype, v_list, iostat, iomsg)
        class(data_type), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg
        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%x, dtv%y
    end subroutine read_data

end module dtio_read_write_module
