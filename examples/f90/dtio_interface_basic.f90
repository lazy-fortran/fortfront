! DTIO (Data Transfer I/O) interface - basic example
! Tests interface write(formatted) syntax
module dtio_basic_module
    implicit none

    type :: simple_type
        integer :: value
    end type simple_type

    interface write(formatted)
        module procedure write_simple
    end interface

contains

    subroutine write_simple(dtv, unit, iotype, v_list, iostat, iomsg)
        class(simple_type), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg
        write (unit, "(i5)", iostat=iostat, iomsg=iomsg) dtv%value
    end subroutine write_simple

end module dtio_basic_module
