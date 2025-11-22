module io_helpers
    implicit none
    interface
        subroutine write_formatted(unit, fmt, value)
            integer, intent(in) :: unit
            character(len=*), intent(in) :: fmt
            real, intent(in) :: value
        end subroutine write_formatted
    end interface
end module io_helpers
