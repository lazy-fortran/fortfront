module io_helpers
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    interface
        subroutine write_formatted(unit, fmt, value)
            use, intrinsic :: iso_fortran_env, only: dp => real64
            implicit none
            integer, intent(in) :: unit
            character(len=*), intent(in) :: fmt
            real(dp), intent(in) :: value
        end subroutine write_formatted
    end interface
end module io_helpers
