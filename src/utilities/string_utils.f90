module string_utils_mod
    implicit none
    private

    public :: int_to_string

contains

    pure function int_to_string(val, width) result(str)
        integer, intent(in) :: val
        integer, intent(in), optional :: width
        character(len=:), allocatable :: str
        character(len=64) :: buffer
        character(len=16) :: fmt
        integer :: fmt_width

        if (present(width)) then
            fmt_width = width
            if (fmt_width > 0) then
                write (fmt, '( "(I", I0, ")" )') fmt_width
                write (buffer, fmt) val
            else
                write (buffer, '(I0)') val
            end if
        else
            write (buffer, '(I0)') val
        end if

        str = trim(adjustl(buffer))
    end function int_to_string

end module string_utils_mod
