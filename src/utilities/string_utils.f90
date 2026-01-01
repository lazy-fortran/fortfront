module string_utils_mod
    implicit none
    private

    public :: int_to_string
    public :: to_lower

contains

    pure function to_lower(text) result(lower_text)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: lower_text
        integer :: idx
        integer :: char_code

        allocate (character(len=len(text)) :: lower_text)
        lower_text = text
        do idx = 1, len(text)
            char_code = iachar(lower_text(idx:idx))
            if (char_code >= iachar('A') .and. char_code <= iachar('Z')) then
                lower_text(idx:idx) = achar(char_code + 32)
            end if
        end do
    end function to_lower

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

        str = trim(buffer)
    end function int_to_string

end module string_utils_mod
