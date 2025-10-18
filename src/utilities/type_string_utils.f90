module type_string_utils
    implicit none
    private

    public :: is_character_type_string
    public :: to_lower_ascii

contains

    pure logical function is_character_type_string(type_str) result(is_character)
        character(len=*), intent(in) :: type_str
        character(len=:), allocatable :: trimmed
        character(len=:), allocatable :: lowered

        trimmed = adjustl(trim(type_str))
        if (len_trim(trimmed) < len("character")) then
            is_character = .false.
            return
        end if

        lowered = to_lower_ascii(trimmed)
        is_character = index(lowered, "character") == 1
    end function is_character_type_string

    pure function to_lower_ascii(text) result(lower_text)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: lower_text
        integer :: i, position
        character(len=*), parameter :: uppercase_letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        character(len=*), parameter :: lowercase_letters = "abcdefghijklmnopqrstuvwxyz"

        lower_text = text
        if (len(text) == 0) return

        do i = 1, len(text)
            position = index(uppercase_letters, text(i:i))
            if (position > 0) lower_text(i:i) = lowercase_letters(position:position)
        end do
    end function to_lower_ascii

end module type_string_utils
