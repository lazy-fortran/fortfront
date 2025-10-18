module type_string_utils
    use string_utils_mod, only: to_lower
    implicit none
    private

    public :: is_character_type_string

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

        lowered = to_lower(trimmed)
        is_character = index(lowered, "character") == 1
    end function is_character_type_string

end module type_string_utils
