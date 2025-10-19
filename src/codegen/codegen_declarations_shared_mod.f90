module codegen_declarations_shared_mod
    implicit none
    private
    public :: fix_character_len_placeholder

contains

    pure function fix_character_len_placeholder(text) result(out)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: out
        integer :: pos

        out = text

        pos = index(out, "len=))")
        do while (pos > 0)
            out = out(:pos - 1) // "len=*" // out(pos + 5:)
            pos = index(out, "len=))")
        end do

        pos = index(out, "len=)")
        do while (pos > 0)
            out = out(:pos - 1) // "len=*" // out(pos + 4:)
            pos = index(out, "len=)")
        end do
    end function fix_character_len_placeholder
end module codegen_declarations_shared_mod
