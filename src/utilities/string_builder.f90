module string_builder_mod
    implicit none
    private

    public :: join_strings

contains

    pure function join_strings(fragments, separator) result(joined)
        character(len=*), intent(in) :: fragments(:)
        character(len=*), intent(in) :: separator
        character(len=:), allocatable :: joined
        integer :: total_len
        integer :: i
        integer :: pos
        integer :: frag_len
        integer :: sep_len
        integer :: n_fragments

        n_fragments = size(fragments)
        if (n_fragments == 0) then
            joined = ""
            return
        end if

        sep_len = len(separator)
        total_len = 0
        do i = 1, n_fragments
            total_len = total_len + len_trim(fragments(i))
        end do
        total_len = total_len + (n_fragments - 1) * sep_len

        allocate (character(len=total_len) :: joined)

        pos = 1
        do i = 1, n_fragments
            frag_len = len_trim(fragments(i))
            if (frag_len > 0) then
                joined(pos:pos + frag_len - 1) = trim(fragments(i))
                pos = pos + frag_len
            end if
            if (i < n_fragments) then
                if (sep_len > 0) then
                    joined(pos:pos + sep_len - 1) = separator
                    pos = pos + sep_len
                end if
            end if
        end do
    end function join_strings

end module string_builder_mod
