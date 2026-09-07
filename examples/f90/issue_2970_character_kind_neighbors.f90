subroutine character_kind_neighbors(intrinsic_text, table_text, literal_text, &
                                    inquiry_text)
    interface
        character function choose_char()
        end function choose_char
    end interface
    integer, parameter :: kinds(2) = [kind('a'), kind('a')]
    character(kind=kind('a'), len=4) :: intrinsic_text
    character(len=4, kind=kinds(2)) :: table_text
    character(kind=1, len=4) :: literal_text
    character(kind=kind(choose_char())) :: inquiry_text
end subroutine character_kind_neighbors
