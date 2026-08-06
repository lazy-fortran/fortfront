program issue_669_nested_character_substring
    character(len=6) :: c(2)
    character(len=3) :: first

    c(1) = 'abcdef'
    c(2) = 'ghijkl'
    first = c(2)(1:3)
    c(2)(1:3) = c(2)(2:4)
    call consume(c(2)(1:3))

contains

    subroutine consume(value)
        character(len=*), intent(in) :: value
        print *, value
    end subroutine consume

end program issue_669_nested_character_substring
