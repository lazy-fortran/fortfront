module optional_mod
    implicit none
contains
    subroutine greet(name, title)
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: title
        if (present(title)) then
            print *, title, ' ', name
        else
            print *, name
        end if
    end subroutine greet
end module optional_mod
