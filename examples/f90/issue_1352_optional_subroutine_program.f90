subroutine greet(name, title)
    character(len=*), intent(in) :: name
    character(len=*), intent(in), optional :: title
    if (present(title)) then
        print *, trim(title), ' ', trim(name)
    else
        print *, trim(name)
    end if
end subroutine greet

program test_optional
    implicit none
    call greet('Alice')
    call greet('Bob', 'Dr.')
end program test_optional
