program test_optional_present
    implicit none

    call greet('Alice')
    call greet('Bob', 'Good morning')

contains

    subroutine greet(name, message)
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: message

        if (present(message)) then
            print *, trim(message), ', ', trim(name), '!'
        else
            print *, 'Hello, ', trim(name), '!'
        end if
    end subroutine greet

end program test_optional_present
