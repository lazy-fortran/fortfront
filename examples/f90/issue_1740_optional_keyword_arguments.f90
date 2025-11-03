program test_optional_present
    implicit none
    call greet('Alice')
    call greet('Bob', 'Hello')
    call greet('Charlie', greeting='Hi')
contains
    subroutine greet(name, greeting)
        character(len=*), intent(in) :: name
        character(len=*), optional, intent(in) :: greeting
        if (present(greeting)) then
            print *, greeting, name
        else
            print *, 'Good day', name
        end if
    end subroutine greet
end program test_optional_present
