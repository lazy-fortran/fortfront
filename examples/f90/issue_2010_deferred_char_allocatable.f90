program test_allocatable_character
    implicit none
    character(len=:), allocatable :: str
    integer :: n

    n = 15
    allocate(character(len=n) :: str)
    str = 'Hello, World!'
    print *, 'String: ', str
    print *, 'Length: ', len(str)
    deallocate(str)
end program test_allocatable_character
