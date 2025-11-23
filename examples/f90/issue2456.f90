program test_char
    implicit none
    call process("Hello")
contains
    subroutine process(input)
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: local_copy
        allocate (character(len=len(input)) :: local_copy)
        local_copy = input
        print *, trim(local_copy)
    end subroutine process
end program test_char
