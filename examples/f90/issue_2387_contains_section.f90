program test_contains
    implicit none
    integer :: x

    x = 42
    call test_sub()

contains

    subroutine test_sub()
        implicit none
        print *, "Hello from contains section"
    end subroutine test_sub

end program test_contains