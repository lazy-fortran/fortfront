program test_return
    implicit none

    call test_sub(5)
    call test_sub(15)

contains
    subroutine test_sub(x)
        integer, intent(in) :: x
        print *, 'Start, x=', x
        if (x > 10) return
        print *, 'After check, x=', x
    end subroutine test_sub
end program test_return
