program test_subroutine
    implicit none
    integer :: x
    x = 5
    call increment(x)
    print *, x
contains
    subroutine increment(val)
        integer, intent(inout) :: val
        val = val + 1
    end subroutine
end program
