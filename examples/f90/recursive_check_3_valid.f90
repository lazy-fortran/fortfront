program recursive_check_3_valid
    ! VALID neighbour of recursive_check_3.f90. Each prefix keyword appears at
    ! most once, and distinct keywords may still be combined.
    implicit none

contains

    pure subroutine a1(b)
        real, intent(in) :: b
    end subroutine a1

    pure elemental subroutine a2(b, c)
        real, intent(in) :: b
        real, intent(out) :: c
        c = b
    end subroutine a2

    recursive subroutine a3(n)
        integer, intent(in) :: n
        if (n > 0) call a3(n - 1)
    end subroutine a3

end program recursive_check_3_valid
