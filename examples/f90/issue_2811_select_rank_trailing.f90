! Issue #2811: a statement after end select must be a sibling of the
! select rank construct, not absorbed into the rank default arm body.
program issue_2811_select_rank_trailing
    implicit none
    integer :: a1(3)

    a1 = 0
    call probe(a1)

contains

    subroutine probe(arr)
        integer :: arr(..)
        integer :: i

        i = 0
        select rank (arr)
        rank (1)
            i = 1
        rank default
            i = 2
        end select
        print *, i
    end subroutine probe

end program issue_2811_select_rank_trailing
