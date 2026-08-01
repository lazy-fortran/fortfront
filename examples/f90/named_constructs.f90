! Named constructs: every construct that may carry a construct name, plus
! EXIT and CYCLE referring to an enclosing construct by name.
program named_constructs
    implicit none
    integer :: i, j, total

    total = 0

    outer: do i = 1, 3
        inner: do j = 1, 3
            if (j == 2) cycle outer
            if (j == 3) exit outer
            total = total + j
        end do inner
    end do outer

    check: if (total > 0) then
        total = total + 1
    else
        total = -1
    end if check

    pick: select case (total)
    case (1)
        total = 2
    case default
        total = 3
    end select pick

    scope: block
        integer :: k
        k = 5
        total = total + k
    end block scope

    link: associate (m => total)
        m = m + 1
    end associate link

    print *, total
end program named_constructs
