program issue_2287_where_elsewhere
    ! Demonstrates valid WHERE/ELSEWHERE construct per F2018 Section 10.2.3.2
    ! WHERE body may only contain: array assignments, nested WHERE, nested WHERE stmt
    implicit none
    integer :: a(4), b(4), c(4)

    a = (/1, 2, 3, 4/)
    b = (/0, 1, 0, 1/)
    c = 0

    ! Valid WHERE/ELSEWHERE with array assignments only
    where (b == 0)
        c = a * 10 ! Valid: array assignment
    elsewhere
        c = a ! Valid: array assignment
    end where

    ! Valid nested WHERE construct
    where (a > 2)
        where (b == 1)
            c = 99 ! Valid: nested WHERE with array assignment
        end where
    end where

    print *, 'c =', c
end program issue_2287_where_elsewhere
