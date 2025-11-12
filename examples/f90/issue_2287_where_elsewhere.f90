program issue_2287_where_elsewhere
    implicit none
    integer :: b(2)

    b = (/1, 0/)
    where (b == 0)
        if (any(b == 0)) then
            stop 1
        end if
    elsewhere
        b = 2
    end where
end program issue_2287_where_elsewhere
