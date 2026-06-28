program issue_2562_where_invalid_if
    ! This example demonstrates INVALID Fortran per F2018 Section 10.2.3.2
    ! WHERE/ELSEWHERE body may only contain:
    !   - Array assignment statements
    !   - Nested WHERE statements
    !   - Nested WHERE constructs
    ! This example intentionally contains an IF construct inside WHERE
    ! which should be rejected by semantic validation
    implicit none
    integer :: b(2)

    b = (/1, 0/)
    where (b == 0)
        if (any(b == 0)) then ! INVALID: IF construct not allowed in WHERE
            stop 1 ! INVALID: STOP statement not allowed in WHERE
        end if
    elsewhere
        b = 2 ! Valid: array assignment
    end where
end program issue_2562_where_invalid_if
