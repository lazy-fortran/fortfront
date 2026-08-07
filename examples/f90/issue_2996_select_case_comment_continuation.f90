program continuation_comment
    implicit none
    integer :: value

    value = 2
    select case (value)
    case (1, &
        ! a comment between continuation lines
        2)
        print *, 'PASS'
    case default
        error stop 1
    end select
end program continuation_comment
