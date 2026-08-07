program continuation_plain
    implicit none
    integer :: value

    value = 2
    select case (value)
    case (1, &
        2)
        print *, 'PASS'
    case default
        error stop 1
    end select
end program continuation_plain
