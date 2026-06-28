program test
    integer :: x = 2
    select case (x)
    case (1)
        print *, "one"
    case (2)
        print *, "two"
    case default
        print *, "other"
    end select
end program test
