program test
    integer :: x = 2, y = 3
    select case (x)
    case (1)
        print *, "x is one"
    case (2)
        select case (y)
        case (3)
            print *, "x is two, y is three"
        end select
    end select
end program test
