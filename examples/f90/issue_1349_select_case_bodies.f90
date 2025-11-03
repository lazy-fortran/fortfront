program test_select_case
    implicit none
    integer :: i

    do i = 1, 5
        select case (i)
        case (1)
            print *, 'One'
        case (2:3)
            print *, 'Two or Three'
        case (4)
            print *, 'Four'
        case (5, 6, 7)
            print *, 'FiveSixSeven'
        case default
            print *, 'Other'
        end select
    end do

end program test_select_case
