! Test SELECT CASE with open-ended ranges (issue #1692)
program test_select_case
    implicit none
    integer :: grade, i
    character(len=10) :: category

    do i = 1, 5
        select case (i * 20)
        case (0:50)
            category = 'F'
        case (51:70)
            category = 'D'
        case (71:85)
            category = 'C'
        case (86:95)
            category = 'B'
        case (96:)
            category = 'A'
        case default
            category = 'Unknown'
        end select
        print *, 'Score:', i*20, ' Grade:', category
    end do
end program test_select_case
