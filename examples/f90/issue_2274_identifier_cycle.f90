program issue_2274_identifier_cycle
    implicit none
contains
    subroutine track_cycle_value()
        implicit none
        integer :: cycle

        cycle = 1
        print *, cycle
    end subroutine track_cycle_value
end program issue_2274_identifier_cycle
