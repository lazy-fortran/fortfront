program issue_2273_identifier_stop
    implicit none
contains
    subroutine track_stop()
        implicit none
        integer :: stop
        stop = 1
        print *, stop
    end subroutine track_stop
end program issue_2273_identifier_stop
