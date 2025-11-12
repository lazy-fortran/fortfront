program issue_2275_identifier_exit
    implicit none
contains
    subroutine track_exit_value()
        implicit none
        integer :: exit

        exit = 1
        print *, exit
    end subroutine track_exit_value
end program issue_2275_identifier_exit
