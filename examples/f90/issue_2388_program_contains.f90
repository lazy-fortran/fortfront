program issue_2388_program_contains
    implicit none
    integer :: counter

contains

    subroutine bump_counter()
        counter = counter + 1
    end subroutine bump_counter

end program issue_2388_program_contains
