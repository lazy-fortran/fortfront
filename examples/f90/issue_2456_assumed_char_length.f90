! ISO/IEC 1539-1:2018 Section 7.4.4.4:
! Assumed-length character (len=*) only valid for:
! - Dummy arguments (procedure parameters)
! - Named constants (PARAMETER)
! NOT valid for local variables

program test_assumed_length_valid
    implicit none

    call process_string("Hello World")

contains
    subroutine process_string(input)
        ! VALID: len=* for dummy argument
        character(len=*), intent(in) :: input

        ! Local variable must use allocatable or explicit length
        character(len=:), allocatable :: local_copy

        allocate (character(len=len(input)) :: local_copy)
        local_copy = input
        print *, trim(local_copy)
    end subroutine process_string
end program test_assumed_length_valid
