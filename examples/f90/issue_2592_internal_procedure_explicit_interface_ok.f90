program issue_2592_internal_procedure_explicit_interface_ok
    implicit none

    integer :: x

    x = 1
    call internal_sub(x)

contains

    subroutine internal_sub(a)
        implicit none

        integer, intent(in) :: a

        print *, a
    end subroutine internal_sub

end program issue_2592_internal_procedure_explicit_interface_ok

