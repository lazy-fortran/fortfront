! Corrected neighbour of pr77414.f90 (issue #2888).
! The contained procedure carries its own name.
subroutine pr77414_corrected_outer(x)
    implicit none
    character(len=*), intent(in) :: x

    call report(x)
contains
    subroutine report(y)
        implicit none
        character(len=*), intent(in) :: y

        print *, len(y)
    end subroutine report
end subroutine pr77414_corrected_outer
