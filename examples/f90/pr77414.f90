! Negative fixture for issue #2888 (reject-scope-02), after gfortran.dg.
! A contained procedure may not repeat the name of the procedure that
! contains it.
subroutine pr77414_outer(x)
    implicit none
    character(len=*), intent(in) :: x

    print *, len(x)
contains
    subroutine pr77414_outer(y)
        implicit none
        character(len=*), intent(in) :: y

        print *, len(y)
    end subroutine pr77414_outer
end subroutine pr77414_outer
