! Invalid: an interface body has no executable part, so a statement function
! cannot appear within it. Reduced from gfortran.dg/pr68319.f90.
subroutine pr68319()
    implicit none
    interface
        real function bar(i)
            integer :: i
            f(i) = 2*i
        end function bar
    end interface
end subroutine pr68319
