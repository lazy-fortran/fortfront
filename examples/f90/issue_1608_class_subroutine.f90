! Issue #1608: CLASS keyword preserved in subroutine arguments
module atype_mod
    implicit none
    type :: atype
        integer :: value
    end type atype
contains
    subroutine create_class(self, n)
        implicit none
        class(atype), intent(inout) :: self
        integer, intent(in) :: n
        self%value = n
    end subroutine create_class
end module atype_mod
