! Issue #1608: CLASS keyword preserved in subroutine arguments
subroutine create_class(self, n)
    class(atype), intent(inout) :: self
    integer, intent(in) :: n
end subroutine create_class
