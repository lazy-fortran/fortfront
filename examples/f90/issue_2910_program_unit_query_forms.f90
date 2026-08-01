module m
    use dep, only: local => remote
    private
    integer, parameter :: n = 2
    type :: pair
        integer :: x
    contains
        procedure :: show
    end type pair
contains
    subroutine show(self)
        type(pair) :: self
    end subroutine show
end module m
program p
    use m
end program p
