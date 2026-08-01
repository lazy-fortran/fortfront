! Corrected neighbour of use_19.f90 (issue #2887).
! The module extends the intrinsic operator, so importing it is legal.
module m
    implicit none
    interface operator(/)
        module procedure divide_flags
    end interface
contains
    logical function divide_flags(a, b) result(c)
        logical, intent(in) :: a, b
        c = a .and. .not. b
    end function divide_flags
end module m

program main
    use m, only: operator(/)
    implicit none
    print *, .true./.false.
end program main
