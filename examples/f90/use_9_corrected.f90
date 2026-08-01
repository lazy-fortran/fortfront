! Corrected neighbour of use_9.f90 (issue #2887).
! The ONLY clause names the operator the module really exports.
module test
    implicit none
    interface operator(.bar.)
        module procedure func
    end interface
contains
    integer function func(a)
        integer, intent(in) :: a
        func = a + 1
    end function func
end module test

program main
    use test, only: operator(.bar.)
    implicit none
    print *, .bar. 1
end program main
