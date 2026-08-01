! Corrected neighbour of operator_6.f90 (issue #2887).
module foo
    implicit none
    interface operator(.none.)
        module procedure none_of
    end interface
contains
    logical function none_of(a) result(c)
        logical, intent(in) :: a
        c = .not. a
    end function none_of
end module foo

program test
    use foo, only: operator(.none.)
    implicit none
    print *, .none. .true.
end program test
