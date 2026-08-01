! Invalid: a derived-type definition belongs to the specification part and
! cannot follow executable statements. Reduced from gfortran.dg/pdt_33.f90.
program pdt_33
    implicit none
    integer :: a
    a = 1
    type :: t
        integer :: k
    end type t
end program pdt_33
