! Demonstrates that internal procedures need their own implicit none
! even when the host has implicit none.
!
! ISO/IEC 1539-1:2018 Section 8.7: Implicit typing rules apply
! independently in each scoping unit.

program test_internal_implicit
    implicit none
    real :: result

    result = compute(5.0)
    print *, result
contains
    ! This internal function needs its own implicit none
    real function compute(x)
        real, intent(in) :: x
        compute = x * 2.0
    end function compute
end program test_internal_implicit
