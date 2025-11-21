! Demonstrates that module procedures in CONTAINS sections need
! their own implicit none even when the module has implicit none.
!
! ISO/IEC 1539-1:2018 Section 8.7: Implicit typing rules apply
! independently in each scoping unit.

module math_ops
    implicit none
contains
    ! Module procedure needs its own implicit none
    real function square(x)
        real, intent(in) :: x
        square = x * x
    end function square

    subroutine scale(x, factor)
        real, intent(inout) :: x
        real, intent(in) :: factor
        x = x * factor
    end subroutine scale
end module math_ops

program test_module_implicit
    use math_ops
    implicit none
    real :: val

    val = 3.0
    val = square(val)
    call scale(val, 2.0)
    print *, val
end program test_module_implicit
