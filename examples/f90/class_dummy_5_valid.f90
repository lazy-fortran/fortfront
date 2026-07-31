program class_dummy_5_valid
    ! VALID neighbour of class_dummy_5.f90. A polymorphic dummy argument is
    ! allowed in a PURE procedure as long as it is not INTENT(OUT), and an
    ! INTENT(OUT) polymorphic dummy is allowed in an impure procedure.
    implicit none

    type :: t
        integer :: i = 0
    end type t

    type(t) :: x

    call keep(x)
    call reset(x)

contains

    pure subroutine keep(y)
        class(t), intent(inout) :: y
        y%i = y%i + 1
    end subroutine keep

    subroutine reset(y)
        class(t), intent(out) :: y
        y%i = 0
    end subroutine reset

end program class_dummy_5_valid
