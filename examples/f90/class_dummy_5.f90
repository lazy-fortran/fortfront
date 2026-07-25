program class_dummy_5
    ! INVALID: F2008 C1279. In a PURE subprogram a dummy argument with the
    ! INTENT(OUT) attribute shall not be polymorphic, because finalization of
    ! the actual argument could call an impure final subroutine.
    implicit none

    type :: t
        integer :: i = 0
    end type t

    type(t) :: x

    call foo(x)

contains

    pure subroutine foo(y)
        class(t), intent(out) :: y
        y%i = 0
    end subroutine foo

end program class_dummy_5
