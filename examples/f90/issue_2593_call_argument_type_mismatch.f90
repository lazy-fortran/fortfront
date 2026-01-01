program issue_2593_call_argument_type_mismatch
    implicit none

    real(4) :: r4
    real(8) :: y

    r4 = 1.0
    call process(r4)
    y = f(r4)

contains

    subroutine process(x)
        real(8), intent(in) :: x
    end subroutine process

    real(8) function f(x)
        real(8), intent(in) :: x
        f = x
    end function f

end program issue_2593_call_argument_type_mismatch
