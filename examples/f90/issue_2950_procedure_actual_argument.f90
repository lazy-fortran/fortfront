program issue_2950_procedure_actual_argument
    implicit none
    intrinsic dcos
    call apply(dcos)
    call apply(expression)
contains

    subroutine apply(f)
        interface
            function f(x)
                real(kind=8) :: f
                real(kind=8), intent(in) :: x
            end function f
        end interface
        real(kind=8) :: value
        value = f(1.0d0)
    end subroutine apply

    function expression(x) result(y)
        real(kind=8), intent(in) :: x
        real(kind=8) :: y
        y = x
    end function expression

end program issue_2950_procedure_actual_argument
