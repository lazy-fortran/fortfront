program procedure_reassignment_call_query
    implicit none
    procedure(real(kind=8)), pointer :: callback
    real(kind=8) :: value

    callback => first_target
    callback => second_target
    value = callback(3.0d0)

contains

    real(kind=8) function first_target(x)
        real(kind=8), intent(in) :: x
        first_target = 2.0d0 * x
    end function first_target

    real(kind=8) function second_target(x)
        real(kind=8), intent(in) :: x
        second_target = 3.0d0 * x + 1.0d0
    end function second_target

end program procedure_reassignment_call_query
