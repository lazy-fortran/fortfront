module procedure_actual_interface_query_example
implicit none

abstract interface
    real(8) function callback_iface(value)
        real(8), intent(in) :: value
    end function callback_iface
end interface

contains

    subroutine apply(operation, value)
        procedure(callback_iface), optional :: operation
        real(8), intent(inout) :: value

        if (present(operation)) then
            value = operation(value)
        end if
    end subroutine apply

    subroutine direct_case(value)
        real(8), intent(inout) :: value

        call apply(operation=increment, value=value)
    end subroutine direct_case

    subroutine pointer_case(value)
        real(8), intent(inout) :: value
        procedure(callback_iface), pointer :: callback

        callback => increment
        call apply(value=value, operation=callback)
    end subroutine pointer_case

    real(8) function increment(value)
        real(8), intent(in) :: value

        increment = value + 1.0d0
    end function increment

end module procedure_actual_interface_query_example

program procedure_actual_interface_query_runtime
    use procedure_actual_interface_query_example, only: pointer_case
    implicit none
    real(8) :: value

    value = 2.0d0
    call pointer_case(value)
    if (abs(value - 3.0d0) > 1.0d-12) error stop 1
    print *, 'PASS: procedure actual/formal interface runtime'
end program procedure_actual_interface_query_runtime
