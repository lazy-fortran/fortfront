module call_argument_contract_example
    implicit none
    real(8) :: shared_state

contains

    subroutine update(value, scale)
        real(8), intent(inout) :: value
        real(8), intent(in) :: scale

        value = value * scale
    end subroutine update

    subroutine update_global(value)
        real(8), intent(inout) :: value

        value = value + shared_state
    end subroutine update_global

    subroutine update_alias(left, right)
        real(8), intent(inout) :: left
        real(8), intent(inout) :: right

        left = left + right
    end subroutine update_alias

    subroutine apply_callback(callback, value)
        procedure(real), pointer :: callback
        real(8), intent(inout) :: value

        value = callback(value)
    end subroutine apply_callback

end module call_argument_contract_example

program call_argument_contract_driver
    use call_argument_contract_example, only: update, update_global, &
        update_alias, apply_callback
    implicit none
    real(8) :: value
    procedure(real), pointer :: callback

    value = 1.0d0
    call update(value, 2.0d0)
    call update_global(value)
    call update_alias(value, value)
    call apply_callback(callback, value)
end program call_argument_contract_driver
