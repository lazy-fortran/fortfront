module procedure_actual_interface_mismatch_example
implicit none

abstract interface
    real(8) function callback_iface(value)
        real(8), intent(in) :: value
    end function callback_iface
end interface

contains

    subroutine apply(operation, value)
        procedure(callback_iface) :: operation
        real(8), intent(inout) :: value

        value = operation(value)
    end subroutine apply

    subroutine mismatch_case(value)
        real(8), intent(inout) :: value

        call apply(wrong_kind, value)
    end subroutine mismatch_case

    real(4) function wrong_kind(value)
        real(4), intent(in) :: value

        wrong_kind = value + 1.0
    end function wrong_kind

end module procedure_actual_interface_mismatch_example
