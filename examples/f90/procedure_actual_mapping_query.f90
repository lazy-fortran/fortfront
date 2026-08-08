module procedure_actual_mapping_query_example
    implicit none
contains

    subroutine apply(operation, value)
        procedure(real) :: operation
        real, intent(inout) :: value

        value = operation(value)
    end subroutine apply

    subroutine direct_case(value)
        real, intent(inout) :: value

        call apply(increment, value)
    end subroutine direct_case

    subroutine pointer_context_case(value)
        real, intent(inout) :: value
        procedure(real), pointer :: callback

        callback => increment
        call apply(callback, value)
    end subroutine pointer_context_case

    subroutine null_pointer_case(value)
        real, intent(inout) :: value
        procedure(real), pointer :: callback

        callback => null()
        call apply(callback, value)
    end subroutine null_pointer_case

    subroutine reassigned_case(value)
        real, intent(inout) :: value
        procedure(real), pointer :: callback

        callback => increment
        callback => decrement
        call apply(callback, value)
    end subroutine reassigned_case

    subroutine branched_case(value, choose_increment)
        real, intent(inout) :: value
        logical, intent(in) :: choose_increment
        procedure(real), pointer :: callback

        if (choose_increment) then
            callback => increment
        else
            callback => decrement
        end if
        call apply(callback, value)
    end subroutine branched_case

    real function increment(x)
        real, intent(in) :: x

        increment = x + 1.0
    end function increment

    real function decrement(x)
        real, intent(in) :: x

        decrement = x - 1.0
    end function decrement

end module procedure_actual_mapping_query_example
