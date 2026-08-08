program procedure_callback_flow
    implicit none
    procedure(real), pointer :: callback
    logical :: choose_left
    real :: value

    if (choose_left) then
        callback => left_target
    else
        callback => right_target
    end if
    call callback(value)

contains

    real function left_target(x)
        real, intent(in) :: x
        left_target = x + 1.0
    end function left_target

    real function right_target(x)
        real, intent(in) :: x
        right_target = x - 1.0
    end function right_target

end program procedure_callback_flow
