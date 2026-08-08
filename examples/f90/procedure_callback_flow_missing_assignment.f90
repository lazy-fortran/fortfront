module procedure_callback_flow_missing_assignment
implicit none
contains

    subroutine left_target(x)
        real, intent(in) :: x
    end subroutine left_target

    subroutine right_target(x)
        real, intent(in) :: x
    end subroutine right_target

    subroutine kernel(x, choose_left)
        real, intent(in) :: x
        logical, intent(in) :: choose_left
        procedure(left_target), pointer :: callback
        real :: scratch

        if (choose_left) then
            scratch = x
        else
            callback => right_target
        end if
        call callback(x)
    end subroutine kernel

end module procedure_callback_flow_missing_assignment
