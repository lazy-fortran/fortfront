module procedure_callback_flow_loop
    implicit none
contains

    subroutine left_target(x, y)
        real, intent(in) :: x
        real, intent(out) :: y
        y = x + 1.0
    end subroutine left_target

    subroutine right_target(x, y)
        real, intent(in) :: x
        real, intent(out) :: y
        y = x - 1.0
    end subroutine right_target

    subroutine kernel(x, choose_left, value)
        real, intent(in) :: x
        logical, intent(in) :: choose_left
        real, intent(out) :: value
        procedure(left_target), pointer :: callback
        integer :: i

        if (choose_left) then
            callback => left_target
            do i = 1, 1
                value = x
            end do
        else
            callback => right_target
        end if
        call callback(x, value)
    end subroutine kernel

    subroutine kernel_while(x, choose_left, value)
        real, intent(in) :: x
        logical, intent(in) :: choose_left
        real, intent(out) :: value
        procedure(left_target), pointer :: callback
        integer :: i

        if (choose_left) then
            callback => left_target
        else
            callback => right_target
            i = 0
            do while (i < 1)
                i = i + 1
            end do
        end if
        call callback(x, value)
    end subroutine kernel_while

end module procedure_callback_flow_loop
