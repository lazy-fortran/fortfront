program issue_1958_intent_preservation
    implicit none
contains

    function add_int(a, b) result(c)
        integer, intent(in) :: a, b
        integer :: c
        c = a + b
    end function add_int

    subroutine apply_flag(flag, value)
        logical, intent(in), optional :: flag
        integer, intent(out) :: value

        if (present(flag)) then
            if (flag) then
                value = 1
            else
                value = 0
            end if
        else
            value = -1
        end if
    end subroutine apply_flag

end program issue_1958_intent_preservation
