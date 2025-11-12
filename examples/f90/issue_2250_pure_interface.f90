module issue_2250_pure_interface_mod
    implicit none
    interface
        pure function double(text)
            character(len=*), intent(in) :: text
            character(len=len(text)*2) :: double
        end function double

        pure subroutine consume_value(length)
            integer, intent(in) :: length
        end subroutine consume_value
    end interface
contains
    subroutine invoke_callback(callback, value)
        procedure(double) :: callback
        character(len=*), intent(in) :: value
        character(len=:), allocatable :: doubled

        doubled = callback(value)
        call consume_value(len(doubled))
    end subroutine invoke_callback
end module issue_2250_pure_interface_mod
