module issue_2289_impure_interface
    implicit none

    interface
        impure subroutine log_state(value)
            integer, intent(in) :: value
        end subroutine log_state
    end interface
end module issue_2289_impure_interface
