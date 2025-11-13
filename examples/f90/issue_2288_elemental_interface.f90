module issue_2288_elemental_interface_mod
    implicit none
    interface
        elemental subroutine scale_value(x)
            real, intent(in) :: x
        end subroutine scale_value
    end interface
end module issue_2288_elemental_interface_mod
