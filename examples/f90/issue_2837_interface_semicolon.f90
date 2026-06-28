module issue_2837_interface_semicolon
    implicit none
    interface get_args;  module procedure get_scalar_i;  end interface
    contains
        subroutine get_scalar_i()
        end subroutine get_scalar_i
    end module issue_2837_interface_semicolon
