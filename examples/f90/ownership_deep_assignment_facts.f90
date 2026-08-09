module ownership_deep_assignment_facts
    implicit none

    type :: payload_t
        integer, allocatable :: values(:)
    end type payload_t

    type :: wrapper_t
        type(payload_t) :: payload
    end type wrapper_t

    type(wrapper_t), save :: shared
contains

    subroutine copy_local(lhs, rhs)
        type(wrapper_t), intent(inout) :: lhs
        type(wrapper_t), intent(in) :: rhs

        lhs = rhs
    end subroutine copy_local

    subroutine copy_global(rhs)
        type(wrapper_t), intent(in) :: rhs

        shared = rhs
    end subroutine copy_global

    subroutine copy_target(alias_lhs, rhs)
        type(wrapper_t), target, intent(inout) :: alias_lhs
        type(wrapper_t), intent(in) :: rhs

        alias_lhs = rhs
    end subroutine copy_target

end module ownership_deep_assignment_facts
