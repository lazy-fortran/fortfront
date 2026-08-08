module abstract_binding_hierarchy_facts
    implicit none

    type, abstract :: root_t
    contains
        procedure(operation_interface), deferred :: operate
    end type root_t

    type, abstract, extends(root_t) :: middle_t
    contains
        procedure, nopass :: helper => middle_helper
    end type middle_t

    type, extends(middle_t) :: leaf_t
    contains
        procedure(operation_interface), pass(state) :: operate => leaf_operate
    end type leaf_t

    type :: generic_t
    contains
        generic :: choose => choose_left, choose_right
    end type generic_t

    abstract interface
        subroutine operation_interface(self, scale)
            import root_t
            class(root_t), intent(inout) :: self
            real, intent(in) :: scale
        end subroutine operation_interface
    end interface

contains

    subroutine middle_helper()
    end subroutine middle_helper

    subroutine leaf_operate(state, scale)
        class(leaf_t), intent(inout) :: state
        real, intent(in) :: scale
    end subroutine leaf_operate

    subroutine choose_left()
    end subroutine choose_left

    subroutine choose_right()
    end subroutine choose_right

end module abstract_binding_hierarchy_facts
