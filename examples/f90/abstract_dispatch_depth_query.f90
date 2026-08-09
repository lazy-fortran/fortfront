module abstract_dispatch_depth_query
    implicit none

    type, abstract :: root_t
    contains
        procedure(operation_interface), deferred :: operate
    end type root_t

    type, abstract, extends(root_t) :: middle_t
    contains
        procedure, nopass :: helper => middle_helper
    end type middle_t

    type, abstract, extends(middle_t) :: late_t
    contains
        procedure, pass(self) :: operate => late_operate
    end type late_t

    type, extends(late_t) :: leaf_t
    end type leaf_t

    type, extends(leaf_t) :: deep_leaf_t
    end type deep_leaf_t

    type, extends(root_t) :: local_t
    contains
        procedure, pass(self) :: operate => local_operate
    end type local_t

    type :: generic_t
    contains
        procedure, nopass :: choose_left
        procedure, nopass :: choose_right
        generic :: choose => choose_left, choose_right
    end type generic_t

    abstract interface
        subroutine operation_interface(self)
            import root_t
            class(root_t), intent(inout) :: self
        end subroutine operation_interface
    end interface

contains

    subroutine invoke(self)
        class(root_t), intent(inout) :: self

        call self%operate()
    end subroutine invoke

    subroutine middle_helper()
    end subroutine middle_helper

    subroutine late_operate(self)
        class(late_t), intent(inout) :: self
    end subroutine late_operate

    subroutine local_operate(self)
        class(local_t), intent(inout) :: self
    end subroutine local_operate

    subroutine choose_left(value)
        integer, intent(in) :: value
    end subroutine choose_left

    subroutine choose_right(value)
        real, intent(in) :: value
    end subroutine choose_right

end module abstract_dispatch_depth_query
