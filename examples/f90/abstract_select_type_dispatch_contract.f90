module abstract_select_type_dispatch_contract
    implicit none

    type, abstract :: root_t
    contains
        procedure :: work => root_work
        procedure(run_interface), deferred, pass(self) :: run
    end type root_t

    type, abstract, extends(root_t) :: middle_t
    contains
        procedure :: work => middle_work
    end type middle_t

    type, extends(middle_t) :: leaf_t
    contains
        procedure, pass(self) :: run => leaf_run
    end type leaf_t

    abstract interface
        subroutine run_interface(self)
            import root_t
            class(root_t), intent(inout) :: self
        end subroutine run_interface
    end interface

contains

    subroutine dispatch_class(object)
        class(root_t), intent(inout) :: object

        select type (object)
            class is (leaf_t)
                call object%work()
        end select
    end subroutine dispatch_class

    subroutine dispatch_type(object)
        class(root_t), intent(inout) :: object

        select type (object)
            type is (leaf_t)
                call object%work()
        end select
    end subroutine dispatch_type

    subroutine root_work(self)
        class(root_t), intent(inout) :: self
    end subroutine root_work

    subroutine middle_work(self)
        class(middle_t), intent(inout) :: self
    end subroutine middle_work

    subroutine leaf_run(self)
        class(leaf_t), intent(inout) :: self
    end subroutine leaf_run

end module abstract_select_type_dispatch_contract
