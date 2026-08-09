module abstract_dispatch_runtime_boundary
    implicit none

    type, abstract :: root_t
        integer :: implementation_id = 0
    contains
        procedure :: work => root_work
        procedure(run_interface), deferred, pass(self) :: run
    end type root_t

    type, abstract, extends(root_t) :: middle_t
    end type middle_t

    type, extends(middle_t) :: inherited_leaf_t
    contains
        procedure, pass(self) :: run => inherited_run
    end type inherited_leaf_t

    type, extends(root_t) :: override_leaf_t
    contains
        procedure, pass(self) :: work => override_work
        procedure, pass(self) :: run => override_run
    end type override_leaf_t

    abstract interface
        subroutine run_interface(self)
            import root_t
            class(root_t), intent(inout) :: self
        end subroutine run_interface
    end interface

contains

    subroutine dispatch(value)
        class(root_t), intent(inout) :: value

        select type (value)
            type is (inherited_leaf_t)
                call value%work()
            type is (override_leaf_t)
                call value%work()
            class is (middle_t)
                call value%work()
        end select
    end subroutine dispatch

    subroutine root_work(self)
        class(root_t), intent(inout) :: self

        self%implementation_id = 1
    end subroutine root_work

    subroutine override_work(self)
        class(override_leaf_t), intent(inout) :: self

        self%implementation_id = 2
    end subroutine override_work

    subroutine inherited_run(self)
        class(inherited_leaf_t), intent(inout) :: self
    end subroutine inherited_run

    subroutine override_run(self)
        class(override_leaf_t), intent(inout) :: self
    end subroutine override_run

end module abstract_dispatch_runtime_boundary

program abstract_dispatch_runtime_boundary_oracle
    use abstract_dispatch_runtime_boundary, only: inherited_leaf_t, &
        override_leaf_t, dispatch
    implicit none

    type(inherited_leaf_t) :: inherited
    type(override_leaf_t) :: overridden

    call dispatch(inherited)
    if (inherited%implementation_id /= 1) error stop 1

    call dispatch(overridden)
    if (overridden%implementation_id /= 2) error stop 2
end program abstract_dispatch_runtime_boundary_oracle
