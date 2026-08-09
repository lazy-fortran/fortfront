module owned_array_class_is_binding_identity
    implicit none

    type, abstract :: base_t
    contains
        procedure(run_interface), deferred, pass(self) :: run
    end type base_t

    type, extends(base_t) :: child_t
    contains
        procedure, pass(self) :: run => child_run
    end type child_t

    type, extends(child_t) :: grandchild_t
    end type grandchild_t

    type, extends(base_t), abstract :: deferred_t
    end type deferred_t

    abstract interface
        subroutine run_interface(self)
            import base_t
            class(base_t), intent(inout) :: self
        end subroutine run_interface
    end interface

    class(base_t), allocatable, save :: global_values(:)

contains

    subroutine accepted_direct()
        class(base_t), allocatable :: values(:)
        type(child_t), allocatable :: seed(:)

        allocate (seed(1))
        call move_alloc(seed, values)
        select type (values)
        class is (child_t)
            call values(1)%run()
        end select
    end subroutine accepted_direct

    subroutine accepted_inherited()
        class(base_t), allocatable :: values(:)
        type(grandchild_t), allocatable :: seed(:)

        allocate (seed(1))
        call move_alloc(seed, values)
        select type (values)
        class is (grandchild_t)
            call values(1)%run()
        end select
    end subroutine accepted_inherited

    subroutine refused_global()
        select type (global_values)
        class is (child_t)
            call global_values(1)%run()
        end select
    end subroutine refused_global

    subroutine refused_alias()
        class(base_t), allocatable, target :: values(:)

        select type (alias => values)
        class is (child_t)
            call alias(1)%run()
        end select
    end subroutine refused_alias

    subroutine refused_control_flow(flag)
        logical, intent(in) :: flag
        class(base_t), allocatable :: values(:)

        select type (values)
        class is (child_t)
            if (flag) call values(1)%run()
        end select
    end subroutine refused_control_flow

    subroutine refused_abstract_guard()
        class(base_t), allocatable :: values(:)

        select type (values)
        class is (deferred_t)
            call values(1)%run()
        end select
    end subroutine refused_abstract_guard

    subroutine child_run(self)
        class(child_t), intent(inout) :: self
    end subroutine child_run

end module owned_array_class_is_binding_identity
