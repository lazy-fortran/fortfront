module owned_array_class_is_dynamic_identity
    implicit none

    type, abstract :: base_t
        integer :: value
    contains
        procedure(run_interface), deferred :: run
    end type base_t

    type, extends(base_t) :: child_t
    contains
        procedure :: run => child_run
    end type child_t

    abstract interface
        subroutine run_interface(self)
            import base_t
            class(base_t), intent(inout) :: self
        end subroutine run_interface
    end interface

    class(base_t), allocatable, save :: global_items(:)

contains

    subroutine accepted_owned_array()
        class(base_t), allocatable :: items(:)
        type(child_t), allocatable :: seed(:)

        allocate (seed(2))
        call move_alloc(seed, items)
        select type (items)
        class is (child_t)
            items(1)%value = 7
        end select
    end subroutine accepted_owned_array

    subroutine refused_global_array()
        select type (global_items)
        class is (child_t)
            global_items(1)%value = 8
        end select
    end subroutine refused_global_array

    subroutine refused_alias_array()
        class(base_t), allocatable, target :: items(:)

        select type (alias => items)
        class is (child_t)
            alias(1)%value = 9
        end select
    end subroutine refused_alias_array

    subroutine refused_control_flow(flag)
        logical, intent(in) :: flag
        class(base_t), allocatable :: items(:)

        select type (items)
        class is (child_t)
            if (flag) then
                items(1)%value = 10
            end if
        end select
    end subroutine refused_control_flow

    subroutine child_run(self)
        class(child_t), intent(inout) :: self
    end subroutine child_run

end module owned_array_class_is_dynamic_identity
