module owned_array_class_is_explicit_pass
    implicit none

    type, abstract :: base_t
    contains
        procedure, pass(self) :: choose_int
        generic :: choose => choose_int
    end type base_t

    type, extends(base_t) :: child_t
    end type child_t

    class(base_t), allocatable, save :: global_values(:)

contains

    subroutine accepted_explicit_pass()
        class(base_t), allocatable :: values(:)
        type(child_t), allocatable :: seed(:)
        integer :: value

        allocate (seed(1))
        call move_alloc(seed, values)
        value = 3
        select type (values)
        class is (child_t)
            call values(1)%choose(value)
        end select
    end subroutine accepted_explicit_pass

    subroutine refused_global(value)
        integer, intent(in) :: value

        select type (global_values)
        class is (child_t)
            call global_values(1)%choose(value)
        end select
    end subroutine refused_global

    subroutine refused_alias(value)
        integer, intent(in) :: value
        class(base_t), allocatable, target :: values(:)

        select type (alias => values)
        class is (child_t)
            call alias(1)%choose(value)
        end select
    end subroutine refused_alias

    subroutine refused_control_flow(flag, value)
        logical, intent(in) :: flag
        integer, intent(in) :: value
        class(base_t), allocatable :: values(:)

        select type (values)
        class is (child_t)
            if (flag) call values(1)%choose(value)
        end select
    end subroutine refused_control_flow

    subroutine choose_int(value, self)
        integer, intent(in) :: value
        class(base_t), intent(inout) :: self
    end subroutine choose_int

end module owned_array_class_is_explicit_pass
