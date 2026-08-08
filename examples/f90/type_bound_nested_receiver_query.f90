module type_bound_nested_receiver_query
    implicit none

    type :: inner_t
    contains
        procedure, pass(self) :: apply => inner_apply
        procedure, nopass :: reset => inner_reset
        procedure, pass(self) :: measure => inner_measure
        generic :: ambiguous => first_ambiguous, second_ambiguous
    end type inner_t

    type :: outer_t
        type(inner_t) :: inner
        class(inner_t), allocatable :: polymorphic_inner
    contains
        procedure, pass(self) :: outer_noop
    end type outer_t

    type, extends(inner_t) :: child_inner_t
    contains
        procedure :: measure => child_measure
    end type child_inner_t

contains

    subroutine exercise_static(outer, value)
        type(outer_t), intent(inout) :: outer
        real, intent(inout) :: value

        call outer%inner%apply(value)
        call outer%inner%reset(value)
        call outer%inner%ambiguous(value)
        call outer%inner%missing(value)
        value = outer%polymorphic_inner%measure(value)
    end subroutine exercise_static

    subroutine exercise_polymorphic(outer, value)
        class(outer_t), intent(inout) :: outer
        real, intent(inout) :: value

        value = outer%inner%measure(value)
    end subroutine exercise_polymorphic

    subroutine inner_apply(self, value)
        class(inner_t), intent(inout) :: self
        real, intent(inout) :: value
        value = value + 1.0
    end subroutine inner_apply

    subroutine inner_reset(value)
        real, intent(inout) :: value
        value = 0.0
    end subroutine inner_reset

    function inner_measure(self, value) result(output)
        class(inner_t), intent(in) :: self
        real, intent(in) :: value
        real :: output
        output = value
    end function inner_measure

    function child_measure(self, value) result(output)
        class(child_inner_t), intent(in) :: self
        real, intent(in) :: value
        real :: output
        output = value + 1.0
    end function child_measure

    subroutine first_ambiguous(self, value)
        class(inner_t), intent(inout) :: self
        real, intent(inout) :: value
    end subroutine first_ambiguous

    subroutine second_ambiguous(self, value)
        class(inner_t), intent(inout) :: self
        real, intent(inout) :: value
    end subroutine second_ambiguous

    subroutine outer_noop(self)
        class(outer_t), intent(inout) :: self
    end subroutine outer_noop

end module type_bound_nested_receiver_query
