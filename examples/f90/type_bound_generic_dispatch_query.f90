module type_bound_generic_dispatch_query_example
    implicit none

    type :: base_t
        integer :: last_kind = 0
    contains
        procedure, pass(self) :: choose_int
        procedure, pass(self) :: choose_real
        generic :: choose => choose_int, choose_real
    end type base_t

    type, extends(base_t) :: child_t
    end type child_t

    type :: named_pass_t
        integer :: last_value = 0
    contains
        procedure, pass(self) :: choose_named
        generic :: choose => choose_named
    end type named_pass_t

    type(child_t), save :: global_object

contains

    subroutine accepted_integer(object, value)
        type(child_t), intent(inout) :: object
        integer, intent(in) :: value

        call object%choose(value)
    end subroutine accepted_integer

    subroutine accepted_real(object, value)
        type(child_t), intent(inout) :: object
        real(8), intent(in) :: value

        call object%choose(value)
    end subroutine accepted_real

    subroutine accepted_named_pass(object, value)
        type(named_pass_t), intent(inout) :: object
        integer, intent(in) :: value

        call object%choose(value)
    end subroutine accepted_named_pass

    subroutine refused_alias(object, value)
        type(child_t), target, intent(inout) :: object
        integer, intent(in) :: value

        call object%choose(value)
    end subroutine refused_alias

    subroutine refused_global(value)
        integer, intent(in) :: value

        call global_object%choose(value)
    end subroutine refused_global

    subroutine refused_dynamic(object, value)
        class(base_t), allocatable, intent(inout) :: object
        integer, intent(in) :: value

        call object%choose(value)
    end subroutine refused_dynamic

    subroutine choose_int(self, value)
        class(base_t), intent(inout) :: self
        integer, intent(in) :: value

        self%last_kind = value
    end subroutine choose_int

    subroutine choose_real(self, value)
        class(base_t), intent(inout) :: self
        real(8), intent(in) :: value

        self%last_kind = int(value)
    end subroutine choose_real

    subroutine choose_named(value, self)
        integer, intent(in) :: value
        type(named_pass_t), intent(inout) :: self

        self%last_value = value
    end subroutine choose_named

end module type_bound_generic_dispatch_query_example

program type_bound_generic_dispatch_query_runtime
    use type_bound_generic_dispatch_query_example, only: child_t, &
        named_pass_t, accepted_integer, accepted_real, accepted_named_pass
    implicit none
    type(child_t) :: child
    type(named_pass_t) :: named

    call accepted_integer(child, 3)
    call accepted_real(child, 4.0d0)
    call accepted_named_pass(named, 7)
    if (child%last_kind /= 4 .or. named%last_value /= 7) error stop 1
    print *, 'PASS: type-bound generic dispatch runtime'
end program type_bound_generic_dispatch_query_runtime
