module type_bound_call_query
    implicit none

    type, abstract :: base_t
    contains
        procedure(run_interface), deferred, pass(self) :: run
        procedure, nopass :: inherited => base_inherited
        generic :: ambiguous => first_ambiguous, second_ambiguous
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

contains

    subroutine dispatch_from_base(self)
        class(base_t), intent(inout) :: self

        call self%run()
        call self%inherited()
        call self%ambiguous()
        call self%missing()
    end subroutine dispatch_from_base

    subroutine dispatch_from_child(self)
        type(child_t), intent(inout) :: self

        call self%run()
        call self%inherited()
    end subroutine dispatch_from_child

    subroutine base_inherited()
    end subroutine base_inherited

    subroutine child_run(self)
        class(child_t), intent(inout) :: self
    end subroutine child_run

    subroutine first_ambiguous()
    end subroutine first_ambiguous

    subroutine second_ambiguous()
    end subroutine second_ambiguous

end module type_bound_call_query
