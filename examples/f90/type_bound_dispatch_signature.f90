module type_bound_dispatch_signature
    implicit none

    type, abstract :: base_t
    contains
        procedure(run_interface), deferred, pass(self) :: run
        generic :: ambiguous => first_ambiguous, second_ambiguous
        generic :: generic => one_generic
    end type base_t

    type, extends(base_t) :: child_a_t
    contains
        procedure, pass(self) :: run => child_a_run
    end type child_a_t

    type, extends(base_t) :: child_b_t
    contains
        procedure, pass(obj) :: run => child_b_run
    end type child_b_t

    abstract interface
        subroutine run_interface(token, self)
            import base_t
            integer, optional :: token
            class(base_t), intent(inout) :: self
        end subroutine run_interface
    end interface

contains

    subroutine dispatch_from_base(self)
        class(base_t), intent(inout) :: self

        call self%run()
        call self%ambiguous()
        call self%generic()
        call self%unresolved()
    end subroutine dispatch_from_base

    subroutine child_a_run(token, self)
        integer, optional :: token
        class(child_a_t), intent(inout) :: self
    end subroutine child_a_run

    subroutine child_b_run(token, obj)
        integer, optional :: token
        class(child_b_t), intent(inout) :: obj
    end subroutine child_b_run

    subroutine first_ambiguous()
    end subroutine first_ambiguous

    subroutine second_ambiguous()
    end subroutine second_ambiguous

    subroutine one_generic()
    end subroutine one_generic

end module type_bound_dispatch_signature
