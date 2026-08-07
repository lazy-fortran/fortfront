module binding_hierarchy_query
    implicit none

    type, abstract :: base_t
    contains
        procedure(run_interface), deferred, pass(self) :: run
        procedure, nopass :: inherited => base_inherited
    end type base_t

    type, abstract, extends(base_t) :: intermediate_t
    contains
        procedure, nopass :: marker => intermediate_marker
    end type intermediate_t

    type, extends(intermediate_t) :: concrete_t
    contains
        procedure, pass(self) :: run => concrete_run
    end type concrete_t

    type :: ambiguous_t
    contains
        generic :: ambiguous => first_ambiguous, second_ambiguous
    end type ambiguous_t

    abstract interface
        subroutine run_interface(self)
            import base_t
            class(base_t), intent(inout) :: self
        end subroutine run_interface
    end interface

contains

    subroutine base_inherited()
    end subroutine base_inherited

    subroutine intermediate_marker()
    end subroutine intermediate_marker

    subroutine concrete_run(self)
        class(concrete_t), intent(inout) :: self
    end subroutine concrete_run

    subroutine first_ambiguous()
    end subroutine first_ambiguous

    subroutine second_ambiguous()
    end subroutine second_ambiguous

end module binding_hierarchy_query
