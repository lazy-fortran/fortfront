module select_type_associate_facts
    implicit none

    type, abstract :: base_t
    contains
        procedure(run_iface), deferred :: run
    end type base_t

    type, extends(base_t) :: child_t
    contains
        procedure :: run => child_run
    end type child_t

    abstract interface
        subroutine run_iface(self)
            import base_t
            class(base_t), intent(inout) :: self
        end subroutine run_iface
    end interface

contains

    subroutine inspect_alias(box)
        class(base_t), intent(inout) :: box

        select type (typed => box)
        type is (child_t)
            call typed%run()
        class default
        end select
    end subroutine inspect_alias

    subroutine inspect_direct(box)
        class(base_t), intent(inout) :: box

        select type (box)
        type is (child_t)
            call box%run()
        class default
        end select
    end subroutine inspect_direct

    subroutine child_run(self)
        class(child_t), intent(inout) :: self
    end subroutine child_run

end module select_type_associate_facts
