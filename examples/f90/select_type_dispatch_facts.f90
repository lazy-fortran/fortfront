module select_type_dispatch_facts
    implicit none

    type, abstract :: base_t
    contains
        procedure(run_interface), deferred, pass(self) :: run
    end type base_t

    type, extends(base_t) :: child_t
    contains
        procedure, pass(self) :: run => child_run
    end type child_t

    type, extends(base_t), abstract :: middle_t
    end type middle_t

    type, extends(base_t) :: bad_pass_t
    contains
        procedure, pass(self) :: run => bad_pass_run
    end type bad_pass_t

    type :: generic_t
    contains
        generic :: choose => choose_left, choose_right
    end type generic_t

    abstract interface
        subroutine run_interface(self, amount)
            import base_t
            class(base_t), intent(inout) :: self
            integer, intent(in) :: amount
        end subroutine run_interface
    end interface

contains

    subroutine dispatch_supported(object, amount)
        class(base_t), intent(inout) :: object
        integer, intent(in) :: amount

        select type (object)
            type is (child_t)
            call object%run(amount)
        class default
        end select
    end subroutine dispatch_supported

    subroutine dispatch_default(object, amount)
        class(base_t), intent(inout) :: object
        integer, intent(in) :: amount

        select type (object)
        class default
            call object%run(amount)
        end select
    end subroutine dispatch_default

    subroutine dispatch_nested(object, amount)
        class(base_t), intent(inout) :: object
        integer, intent(in) :: amount

        select type (object)
            type is (child_t)
            if (amount > 0) call object%run(amount)
        end select
    end subroutine dispatch_nested

    subroutine dispatch_dynamic(object, other, amount)
        class(base_t), intent(inout) :: object
        class(base_t), intent(inout) :: other
        integer, intent(in) :: amount

        select type (object)
            type is (child_t)
            call other%run(amount)
        end select
    end subroutine dispatch_dynamic

    subroutine dispatch_array(object, amount)
        class(base_t), intent(inout) :: object(:)
        integer, intent(in) :: amount

        select type (object)
            type is (child_t)
            call object%run(amount)
        end select
    end subroutine dispatch_array

    subroutine dispatch_pointer(object, amount)
        class(base_t), pointer, intent(inout) :: object
        integer, intent(in) :: amount

        select type (object)
            type is (child_t)
            call object%run(amount)
        end select
    end subroutine dispatch_pointer

    subroutine dispatch_deferred(object, amount)
        class(base_t), intent(inout) :: object
        integer, intent(in) :: amount

        select type (object)
            type is (middle_t)
            call object%run(amount)
        end select
    end subroutine dispatch_deferred

    subroutine dispatch_generic(object)
        class(*), intent(inout) :: object

        select type (object)
            type is (generic_t)
            call object%choose()
        end select
    end subroutine dispatch_generic

    subroutine dispatch_bad_pass(object, amount)
        class(base_t), intent(inout) :: object
        integer, intent(in) :: amount

        select type (object)
            type is (bad_pass_t)
            call object%run(amount)
        end select
    end subroutine dispatch_bad_pass

    subroutine dispatch_unresolved(object)
        class(*), intent(inout) :: object

        select type (object)
            type is (missing_t)
            call object%run()
        end select
    end subroutine dispatch_unresolved

    subroutine child_run(self, amount)
        class(child_t), intent(inout) :: self
        integer, intent(in) :: amount
    end subroutine child_run

    subroutine bad_pass_run(wrong, amount)
        class(bad_pass_t), intent(inout) :: wrong
        integer, intent(in) :: amount
    end subroutine bad_pass_run

    subroutine choose_left()
    end subroutine choose_left

    subroutine choose_right()
    end subroutine choose_right

end module select_type_dispatch_facts
