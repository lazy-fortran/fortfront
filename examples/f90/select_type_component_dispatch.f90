module select_type_component_dispatch
    implicit none

    type, abstract :: component_base_t
    contains
        procedure(component_run_interface), deferred, pass(self) :: run
    end type component_base_t

    type, abstract, extends(component_base_t) :: component_mid_t
    contains
        procedure, pass(self) :: run => mid_run
    end type component_mid_t

    type, extends(component_mid_t) :: component_leaf_t
    end type component_leaf_t

    type :: generic_leaf_t
    contains
        generic :: choose => choose_int, choose_real
    end type generic_leaf_t

    type, abstract :: dispatch_base_t
    end type dispatch_base_t

    type, extends(dispatch_base_t) :: container_t
        type(component_leaf_t) :: leaf
        type(component_leaf_t) :: leaf_section(6)
        type(component_leaf_t) :: leaf_matrix(2, 2)
        type(generic_leaf_t) :: generic
        type(component_leaf_t), pointer :: pointer_leaf
        type(component_leaf_t), allocatable :: allocatable_leaf
    end type container_t

    class(dispatch_base_t), allocatable, save :: global_object

    abstract interface
        subroutine component_run_interface(self, amount)
            import component_base_t
            class(component_base_t), intent(inout) :: self
            integer, intent(in) :: amount
        end subroutine component_run_interface
    end interface

contains

    subroutine inspect_supported(object, amount)
        class(dispatch_base_t), intent(inout) :: object
        integer, intent(in) :: amount

        select type (typed => object)
            type is (container_t)
            call typed%leaf%run(amount)
        end select
    end subroutine inspect_supported

    subroutine inspect_section(object, amount)
        class(dispatch_base_t), intent(inout) :: object
        integer, intent(in) :: amount

        select type (typed => object)
            type is (container_t)
            call typed%leaf_section(2:4)%run(amount)
        end select
    end subroutine inspect_section

    subroutine inspect_stride(object, amount)
        class(dispatch_base_t), intent(inout) :: object
        integer, intent(in) :: amount

        select type (typed => object)
            type is (container_t)
            call typed%leaf_section(2:4:2)%run(amount)
        end select
    end subroutine inspect_stride

    subroutine inspect_dynamic(object, amount, lower, upper)
        class(dispatch_base_t), intent(inout) :: object
        integer, intent(in) :: amount, lower, upper

        select type (typed => object)
            type is (container_t)
            call typed%leaf_section(lower:upper)%run(amount)
        end select
    end subroutine inspect_dynamic

    subroutine inspect_rank2(object, amount)
        class(dispatch_base_t), intent(inout) :: object
        integer, intent(in) :: amount

        select type (typed => object)
            type is (container_t)
            call typed%leaf_matrix(1:2, 1:2)%run(amount)
        end select
    end subroutine inspect_rank2

    subroutine inspect_generic(object, amount)
        class(dispatch_base_t), intent(inout) :: object
        integer, intent(in) :: amount

        select type (typed => object)
            type is (container_t)
            call typed%generic%choose(amount)
        end select
    end subroutine inspect_generic

    subroutine inspect_pointer(object, amount)
        class(dispatch_base_t), intent(inout) :: object
        integer, intent(in) :: amount

        select type (typed => object)
            type is (container_t)
            call typed%pointer_leaf%run(amount)
        end select
    end subroutine inspect_pointer

    subroutine inspect_allocatable(object, amount)
        class(dispatch_base_t), intent(inout) :: object
        integer, intent(in) :: amount

        select type (typed => object)
            type is (container_t)
            call typed%allocatable_leaf%run(amount)
        end select
    end subroutine inspect_allocatable

    subroutine inspect_global(amount)
        integer, intent(in) :: amount

        select type (typed => global_object)
            type is (container_t)
            call typed%leaf%run(amount)
        end select
    end subroutine inspect_global

    subroutine inspect_alias(object, amount)
        class(dispatch_base_t), intent(inout) :: object
        integer, intent(in) :: amount

        associate (alias => object)
            select type (typed => alias)
                type is (container_t)
                call typed%leaf%run(amount)
            end select
        end associate
    end subroutine inspect_alias

    subroutine inspect_nested(object, amount)
        class(dispatch_base_t), intent(inout) :: object
        integer, intent(in) :: amount

        select type (typed => object)
            type is (container_t)
            if (amount > 0) call typed%leaf%run(amount)
        end select
    end subroutine inspect_nested

    subroutine inspect_missing(object, amount)
        class(dispatch_base_t), intent(inout) :: object
        integer, intent(in) :: amount

        select type (typed => object)
            type is (container_t)
            call typed%leaf%missing(amount)
        end select
    end subroutine inspect_missing

    subroutine mid_run(self, amount)
        class(component_mid_t), intent(inout) :: self
        integer, intent(in) :: amount
    end subroutine mid_run

    subroutine choose_int(self, amount)
        type(generic_leaf_t), intent(inout) :: self
        integer, intent(in) :: amount
    end subroutine choose_int

    subroutine choose_real(self, amount)
        type(generic_leaf_t), intent(inout) :: self
        real, intent(in) :: amount
    end subroutine choose_real

end module select_type_component_dispatch
