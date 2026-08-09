module abstract_select_type_function_dispatch
    implicit none

    type, abstract :: root_t
    contains
        procedure(evaluate_interface), deferred, pass(self) :: evaluate
    end type root_t

    type, extends(root_t) :: leaf_t
    contains
        procedure, pass(self) :: evaluate => leaf_evaluate
    end type leaf_t

    class(root_t), allocatable, save :: global_object

    abstract interface
        pure function evaluate_interface(self, x) result(value)
            import root_t
            class(root_t), intent(in) :: self
            real(8), intent(in) :: x
            real(8) :: value
        end function evaluate_interface
    end interface

contains

    pure function evaluate_selected(object, x) result(value)
        class(root_t), intent(in) :: object
        real(8), intent(in) :: x
        real(8) :: value

        select type (object)
            type is (leaf_t)
                value = object%evaluate(x)
            class default
                value = object%evaluate(x)
        end select
    end function evaluate_selected

    function evaluate_global(x) result(value)
        real(8), intent(in) :: x
        real(8) :: value

        select type (global_object)
            type is (leaf_t)
                value = global_object%evaluate(x)
        end select
    end function evaluate_global

    pure function leaf_evaluate(self, x) result(value)
        class(leaf_t), intent(in) :: self
        real(8), intent(in) :: x
        real(8) :: value

        value = 2.0d0*x
    end function leaf_evaluate

end module abstract_select_type_function_dispatch
