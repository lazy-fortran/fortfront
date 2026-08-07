module type_bound_call_base_case
    implicit none

    type :: box_t
        real(8) :: scale
    contains
        procedure :: value
    end type box_t

contains

    pure real(8) function value(self, x) result(y)
        class(box_t), intent(in) :: self
        real(8), intent(in) :: x

        y = self%scale*x
    end function value

    pure real(8) function top(model, x) result(y)
        type(box_t), intent(in) :: model
        real(8), intent(in) :: x

        y = model%value(x)
    end function top

end module type_bound_call_base_case

program type_bound_call_base_driver
    use type_bound_call_base_case, only: box_t, top
    implicit none

    type(box_t) :: model

    model%scale = 2.5d0
    if (abs(top(model, 1.2d0) - 3.0d0) > 1.0d-13) error stop 1
end program type_bound_call_base_driver
