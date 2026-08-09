module select_type_real8_expression_query_example
    implicit none

    type :: real8_box_t
    contains
        procedure, pass(self) :: apply_real
        generic :: apply => apply_real
    end type real8_box_t

contains

    real(8) function apply_real(self, x) result(value)
        class(real8_box_t), intent(inout) :: self
        real(8), intent(in) :: x

        value = x + 1
    end function apply_real

    subroutine invoke(object, x, value)
        class(real8_box_t), intent(inout) :: object
        real(8), intent(in) :: x
        real(8), intent(out) :: value

        select type (object)
            type is (real8_box_t)
            value = object%apply(x + 1)
        end select
    end subroutine invoke

end module select_type_real8_expression_query_example

program select_type_real8_expression_query_runtime
    use select_type_real8_expression_query_example, only: real8_box_t, invoke
    implicit none

    type(real8_box_t) :: object
    real(8) :: value

    call invoke(object, 2.5_8, value)
    if (abs(value - 4.5_8) > 1.0e-12_8) error stop 1
    print *, 'PASS: SELECT TYPE REAL(8) expression runtime'
end program select_type_real8_expression_query_runtime
