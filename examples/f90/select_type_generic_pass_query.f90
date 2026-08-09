module select_type_generic_pass_query_example
    implicit none

    type, abstract :: base_t
        integer :: last_value = 0
    contains
        procedure, pass(self) :: choose_int
        generic :: choose => choose_int
    end type base_t

    type, extends(base_t) :: child_t
    end type child_t

contains

    subroutine invoke(object, value)
        class(base_t), intent(inout) :: object
        integer, intent(in) :: value

        select type (object)
            type is (child_t)
            call object%choose(value)
        end select
    end subroutine invoke

    subroutine choose_int(value, self)
        integer, intent(in) :: value
        class(base_t), intent(inout) :: self

        self%last_value = value
    end subroutine choose_int

end module select_type_generic_pass_query_example

program select_type_generic_pass_query_runtime
    use select_type_generic_pass_query_example, only: child_t, invoke
    implicit none
    type(child_t) :: object

    call invoke(object, 9)
    if (object%last_value /= 9) error stop 1
    print *, 'PASS: SELECT TYPE generic PASS runtime'
end program select_type_generic_pass_query_runtime
