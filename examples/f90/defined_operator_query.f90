module defined_operator_query_fixture
    implicit none

    type :: vector_t
        real(8) :: x, y
    end type vector_t

    type(vector_t), save :: global_vector

    interface operator(.plus.)
        module procedure add_vector, add_vector_scalar
    end interface

    interface operator(.neg.)
        module procedure negate_vector
    end interface

contains

    function add_vector(left, right) result(value)
        type(vector_t), intent(in) :: left, right
        type(vector_t) :: value

        value%x = left%x + right%x
        value%y = left%y + right%y
    end function add_vector

    function add_vector_scalar(left, right) result(value)
        type(vector_t), intent(in) :: left
        real(8), intent(in) :: right
        type(vector_t) :: value

        value%x = left%x + right
        value%y = left%y + right
    end function add_vector_scalar

    function negate_vector(value) result(negated)
        type(vector_t), intent(in) :: value
        type(vector_t) :: negated

        negated%x = -value%x
        negated%y = -value%y
    end function negate_vector

end module defined_operator_query_fixture

program defined_operator_query_driver
    use defined_operator_query_fixture, only: vector_t, global_vector, &
        operator(.plus.), operator(.neg.)
    implicit none

    type(vector_t) :: left, right
    type(vector_t), target :: target_left
    type(vector_t), pointer :: pointer_vector
    type(vector_t) :: result, unary_input

    left = vector_t(1.0d0, 2.0d0)
    right = vector_t(3.0d0, 4.0d0)
    global_vector = vector_t(5.0d0, 6.0d0)
    unary_input = vector_t(7.0d0, 8.0d0)
    target_left = vector_t(9.0d0, 10.0d0)
    pointer_vector => target_left
    result = left .plus. right
    result = global_vector .plus. right
    result = pointer_vector .plus. right
    result = .neg. unary_input
    print *, result%x, result%y
end program defined_operator_query_driver
