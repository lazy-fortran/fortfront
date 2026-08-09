module defined_operator_ambiguous_refusal_mod
    implicit none

    type :: value_t
        integer :: value
    end type value_t

    interface operator(.amb.)
        module procedure choose_left, choose_right
    end interface

contains

    function choose_left(left, right) result(value)
        type(value_t), intent(in) :: left, right
        type(value_t) :: value

        value%value = left%value + right%value
    end function choose_left

    function choose_right(left, right) result(value)
        type(value_t), intent(in) :: left, right
        type(value_t) :: value

        value%value = left%value - right%value
    end function choose_right

end module defined_operator_ambiguous_refusal_mod

program defined_operator_ambiguous_refusal
    use defined_operator_ambiguous_refusal_mod, only: value_t, operator(.amb.)
    implicit none

    type(value_t) :: left, right, value

    left%value = 1
    right%value = 2
    value = left .amb. right
    print *, value%value
end program defined_operator_ambiguous_refusal
