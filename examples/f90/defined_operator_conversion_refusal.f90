module defined_operator_conversion_refusal_mod
    implicit none

    interface operator(.conv.)
        module procedure convert_real
    end interface

contains

    function convert_real(left, right) result(value)
        real(8), intent(in) :: left, right
        real(8) :: value

        value = left + right
    end function convert_real

end module defined_operator_conversion_refusal_mod

program defined_operator_conversion_refusal
    use defined_operator_conversion_refusal_mod, only: operator(.conv.)
    implicit none

    integer :: integer_value
    real(8) :: real_value, value

    integer_value = 1
    real_value = 2.0d0
    value = integer_value .conv. real_value
    print *, value
end program defined_operator_conversion_refusal
