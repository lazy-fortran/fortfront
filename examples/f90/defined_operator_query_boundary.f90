module defined_operator_query_boundary_fixture
    implicit none

    interface operator(.blend.)
        module procedure blend_real, blend_integer
    end interface

contains

    pure real(8) function blend_real(left, right) result(value)
        real(8), intent(in) :: left, right

        value = left*left + 2.0d0*right
    end function blend_real

    pure integer function blend_integer(left, right) result(value)
        integer, intent(in) :: left, right

        value = left + right
    end function blend_integer

    pure real(8) function blend_top(left, right) result(value)
        real(8), intent(in) :: left, right

        value = left .blend. right
    end function blend_top

end module defined_operator_query_boundary_fixture
