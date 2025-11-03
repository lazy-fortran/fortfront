! Issue #1774: user-defined operators must remain intact after transformation
module operator_mod
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none
    type :: vector
        real(dp) :: x, y
    end type vector

    interface operator(.dot.)
        module procedure dot_product_vec
    end interface
contains
    real(dp) function dot_product_vec(a, b)
        type(vector), intent(in) :: a, b
        dot_product_vec = a%x * b%x + a%y * b%y
    end function dot_product_vec
end module operator_mod

program test_operator_defined
    use operator_mod
    implicit none

    type(vector) :: v1, v2
    real(dp) :: result

    v1 = vector(1.0_dp, 2.0_dp)
    v2 = vector(3.0_dp, 4.0_dp)
    result = v1 .dot. v2
    print *, 'Dot product:', result
end program test_operator_defined
