! Negative fixture for issue #2887 (reject-use-01).
! A module that extends only operator(==) exports the generic spec under both
! spellings == and .eq., but neither /= nor .ne. is exported by it.
! Derived from gfortran.dg/interface_operator_3.f90 (module m8).
module m_eq
    implicit none
    private :: t3
    type t3
    integer :: i
end type t3
interface operator(==)
    module procedure my_cmp
end interface
contains
    elemental function my_cmp(a, b) result(c)
        type(t3), intent(in) :: a, b
        logical :: c
        c = a%i == b%i
    end function my_cmp
end module m_eq

module m8
    use m_eq, only: operator(==), operator(.eq.)
    use m_eq, only: operator(/=)
    use m_eq, only: operator(.ne.)
    implicit none
end module m8
