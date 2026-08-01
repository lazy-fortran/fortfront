! Corrected neighbour of interface_operator_3.f90 (issue #2887).
! Old and new spellings of the same relational operator denote one generic
! spec, so both may be imported from a module that extends either spelling.
module m_eq
    implicit none
    type t3
    integer :: i
end type t3
interface operator(==)
    module procedure my_cmp
end interface
interface operator(.ne.)
    module procedure my_ncmp
end interface
contains
    elemental function my_cmp(a, b) result(c)
        type(t3), intent(in) :: a, b
        logical :: c
        c = a%i == b%i
    end function my_cmp

    elemental function my_ncmp(a, b) result(c)
        type(t3), intent(in) :: a, b
        logical :: c
        c = a%i /= b%i
    end function my_ncmp
end module m_eq

module m8
    use m_eq, only: operator(==), operator(.eq.)
    use m_eq, only: operator(/=), operator(.ne.)
    implicit none
end module m8
