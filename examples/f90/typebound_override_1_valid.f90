! Corrected neighbour of typebound_override_1.f90: the overriding results keep
! the rank and the constant character length, and a nonconstant character
! length on the overridden result stays accepted.
module typebound_override_1_valid
    implicit none

    type :: base_t
    contains
        procedure, nopass :: a => a_base
        procedure, nopass :: b => b_base
        procedure, nopass :: d => d_base
    end type base_t

    type, extends(base_t) :: derived_t
    contains
        procedure, nopass :: a => a_derived
        procedure, nopass :: b => b_derived
        procedure, nopass :: d => d_derived
    end type derived_t

contains

    function a_base()
        character(len=6) :: a_base
        a_base = 'aaaaaa'
    end function a_base

    function a_derived()
        character(len=6) :: a_derived
        a_derived = 'bbbbbb'
    end function a_derived

    function b_base()
        integer :: b_base(2)
        b_base = 0
    end function b_base

    function b_derived()
        integer :: b_derived(2)
        b_derived = 1
    end function b_derived

    function d_base(y)
        integer, intent(in) :: y
        character(len=2*y + 1) :: d_base
        d_base = ''
    end function d_base

    function d_derived(y)
        integer, intent(in) :: y
        character(len=1 + y*2) :: d_derived
        d_derived = ''
    end function d_derived

end module typebound_override_1_valid
