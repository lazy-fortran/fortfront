! An overriding type-bound function result must keep the rank and the constant
! character length of the result it overrides (F2018 7.5.7.3).
module typebound_override_1
    implicit none

    type :: base_t
    contains
        procedure, nopass :: a => a_base
        procedure, nopass :: b => b_base
        procedure, nopass :: e => e_base
    end type base_t

    type, extends(base_t) :: derived_t
    contains
        procedure, nopass :: a => a_derived
        procedure, nopass :: b => b_derived
        procedure, nopass :: e => e_derived
    end type derived_t

contains

    function a_base()
        character(len=6) :: a_base
        a_base = 'aaaaaa'
    end function a_base

    function a_derived()
        character(len=7) :: a_derived
        a_derived = 'bbbbbbb'
    end function a_derived

    function b_base()
        integer :: b_base
        b_base = 0
    end function b_base

    function b_derived()
        integer :: b_derived(2)
        b_derived = 0
    end function b_derived

    function e_base(z)
        integer, intent(in) :: z
        character(len=3) :: e_base
        e_base = 'ccc'
    end function e_base

    function e_derived(z)
        integer, intent(in) :: z
        character(len=z) :: e_derived
        e_derived = ''
    end function e_derived

end module typebound_override_1
