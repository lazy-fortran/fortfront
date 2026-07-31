! Every dummy argument of an overriding type-bound procedure other than the
! passed-object dummy must have the same declared type as its counterpart in
! the overridden binding (F2018 7.5.7.3).
module typebound_override_4_base
    implicit none

    type :: base_type
    contains
        procedure, pass(map) :: clone => base_clone
    end type base_type

contains

    subroutine base_clone(map, mapout)
        class(base_type), intent(inout) :: map
        class(base_type), intent(inout) :: mapout
    end subroutine base_clone

end module typebound_override_4_base

module typebound_override_4
    use typebound_override_4_base, only: base_type
    implicit none

    type, extends(base_type) :: r_type
    contains
        procedure, pass(map) :: clone => r_clone
    end type r_type

contains

    subroutine r_clone(map, mapout)
        class(r_type), intent(inout) :: map
        class(r_type), intent(inout) :: mapout
    end subroutine r_clone

end module typebound_override_4
