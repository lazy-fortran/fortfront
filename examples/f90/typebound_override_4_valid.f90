! Corrected neighbour of typebound_override_4.f90: only the passed-object
! dummy narrows to the extending type; the other dummy keeps its declared type.
module typebound_override_4_valid_base
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

end module typebound_override_4_valid_base

module typebound_override_4_valid
    use typebound_override_4_valid_base, only: base_type
    implicit none

    type, extends(base_type) :: r_type
    contains
        procedure, pass(map) :: clone => r_clone
    end type r_type

contains

    subroutine r_clone(map, mapout)
        class(r_type), intent(inout) :: map
        class(base_type), intent(inout) :: mapout
    end subroutine r_clone

end module typebound_override_4_valid
