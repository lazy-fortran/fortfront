! A dummy argument of an overriding type-bound procedure may not change to an
! unrelated type. Here the second dummy goes from CLASS(base_type) to INTEGER
! (F2018 7.5.7.3).
module typebound_override_5_base
    implicit none

    type :: base_type
        integer :: kind_id
    contains
        procedure, pass(map) :: clone => base_clone
    end type base_type

contains

    subroutine base_clone(map, mapout, info)
        class(base_type), intent(inout) :: map
        class(base_type), intent(inout) :: mapout
        integer, intent(inout) :: info
    end subroutine base_clone

end module typebound_override_5_base

module typebound_override_5
    use typebound_override_5_base, only: base_type
    implicit none

    type, extends(base_type) :: r_type
        real :: dat
    contains
        procedure, pass(map) :: clone => r_clone
    end type r_type

contains

    subroutine r_clone(map, mapout, info)
        class(r_type), intent(inout) :: map
        integer, intent(inout) :: mapout
        integer, intent(inout) :: info
    end subroutine r_clone

end module typebound_override_5
