! Corrected neighbour of typebound_override_2.f90: the overriding procedure
! repeats the INTENT of the overridden binding.
module typebound_override_2_valid_base
    implicit none

    type :: foo
    contains
        procedure, pass(f) :: bar => base_bar
    end type foo

contains

    subroutine base_bar(f, j)
        class(foo), intent(inout) :: f
        integer, intent(in) :: j
    end subroutine base_bar

end module typebound_override_2_valid_base

module typebound_override_2_valid
    use typebound_override_2_valid_base, only: foo
    implicit none

    type, extends(foo) :: extfoo
    contains
        procedure, pass(f) :: bar => ext_bar
    end type extfoo

contains

    subroutine ext_bar(f, j)
        class(extfoo), intent(inout) :: f
        integer, intent(in) :: j
    end subroutine ext_bar

end module typebound_override_2_valid
