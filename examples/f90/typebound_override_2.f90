! An overriding type-bound procedure must give every dummy argument other than
! the passed-object dummy the same INTENT as the binding it overrides
! (F2018 7.5.7.3).
module typebound_override_2_base
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

end module typebound_override_2_base

module typebound_override_2
    use typebound_override_2_base, only: foo
    implicit none

    type, extends(foo) :: extfoo
    contains
        procedure, pass(f) :: bar => ext_bar
    end type extfoo

contains

    subroutine ext_bar(f, j)
        class(extfoo), intent(inout) :: f
        integer, intent(inout) :: j
    end subroutine ext_bar

end module typebound_override_2
