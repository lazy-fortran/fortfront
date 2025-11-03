! Issue #1619: nested derived types with constructor interface
module test_mod_nested_types
    implicit none

    type :: t_inner
        integer :: value
    end type t_inner

    type :: t_outer
        integer :: id
        type(t_inner) :: inner
    end type t_outer

    interface t_outer
        module procedure new_outer
    end interface t_outer
contains
    function new_outer(id, inner) result(obj)
        implicit none
        integer, intent(in) :: id
        type(t_inner), intent(in) :: inner
        type(t_outer) :: obj

        obj%id = id
        obj%inner = inner
    end function new_outer
end module test_mod_nested_types
