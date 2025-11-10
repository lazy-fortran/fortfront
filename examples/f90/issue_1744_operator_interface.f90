! Issue #1744: operator and assignment generic interfaces must remain intact
module vector_ops_interface
    implicit none

    type :: vec3
        real :: x, y, z
    end type vec3

    interface operator(+)
        module procedure vec3_add
    end interface
contains
    function vec3_add(a, b) result(res)
        implicit none
        type(vec3), intent(in) :: a, b
        type(vec3) :: res
        res%x = a%x + b%x
        res%y = a%y + b%y
        res%z = a%z + b%z
    end function vec3_add
end module vector_ops_interface

module string_ops_interface
    implicit none

    type :: string_type
        character(len=:), allocatable :: value
    end type string_type

    interface assignment(=)
        module procedure assign_string
    end interface
contains
    subroutine assign_string(lhs, rhs)
        implicit none
        type(string_type), intent(out) :: lhs
        character(len=*), intent(in) :: rhs
        lhs%value = rhs
    end subroutine assign_string
end module string_ops_interface
