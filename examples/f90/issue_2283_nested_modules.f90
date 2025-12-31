! Issue #2283: Module with internal procedures accessing module data
! Note: Nested module syntax is NOT valid Fortran. This example demonstrates
! the valid pattern of using internal (contained) procedures within a module.
module issue_2283_outer
    implicit none
    integer :: module_value
contains
    subroutine set_module_value(value)
        integer, intent(in) :: value
        module_value = value
    end subroutine set_module_value

    subroutine touch_value()
        call set_module_value(1)
    end subroutine touch_value
end module issue_2283_outer
