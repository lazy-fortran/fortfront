! Issue #1565: USE association should not redeclare imported variables
module test_module
    implicit none
    integer :: module_var
contains
    subroutine module_sub()
        implicit none
        print *, "Module subroutine"
    end subroutine module_sub
end module test_module

program main
    use test_module
    implicit none

    module_var = 42
    call module_sub()
end program main
