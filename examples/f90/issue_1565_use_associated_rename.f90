! Issue #1565: USE rename should not introduce duplicate declarations
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
    use test_module, only: module_value => module_var, module_call => module_sub
    implicit none

    module_value = 42
    call module_call()
end program main
