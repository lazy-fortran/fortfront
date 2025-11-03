module test_protected_mod
    implicit none
    integer, protected :: protected_var
contains
    subroutine set_protected(val)
        integer, intent(in) :: val
        protected_var = val
    end subroutine set_protected
end module test_protected_mod
