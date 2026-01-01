module issue_2644_scope_lookup_mod
    implicit none
contains
    integer function f(x)
        integer, intent(in) :: x
        f = x + 1
    end function f
end module issue_2644_scope_lookup_mod

program issue_2644_strict_arg_checker_scope_lookup
    use issue_2644_scope_lookup_mod, only: f_mod => f
    implicit none

    integer :: value

    value = g(.true.)
    if (value /= 1) error stop 1

contains

    integer function f(flag)
        logical, intent(in) :: flag

        if (flag) then
            f = 1
        else
            f = 0
        end if
    end function f

    integer function g(flag)
        logical, intent(in) :: flag

        g = f(flag)
    end function g

end program issue_2644_strict_arg_checker_scope_lookup

