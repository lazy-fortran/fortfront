! Issue #1771: Module-level kind parameters must remain visible to users
module math_utils_kinds
    implicit none
    integer, parameter :: dp = selected_real_kind(15, 307)
contains
    function square(x) result(res)
        implicit none
        real(dp), intent(in) :: x
        real(dp) :: res
        res = x * x
    end function square
end module math_utils_kinds

program test_module_only
    use math_utils_kinds, only: square
    implicit none

    real(selected_real_kind(15, 307)) :: x
    x = 2.5
    print *, 'Square:', square(x)
end program test_module_only
