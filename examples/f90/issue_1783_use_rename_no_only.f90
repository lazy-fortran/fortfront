! Issue #1783: USE rename without ONLY clause must be preserved
module orig_names
    implicit none
    integer :: value = 42
contains
    function compute() result(res)
        implicit none
        integer :: res
        res = value * 2
    end function compute
end module orig_names

program test_use_rename
    use orig_names, my_value => value, my_compute => compute
    implicit none

    print *, 'Value:', my_value
    print *, 'Compute:', my_compute()
end program test_use_rename
