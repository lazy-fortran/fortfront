! Issue #1783: USE rename with ONLY clause must be preserved
module orig_names_only
    implicit none
    integer :: value = 42
end module orig_names_only

program test_use_rename_only
    use orig_names_only, only: my_value => value
    implicit none

    print *, my_value
end program test_use_rename_only
