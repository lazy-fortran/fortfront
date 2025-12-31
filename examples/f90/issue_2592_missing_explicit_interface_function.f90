program issue_2592_missing_explicit_interface_function
    implicit none

    integer :: x
    integer :: y

    x = 1
    y = external_func(x)
    print *, y
end program issue_2592_missing_explicit_interface_function

