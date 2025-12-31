program issue_2592_missing_explicit_interface_subroutine
    implicit none

    integer :: x

    x = 1
    call external_sub(x)
end program issue_2592_missing_explicit_interface_subroutine

