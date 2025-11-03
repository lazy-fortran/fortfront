program issue_1959_target_attribute
    implicit none
    integer, target :: x, y
    integer, pointer :: p

    x = 10
    y = 20
    p => x
end program issue_1959_target_attribute
