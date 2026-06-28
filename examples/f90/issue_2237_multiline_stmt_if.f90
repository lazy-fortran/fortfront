program multiline_stmt_if
    implicit none
    integer :: a(2)
    a = [1, 2]
    if ((a(1) == 1) &
        .or. (a(2) == 2)) stop 1
end program multiline_stmt_if
