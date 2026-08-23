program continuation_comment_expression
    implicit none
    integer :: value

    value = 1 + &
        ! a comment between continued expression lines
        2
    if (value /= 3) error stop 1
    print *, 'PASS'
end program continuation_comment_expression
