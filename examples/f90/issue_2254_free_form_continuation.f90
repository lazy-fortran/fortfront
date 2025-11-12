program issue_2254_free_form_continuation
    implicit none
    integer :: x

    x = 1 + &
        2

    print *, x
end program issue_2254_free_form_continuation

