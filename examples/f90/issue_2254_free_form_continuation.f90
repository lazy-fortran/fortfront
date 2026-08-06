program issue_2254_free_form_continuation
    implicit none
    integer :: x

    x = 1 + &
        2

    write (*, "(a,',',i0,',', &
        es12.4)") "value", x, real(x)
    print *, x
end program issue_2254_free_form_continuation
