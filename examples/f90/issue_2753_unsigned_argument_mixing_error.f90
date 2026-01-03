program issue_2753_unsigned_argument_mixing_error
    implicit none

    integer, unsigned :: u
    integer :: i

    u = uint(1)
    i = 1
    u = wrap_add(u, i)

end program issue_2753_unsigned_argument_mixing_error
