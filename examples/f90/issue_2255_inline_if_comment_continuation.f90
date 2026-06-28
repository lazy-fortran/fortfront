program test_inline_if_comment_continuation
    implicit none
    logical :: flag

    flag = .true.

    ! Standard-conforming inline IF with comment continuation
    if (flag) &
    ! comment in continuation line
    call say_hi()

contains

    subroutine say_hi()
        print *, "Hello!"
    end subroutine say_hi

end program test_inline_if_comment_continuation
