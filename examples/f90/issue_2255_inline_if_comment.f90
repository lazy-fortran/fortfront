program issue_2255_inline_if_comment
    implicit none
    logical :: flag

    flag = .true.
    if (flag) &
    ! comment in continuation line
    call say_hi()

contains

    subroutine say_hi()
        print *, 'Hello!'
    end subroutine say_hi

end program issue_2255_inline_if_comment
