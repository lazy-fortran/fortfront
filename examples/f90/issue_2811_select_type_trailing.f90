! Issue #2811: a statement after end select must be a sibling of the
! select type construct, not absorbed into the last guard arm body.
program issue_2811_select_type_trailing
    implicit none
    class(*), allocatable :: x
    integer :: i

    i = 0
    allocate (integer :: x)
    select type (x)
        type is (integer)
        i = 1
    class default
        i = 2
    end select
    print *, i
end program issue_2811_select_type_trailing
