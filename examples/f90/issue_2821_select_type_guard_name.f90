! Issue #2821: a SELECT TYPE guard type-name (the type spec in "type is (...)")
! must not be harvested as a program variable and emitted as a spurious
! "<type> :: <guard-name>" declaration.
program issue_2821_select_type_guard_name
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
end program issue_2821_select_type_guard_name
