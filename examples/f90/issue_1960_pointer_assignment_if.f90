program issue_1960_pointer_assignment_if
    implicit none
    integer, pointer :: p
    integer, target :: values(2)

    if (.not. associated(p)) then
        p => values(1)
        print *, 'p is now null'
    end if
end program issue_1960_pointer_assignment_if
