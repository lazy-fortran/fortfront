program uppercase_statement_if
    implicit none
    integer :: value
    value = 0
    if (value == 0) STOP 99
    if (value == 0) CALL emit()
    if (value == 0) WRITE (*,'(A)') 'diag'
contains
    subroutine emit()
        stop 1
    end subroutine emit
end program uppercase_statement_if
