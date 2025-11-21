! Regression test for issue #2394: uppercase FUNCTION in interface block
! Previously caused infinite loop due to case-sensitive text comparison
!
! Minimal reproducer:
implicit none
interface
    FUNCTION s_to_c(string)
        CHARACTER(LEN=*), INTENT(IN) :: string
        CHARACTER(LEN=10) :: s_to_c
    END FUNCTION s_to_c
end interface
end
