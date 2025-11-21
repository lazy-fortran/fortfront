! Regression test for issue #2394: C_F_POINTER interface (gfortran test case)
! From gcc/testsuite/gfortran.dg/c_f_pointer_tests_2.f03
!
! PR fortran/32800
!
FUNCTION C_F_STRING(CPTR) RESULT(FPTR)
    USE ISO_C_BINDING
    implicit none
    TYPE(C_PTR), INTENT(IN) :: CPTR
    CHARACTER(KIND=C_CHAR), DIMENSION(:), POINTER :: FPTR
    INTERFACE
        FUNCTION strlen(string) RESULT(len) BIND(C, NAME="strlen")
            import
            TYPE(C_PTR), VALUE :: string
            integer(c_int) :: len
        END FUNCTION strlen
    END INTERFACE
    CALL C_F_POINTER(FPTR=FPTR, CPTR=CPTR, SHAPE=[strlen(cptr)])
END FUNCTION C_F_STRING
