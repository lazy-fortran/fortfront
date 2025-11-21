! Regression test for issue #2394: BIND(C) interface (gfortran test case)
! From gcc/testsuite/gfortran.dg/bind_c_18.f90
!
! PR fortran/37201
!
implicit none
INTERFACE
    FUNCTION my() BIND(C, name="my") RESULT(r)
        USE iso_c_binding
        CHARACTER(kind=C_CHAR) :: r(10)
    END FUNCTION
END INTERFACE
INTERFACE
    FUNCTION two() BIND(C, name="two") RESULT(r)
        USE iso_c_binding
        CHARACTER(kind=C_CHAR, len=2) :: r
    END FUNCTION
END INTERFACE
END
