! Regression test for issue #2394: uppercase keywords in BIND(C) interface
PROGRAM UPPERCASE_BIND_C
    USE ISO_C_BINDING
    IMPLICIT NONE

    INTEGER(C_INT) :: R
    R = 0_C_INT
END PROGRAM UPPERCASE_BIND_C
