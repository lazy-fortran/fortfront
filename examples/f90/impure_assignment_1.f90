! Rejection fixture (gfortran.dg/impure_assignment_1.f90, PR25059):
! a PURE procedure may not invoke an impure defined assignment.
MODULE M1
 TYPE T1
  INTEGER :: I
 END TYPE T1
 INTERFACE ASSIGNMENT(=)
   MODULE PROCEDURE S1
 END INTERFACE
CONTAINS
   SUBROUTINE S1(I,J)
     TYPE(T1), INTENT(OUT):: I
     TYPE(T1), INTENT(IN) :: J
     I%I=J%I**2
   END SUBROUTINE S1
END MODULE M1

USE M1
CONTAINS
PURE SUBROUTINE S2(I,J)
     TYPE(T1), INTENT(OUT):: I
     TYPE(T1), INTENT(IN) :: J
     I=J                      ! { dg-error "is not PURE" }
END SUBROUTINE S2
END
