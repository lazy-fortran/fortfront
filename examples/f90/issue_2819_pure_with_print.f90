program issue_2819_pure_with_print
    ! This example demonstrates INVALID Fortran per F2008 C1283.
    ! A PURE procedure may not execute external I/O. The PRINT statement
    ! inside the PURE function below must be rejected by semantic validation.
    implicit none
    integer :: r

    r = noisy(2)

contains

    pure integer function noisy(a)
        integer, intent(in) :: a
        print *, a               ! INVALID: I/O not allowed in PURE procedure
        noisy = a + 1
    end function noisy

end program issue_2819_pure_with_print
