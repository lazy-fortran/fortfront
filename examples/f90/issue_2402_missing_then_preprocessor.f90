! Minimal reproducer for missing_then error with preprocessor directives
! Based on gfortran test failures: dev_null.F90, write_to_null.F90, etc.
! Issue #2402

#if defined(__linux__)
program test_preprocessor
    implicit none
    print *, "Linux platform"
end program
#endif
