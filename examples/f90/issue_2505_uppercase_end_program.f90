! Test case for issue #2505: uppercase END PROGRAM fails to parse
! ISO/IEC 1539-1:2018 Section 3.3.2: Fortran is case-insensitive for keywords
program main
    implicit none
    call sub()
contains
    subroutine sub()
        implicit none
        print *, "Internal procedure works"
    end subroutine sub
END program main
