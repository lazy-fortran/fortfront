! Corrected neighbours for the statement-placement rules: SEQUENCE appears in
! a derived-type definition, ELSE in an IF construct, and the program unit
! openers stay at file level.
program placement_sections_corrected
    implicit none

    type :: point_t
        sequence
        real :: x
        real :: y
    end type point_t

    type(point_t) :: p

    p = point_t(1.0, 2.0)
    if (p%x > 0.0) then
        print *, 'right'
    else
        print *, 'left'
    end if
end program placement_sections_corrected
