! Issue #2820: ALLOCATE with MOLD= and SOURCE= must round-trip.
program issue_2820_allocate_mold_source
    implicit none
    integer, allocatable :: a(:), b(:), c(:)

    allocate (a(3))
    a = 1
    allocate (b, mold=a)
    allocate (c, source=a)
    print *, size(b), c
end program issue_2820_allocate_mold_source
