! Legacy EQUIVALENCE / COMMON statements should be preserved
program legacy_statements
    implicit none
    integer :: i
    real :: r
    equivalence (i, r)
    common /blk/ i, r
    i = 1
end program legacy_statements
