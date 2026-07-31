module attribute_conflicts_corrected_mod
    implicit none

    integer, public :: shared_counter = 0
    integer, parameter :: limit = 4
    integer, save :: counter = 0
    integer, protected :: buffer = 0
    real, allocatable :: pool(:)
    real, contiguous, pointer :: view(:) => null()
end module attribute_conflicts_corrected_mod

program attribute_conflicts_corrected
    use attribute_conflicts_corrected_mod, only: limit, counter, buffer, &
        shared_counter
    implicit none

    print *, limit, counter, buffer, shared_counter
end program attribute_conflicts_corrected
