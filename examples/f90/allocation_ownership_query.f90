module allocation_ownership_query
    implicit none

    type :: leaf_t
        integer, allocatable :: values(:)
        integer :: plain
        integer, pointer :: link(:)
    end type leaf_t

    type :: holder_t
        integer, allocatable :: owner(:)
        type(leaf_t) :: nested
    end type holder_t
contains

    subroutine acquire(boxes, seeds)
        type(holder_t), intent(inout) :: boxes(:)
        type(leaf_t), intent(in) :: seeds(:)

        allocate (boxes(1)%owner, mold=seeds(1)%values)
        allocate (boxes(1)%nested%values, source=seeds(1)%values)
        boxes(1)%nested%plain = 1
        nullify (boxes(1)%nested%link)
    end subroutine acquire

end module allocation_ownership_query
