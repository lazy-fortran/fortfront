module ownership_lifetime_facts
    implicit none

    type :: holder_t
        integer, allocatable :: owner(:)
        integer, allocatable :: destination(:)
    end type holder_t
contains

    subroutine lifetime(box, seed)
        type(holder_t), intent(inout) :: box
        integer, allocatable, intent(in) :: seed(:)
        integer, allocatable :: temp(:)

        allocate (box%owner, source=seed)
        deallocate (box%owner)
        allocate (temp(2))
        call move_alloc(temp, box%destination)
        box%destination = seed
    end subroutine lifetime

end module ownership_lifetime_facts
